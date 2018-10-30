-- | Functions to implement 'Api' __servers__.
module Control.Eff.Concurrent.Api.Server
  (
  -- * Api Server
    serve
  , spawnServer
  -- * Api Callbacks
  , ApiHandler(..)
  , unhandledCallError
  , unhandledCastError
  , defaultTermination
  -- * Callback Composition
  , Servable(..)
  , ServerCallback(..)
  , requestHandlerSelector
  , terminationHandler
  )
where

import           Control.Eff
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Internal
import           Control.Eff.Concurrent.Process
import           Control.Lens
import           Data.Proxy
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                )
import           Data.Dynamic
import           Control.Applicative
import           Data.Kind
import           GHC.Stack


-- | A record of callbacks, handling requests sent to a /server/ 'Process', all
-- belonging to a specific 'Api' family instance.
-- The values of this type can be 'serve'ed or combined via 'Servable' or
-- 'ServerCallback's.
data ApiHandler api eff where
  ApiHandler ::
     { -- | A cast will not return a result directly. This is used for async
       -- methods.
       _handleCast
         :: HasCallStack
         => Api api 'Asynchronous -> Eff eff ()
      -- | A call is a blocking operation, the caller is blocked until this
      -- handler calls the reply continuation.
     , _handleCall
         :: forall reply . HasCallStack
         => Api api ('Synchronous reply) -> (reply -> Eff eff ()) -> Eff eff ()
     -- | This callback is called with @Nothing@ if the process exits
     -- peacefully, or @Just "error message..."@ if the process exits with an
     -- error. This function is responsible to exit the process if necessary.
     -- The default behavior is defined in 'defaultTermination'.
     , _handleTerminate
         :: HasCallStack
         => Maybe String -> Eff eff ()
     } -> ApiHandler api eff


-- | Building block for composition of 'ApiHandler'.
-- A wrapper for 'ApiHandler'. Use this to combine 'ApiHandler', allowing a
-- process to implement several 'Api' instances. The termination will be evenly
-- propagated.
-- Create this via e.g. 'Servable' instances
-- To serve multiple apis use '<>' to combine server callbacks, e.g.
--
-- @@@
-- let f = apiHandlerServerCallback px $ ApiHandler ...
--     g = apiHandlerServerCallback px $ ApiHandler ...
--     h = f <> g
-- in serve px h
-- @@@
--
data ServerCallback eff =
  ServerCallback { _requestHandlerSelector :: MessageSelector (Eff eff ())
                 , _terminationHandler :: Maybe String -> Eff eff ()
                 }

makeLenses ''ServerCallback

instance Semigroup (ServerCallback eff) where
  l <> r = l & requestHandlerSelector .~
                  MessageSelector (\x ->
                    runMessageSelector (view requestHandlerSelector l) x <|>
                    runMessageSelector (view requestHandlerSelector r) x)
             & terminationHandler .~
                  (\errorMessage ->
                      do (l^.terminationHandler) errorMessage
                         (r^.terminationHandler) errorMessage)

instance Monoid (ServerCallback eff) where
  mappend = (<>)
  mempty = ServerCallback
              { _requestHandlerSelector = MessageSelector (const Nothing)
              , _terminationHandler = const (return ())
              }

-- | Helper type class to allow composition of 'ApiHandler'.
class Servable a where
  -- | The effect of the callbacks
  type ServerEff a :: [Type -> Type]
  -- | The is used to let the spawn function return multiple 'Server' 'ProcessId's
  -- in a type safe way, e.g. for a tuple instance of this class
  -- @(Server a, Server b)@
  type ServerPids a
  -- | The is used to let the spawn function return multiple 'Server' 'ProcessId's
  -- in a type safe way.
  toServerPids :: proxy a -> ProcessId -> ServerPids a
  -- | Convert the value to a 'ServerCallback'
  toServerCallback :: (SetMember Process (Process effScheduler) (ServerEff a))
    => SchedulerProxy effScheduler -> a -> ServerCallback (ServerEff a)

instance Servable (ServerCallback eff)  where
  type ServerEff (ServerCallback eff) = eff
  type ServerPids (ServerCallback eff) = ProcessId
  toServerCallback  = const id
  toServerPids = const id

instance Typeable a => Servable (ApiHandler a eff)  where
  type ServerEff (ApiHandler a eff) = eff
  type ServerPids (ApiHandler a eff) = Server a
  toServerCallback  = apiHandlerServerCallback
  toServerPids _ = asServer

instance (ServerEff a ~ ServerEff b, Servable a, Servable b) => Servable (a, b) where
  type ServerPids (a, b) = (ServerPids a, ServerPids b)
  type ServerEff (a, b) = ServerEff a
  toServerCallback px (a, b) = toServerCallback px a <> toServerCallback px b
  toServerPids _ pid =
    ( toServerPids (Proxy :: Proxy a) pid
    , toServerPids (Proxy :: Proxy b) pid
    )

-- | Receive and process incoming requests until the process exits.
serve
  :: forall a effScheduler
   . ( Servable a
     , SetMember Process (Process effScheduler) (ServerEff a)
     , HasCallStack
     )
  => SchedulerProxy effScheduler
  -> a
  -> Eff (ServerEff a) ()
serve px a =
  let serverCb = toServerCallback px a
  in  receiveLoopSuchThat px (serverCb ^. requestHandlerSelector) $ \case
        Left  reason   -> (serverCb ^. terminationHandler) reason
        Right handleIt -> handleIt

-- | Spawn a new process, that will receive and process incoming requests
-- until the process exits. Also handle all internal effects.
spawnServer
  :: forall a effScheduler eff
   . ( Servable a
     , SetMember Process (Process effScheduler) (ServerEff a)
     , SetMember Process (Process effScheduler) eff
     , HasCallStack
     )
  => SchedulerProxy effScheduler
  -> a
  -> (  Eff (ServerEff a) ()
     -> Eff (Process effScheduler ': effScheduler) ()
     )
  -> Eff eff (ServerPids a)
spawnServer px a handleEff = do
  pid <- spawn (handleEff (serve px a))
  return (toServerPids (Proxy @a) pid)

-- | Wrap an 'ApiHandler' into a composable 'ServerCallback' value.
apiHandlerServerCallback
  :: forall eff effScheduler api
   . ( HasCallStack
     , Typeable api
     , SetMember Process (Process effScheduler) eff
     )
  => SchedulerProxy effScheduler
  -> ApiHandler api eff
  -> ServerCallback eff
apiHandlerServerCallback px handlers = mempty
  { _requestHandlerSelector = selectHandlerMethod px handlers
  , _terminationHandler     = _handleTerminate handlers
  }

-- | Try to parse an incoming message to an API request, and apply either
-- the 'handleCall' method or the 'handleCast' method to it.
selectHandlerMethod
  :: forall eff effScheduler api
   . ( HasCallStack
     , Typeable api
     , SetMember Process (Process effScheduler) eff
     )
  => SchedulerProxy effScheduler
  -> ApiHandler api eff
  -> MessageSelector (Eff eff ())
selectHandlerMethod px handlers =
  MessageSelector (fmap (applyHandlerMethod px handlers) . fromDynamic)

-- | Apply either the '_handleCall', '_handleCast' or the '_handleTerminate'
-- callback to an incoming request.
applyHandlerMethod
  :: forall eff effScheduler api
   . ( Typeable api
     , SetMember Process (Process effScheduler) eff
     , HasCallStack
     )
  => SchedulerProxy effScheduler
  -> ApiHandler api eff
  -> Request api
  -> Eff eff ()
applyHandlerMethod _px handlers (Terminate e) = _handleTerminate handlers e
applyHandlerMethod _ handlers (Cast request) = _handleCast handlers request
applyHandlerMethod px handlers (Call fromPid request) = _handleCall handlers
                                                                    request
                                                                    sendReply
 where
  sendReply :: Typeable reply => reply -> Eff eff ()
  sendReply reply =
    sendMessage px fromPid (toDyn $! (Response (Proxy @api) $! reply))

-- | A default handler to use in '_handleCall' in 'ApiHandler'. It will call
-- 'raiseError' with a nice error message.
unhandledCallError
  :: forall p x r q
   . ( Show (Api p ( 'Synchronous x))
     , Typeable p
     , HasCallStack
     , SetMember Process (Process q) r
     )
  => SchedulerProxy q
  -> Api p ( 'Synchronous x)
  -> (x -> Eff r ())
  -> Eff r ()
unhandledCallError px api _ = withFrozenCallStack $ raiseError
  px
  ("Unhandled call: (" ++ show api ++ " :: " ++ show (typeRep (Proxy @p)) ++ ")"
  )

-- | A default handler to use in '_handleCast' in 'ApiHandler'. It will call
-- 'raiseError' with a nice error message.
unhandledCastError
  :: forall p r q
   . ( Show (Api p 'Asynchronous)
     , Typeable p
     , HasCallStack
     , SetMember Process (Process q) r
     )
  => SchedulerProxy q
  -> Api p 'Asynchronous
  -> Eff r ()
unhandledCastError px api = withFrozenCallStack $ raiseError
  px
  ("Unhandled cast: (" ++ show api ++ " :: " ++ show (typeRep (Proxy @p)) ++ ")"
  )

-- | Exit the process either normally of the error message is @Nothing@
-- or with 'exitWithError' otherwise.
defaultTermination
  :: forall q r
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Maybe String
  -> Eff r ()
defaultTermination px =
  withFrozenCallStack $ maybe (exitNormally px) (exitWithError px)
