-- | Functions to implement 'Api' __servers__.
module Control.Eff.Concurrent.Api.Server
  (
  -- * Api Server
    serve
  , spawnServer
  , spawnServerWithEffects
  -- * Api Callbacks
  , ApiHandler(..)
  , castCallback
  , callCallback
  , terminateCallback
  , apiHandler
  , apiHandlerForever
  , castHandler
  , castHandlerForever
  , callHandler
  , callHandlerForever
  , castAndCallHandler
  , castAndCallHandlerForever
  , ApiServerCmd(..)
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
import           Control.Eff.Exception
import           Control.Eff.Log
import           Control.Lens
import           Data.Proxy
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                )
import           Data.Dynamic
import           Control.Applicative
import           Data.Kind
import           GHC.Stack
import           Data.Maybe
import           GHC.Generics
import           Control.DeepSeq
import           Data.Default

-- | A record of callbacks, handling requests sent to a /server/ 'Process', all
-- belonging to a specific 'Api' family instance.
-- The values of this type can be 'serve'ed or combined via 'Servable' or
-- 'ServerCallback's.
data ApiHandler api eff where
  ApiHandler ::
     { -- | A cast will not return a result directly. This is used for async
       -- methods. This returns an 'ApiServerCmd' to the server loop.
       _castCallback
         :: Maybe (Api api 'Asynchronous -> Eff eff ApiServerCmd)
      -- | A call is a blocking operation, the caller is blocked until this
      -- handler calls the reply continuation.
      -- This returns an 'ApiServerCmd' to the server loop.
     , _callCallback
         :: forall reply . Maybe (Api api ('Synchronous reply) -> (reply -> Eff eff ()) -> Eff eff ApiServerCmd)
     -- | This callback is called with @Nothing@ if one of these things happen:
     --
     --  * the process exits
     --  * '_callCallback' or '_castCallback' return 'StopApiServer'
     --
     -- If the process exist peacefully the parameter is 'Nothing',
     -- otherwise @Just "error message..."@ if the process exits with an
     -- error.
     --
     -- The default behavior is defined in 'defaultTermination'.
     , _terminateCallback
         :: Maybe (ExitReason 'Recoverable -> Eff eff ())
     } -> ApiHandler api eff


instance Default (ApiHandler api eff) where
  def = ApiHandler { _castCallback = def
                   , _callCallback = def
                   , _terminateCallback = def
                   }

-- | Create an 'ApiHandler' with a '_castCallback', a '_callCallback'  and
--  a '_terminateCallback' implementation.
apiHandler
  :: (Api api 'Asynchronous -> Eff e ApiServerCmd)
  -> (  forall r
      . Api api ( 'Synchronous r)
     -> (r -> Eff e ())
     -> Eff e ApiServerCmd
     )
  -> (ExitReason 'Recoverable -> Eff e ())
  -> ApiHandler api e
apiHandler c d e = ApiHandler
  { _castCallback      = Just c
  , _callCallback      = Just d
  , _terminateCallback = Just e
  }

-- | Like 'apiHandler' but the server will loop until an error is raised or
-- the process exits.
-- The callback actions won't decide wether to stop the
-- server or not, instead the 'ApiServerCmd' 'HandleNextRequest' is used.
apiHandlerForever
  :: (Api api 'Asynchronous -> Eff e ())
  -> (forall r . Api api ( 'Synchronous r) -> (r -> Eff e ()) -> Eff e ())
  -> (ExitReason 'Recoverable -> Eff e ())
  -> ApiHandler api e
apiHandlerForever c d = apiHandler
  (\someCast -> c someCast >> return HandleNextRequest)
  (\someCall k -> d someCall k >> return HandleNextRequest)

-- | Create an 'ApiHandler' with only a '_castCallback' implementation.
castHandler
  :: (Api api 'Asynchronous -> Eff eff ApiServerCmd) -> ApiHandler api eff
castHandler c = def { _castCallback = Just c }

-- | Like 'castHandler' but the server will loop until an error is raised or
-- the process exits. See 'apiHandlerForver'.
castHandlerForever
  :: (Api api 'Asynchronous -> Eff eff ()) -> ApiHandler api eff
castHandlerForever c =
  castHandler (\someCast -> c someCast >> return HandleNextRequest)

-- | Create an 'ApiHandler' with only a '_callCallback' implementation.
callHandler
  :: (  forall r
      . Api api ( 'Synchronous r)
     -> (r -> Eff e ())
     -> Eff e ApiServerCmd
     )
  -> ApiHandler api e
callHandler c = def { _callCallback = Just c }

-- | Like 'callHandler' but the server will loop until an error is raised or
-- the process exits. See 'apiHandlerForver'.
callHandlerForever
  :: (forall r . Api api ( 'Synchronous r) -> (r -> Eff e ()) -> Eff e ())
  -> ApiHandler api e
callHandlerForever d =
  callHandler (\someCall k -> d someCall k >> return HandleNextRequest)

-- | Create an 'ApiHandler' with only a '_castCallback' and '_callCallback' implementation.
castAndCallHandler
  :: (Api api 'Asynchronous -> Eff e ApiServerCmd)
  -> (  forall r
      . Api api ( 'Synchronous r)
     -> (r -> Eff e ())
     -> Eff e ApiServerCmd
     )
  -> ApiHandler api e
castAndCallHandler c d = def { _castCallback = Just c, _callCallback = Just d }

-- | Like 'castAndCallHandler' but the server will loop until an error is raised or
-- the process exits. See 'apiHandlerForver'.
castAndCallHandlerForever
  :: (Api api 'Asynchronous -> Eff e ())
  -> (forall r . Api api ( 'Synchronous r) -> (r -> Eff e ()) -> Eff e ())
  -> ApiHandler api e
castAndCallHandlerForever c d = castAndCallHandler
  (\someCast -> c someCast >> return HandleNextRequest)
  (\someCall k -> d someCall k >> return HandleNextRequest)

-- | A command to the server loop started e.g. by 'server' or 'spawnServerWithEffects'.
-- Typically returned by an 'ApiHandler' member to indicate if the server
-- should continue or stop.
data ApiServerCmd where
  -- | Tell the server to keep the server loop running
  HandleNextRequest  :: ApiServerCmd
  -- | Tell the server to exit, this will make 'serve' stop handling requests without
  -- exitting the process. '_terminateCallback' will be invoked with the given
  -- optional reason.
  StopApiServer :: ExitReason 'Recoverable -> ApiServerCmd
  --  SendReply :: reply -> ApiServerCmd () -> ApiServerCmd (reply -> Eff eff ())
  deriving (Show, Typeable, Generic)

instance NFData ApiServerCmd

makeLenses ''ApiHandler

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
  ServerCallback { _requestHandlerSelector :: MessageSelector (Eff eff ApiServerCmd)
                 , _terminationHandler :: ExitReason 'Recoverable -> Eff eff ()
                 }

makeLenses ''ServerCallback

instance Semigroup (ServerCallback eff) where
  l <> r = l & requestHandlerSelector .~
                  selectDynamicMessageLazy (\x ->
                    runMessageSelector (view requestHandlerSelector l) x <|>
                    runMessageSelector (view requestHandlerSelector r) x)
             & terminationHandler .~
                  (\reason ->
                      do (l^.terminationHandler) reason
                         (r^.terminationHandler) reason)

instance Monoid (ServerCallback eff) where
  mappend = (<>)
  mempty = ServerCallback
              { _requestHandlerSelector = selectDynamicMessageLazy (const Nothing)
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
  toServerCallback
    :: (Member Interrupts (ServerEff a), SetMember Process (Process effScheduler) (ServerEff a))
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
     , Member Interrupts (ServerEff a)
     , HasCallStack
     )
  => SchedulerProxy effScheduler
  -> a
  -> Eff (ServerEff a) ()
serve px a =
  let serverCb = toServerCallback px a
      stopServer reason = do
        (serverCb ^. terminationHandler) reason
        return (Just ())
  in  receiveSelectedLoop px (serverCb ^. requestHandlerSelector) $ \case
        Left  reason   -> stopServer reason
        Right handleIt -> handleIt >>= \case
          HandleNextRequest    -> return Nothing
          StopApiServer reason -> stopServer reason

-- | Spawn a new process, that will receive and process incoming requests
-- until the process exits.
spawnServer
  :: forall a effScheduler eff
   . ( Servable a
     , ServerEff a ~ (Process effScheduler ': effScheduler)
     , SetMember Process (Process effScheduler) eff
     , Member Interrupts eff
     , Member Interrupts (ServerEff a)
     , HasCallStack
     )
  => SchedulerProxy effScheduler
  -> a
  -> Eff eff (ServerPids a)
spawnServer px a = spawnServerWithEffects px a id

-- | Spawn a new process, that will receive and process incoming requests
-- until the process exits. Also handle all internal effects.
spawnServerWithEffects
  :: forall a effScheduler eff
   . ( Servable a
     , SetMember Process (Process effScheduler) (ServerEff a)
     , SetMember Process (Process effScheduler) eff
     , Member Interrupts eff
     , Member Interrupts (ServerEff a)
     , HasCallStack
     )
  => SchedulerProxy effScheduler
  -> a
  -> (  Eff (ServerEff a) ()
     -> Eff (Process effScheduler ': effScheduler) ()
     )
  -> Eff eff (ServerPids a)
spawnServerWithEffects px a handleEff = do
  pid <- spawn (handleEff (serve px a))
  return (toServerPids (Proxy @a) pid)

-- | Wrap an 'ApiHandler' into a composable 'ServerCallback' value.
apiHandlerServerCallback
  :: forall eff effScheduler api
   . ( HasCallStack
     , Typeable api
     , SetMember Process (Process effScheduler) eff
     , Member Interrupts eff
     )
  => SchedulerProxy effScheduler
  -> ApiHandler api eff
  -> ServerCallback eff
apiHandlerServerCallback px handlers = mempty
  { _requestHandlerSelector = selectHandlerMethod px handlers
  , _terminationHandler     = fromMaybe (const (return ()))
                                        (_terminateCallback handlers)
  }

-- | Try to parse an incoming message to an API request, and apply either
-- the 'handleCall' method or the 'handleCast' method to it.
selectHandlerMethod
  :: forall eff effScheduler api
   . ( HasCallStack
     , Typeable api
     , SetMember Process (Process effScheduler) eff
     , Member (Exc (ExitReason 'Recoverable)) eff
     )
  => SchedulerProxy effScheduler
  -> ApiHandler api eff
  -> MessageSelector (Eff eff ApiServerCmd)
selectHandlerMethod px handlers =
  selectDynamicMessageLazy (fmap (applyHandlerMethod px handlers) . fromDynamic)

-- | Apply either the '_callCallback', '_castCallback' or the '_terminateCallback'
-- callback to an incoming request.
applyHandlerMethod
  :: forall eff effScheduler api
   . ( Typeable api
     , SetMember Process (Process effScheduler) eff
     , Member (Exc (ExitReason 'Recoverable)) eff
     , HasCallStack
     )
  => SchedulerProxy effScheduler
  -> ApiHandler api eff
  -> Request api
  -> Eff eff ApiServerCmd
applyHandlerMethod px handlers (Cast request) =
  fromMaybe (unhandledCastError px) (_castCallback handlers) request
applyHandlerMethod px handlers (Call callRef fromPid request) = fromMaybe
  (unhandledCallError px)
  (_callCallback handlers)
  request
  sendReply
 where
  sendReply :: Typeable reply => reply -> Eff eff ()
  sendReply reply =
    sendMessage px fromPid (toDyn $! (Response (Proxy @api) callRef $! reply))

-- | A default handler to use in '_callCallback' in 'ApiHandler'. It will call
-- 'raiseError' with a nice error message.
unhandledCallError
  :: forall p x r q
   . ( Typeable p
     , HasCallStack
     , SetMember Process (Process q) r
     , Member (Exc (ExitReason 'Recoverable)) r
     )
  => SchedulerProxy q
  -> Api p ( 'Synchronous x)
  -> (x -> Eff r ())
  -> Eff r ApiServerCmd
unhandledCallError _px _api _ = throwError
  (ProcessError ("unhandled call on api: " ++ show (typeRep (Proxy @p))))

-- | A default handler to use in '_castCallback' in 'ApiHandler'. It will call
-- 'raiseError' with a nice error message.
unhandledCastError
  :: forall p r q
   . ( Typeable p
     , HasCallStack
     , SetMember Process (Process q) r
     , Member (Exc (ExitReason 'Recoverable)) r
     )
  => SchedulerProxy q
  -> Api p 'Asynchronous
  -> Eff r ApiServerCmd
unhandledCastError _px _api = throwError
  (ProcessError ("unhandled cast on api: " ++ show (typeRep (Proxy @p))))

-- | Either do nothing, if the error message is @Nothing@,
-- or call 'exitWithError' with the error message.
defaultTermination
  :: forall q r
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Member (Logs LogMessage) r
     )
  => SchedulerProxy q
  -> ExitReason 'Recoverable
  -> Eff r ()
defaultTermination _px r = logNotice ("server process terminating " ++ show r)
