-- | Experimental new 'Api' server handler.
--
-- @since 0.13.2
module Control.Eff.Concurrent.Api.Server2
  ( -- * Starting Api Servers
    spawnApiServer
  , spawnApiServerStateful
  , spawnApiServerEffectful
  -- ** Api Server Callbacks
  , CallbackResult(..)
  , MessageCallback(..)
  -- ** Callback Smart Contructors
  -- *** Calls and Casts (for 'Api's)
  , handleCasts
  , handleCalls
  , handleCastsAndCalls
  -- *** Generic Message Handler
  , handleMessages
  , handleSelectedMessages
  , handleAnyMessages
  , handleProcessDowns
  -- *** Fallback Handler
  , dropUnhandledMessages
  , exitOnUnhandled
  , logUnhandledMessages
  -- ** Api Composition
  , (^:)
  , fallbackHandler
  , ToServerPids(..)
  -- ** Interrupt handler
  , InterruptCallback(..)
  , stopServerOnInterrupt
  )
where

import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.Log
import           Control.Eff.State.Lazy
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Internal
import           Control.Eff.Concurrent.Process
import           Control.Monad                  ( (>=>) )
import           Data.Proxy
import           Data.Dynamic
import           Control.Applicative
import           GHC.Stack
import           Control.DeepSeq
import           Data.Kind
import           Data.Foldable
import           Data.Default

-- | /Server/ an 'Api' in a newly spawned process.
--
-- @since 0.13.2
spawnApiServer
  :: forall api eff
   . (ToServerPids api, HasCallStack)
  => MessageCallback api (InterruptableProcess eff)
  -> InterruptCallback (ConsProcess eff)
  -> Eff (InterruptableProcess eff) (ServerPids api)
spawnApiServer (MessageCallback sel cb) (InterruptCallback intCb) =
  toServerPids (Proxy @api)
    <$> (spawnRaw $ receiveSelectedLoop
          (SP @eff)
          sel
          (   either (fmap Left . intCb) (fmap Right . provideInterrupts . cb)
          >=> handleCallbackResult
          )
        )
 where
  handleCallbackResult
    :: Either CallbackResult (Either InterruptReason CallbackResult)
    -> Eff (ConsProcess eff) (Maybe ())
  handleCallbackResult (Left AwaitNext) = return Nothing
  handleCallbackResult (Left (StopServer r)) = exitBecause SP (NotRecovered r)
  handleCallbackResult (Right (Right AwaitNext)) = return Nothing
  handleCallbackResult (Right (Right (StopServer r))) =
    intCb r >>= handleCallbackResult . Left
  handleCallbackResult (Right (Left r)) =
    intCb r >>= handleCallbackResult . Left

-- | /Server/ an 'Api' in a newly spawned process; the callbacks have access
-- to some state initialed by the function in the first parameter.
--
-- @since 0.13.2
spawnApiServerStateful
  :: forall api eff state
   . (HasCallStack, ToServerPids api)
  => Eff (InterruptableProcess eff) state
  -> MessageCallback api (State state ': InterruptableProcess eff)
  -> InterruptCallback (State state ': ConsProcess eff)
  -> Eff (InterruptableProcess eff) (ServerPids api)
spawnApiServerStateful initEffect (MessageCallback sel cb) (InterruptCallback intCb)
  = fmap (toServerPids (Proxy @api)) $ spawnRaw $ do
    state <- provideInterruptsShutdown initEffect
    evalState state $ receiveSelectedLoop (SP @eff) sel $ \msg -> case msg of
      Left  m -> invokeIntCb m
      Right m -> do
        s <- get
        r <- raise (provideInterrupts (evalState s (cb m)))
        case r of
          Left  i              -> invokeIntCb i
          Right (StopServer i) -> invokeIntCb i
          Right AwaitNext      -> return Nothing
 where
  invokeIntCb j = do
    l <- intCb j
    case l of
      AwaitNext                  -> return Nothing
      StopServer ProcessFinished -> return (Just ())
      StopServer k               -> exitBecause SP (NotRecovered k)

-- | /Server/ an 'Api' in a newly spawned process; The caller provides an
-- effect handler for arbitrary effects used by the server callbacks.
--
-- @since 0.13.2
spawnApiServerEffectful
  :: forall api eff serverEff
   . ( HasCallStack
     , ToServerPids api
     , Member Interrupts serverEff
     , SetMember Process (Process eff) serverEff
     )
  => (forall b . Eff serverEff b -> Eff (InterruptableProcess eff) b)
  -> MessageCallback api serverEff
  -> InterruptCallback serverEff
  -> Eff (InterruptableProcess eff) (ServerPids api)
spawnApiServerEffectful handleServerInteralEffects scb (InterruptCallback intCb)
  = toServerPids (Proxy @api) <$> spawn (handleServerInteralEffects (go scb))
 where
  go (MessageCallback sel cb) = receiveSelectedLoop
    (SP @eff)
    sel
    (   either (fmap Left . intCb) (fmap Right . tryUninterrupted . cb)
    >=> handleCallbackResult
    )
   where
    handleCallbackResult
      :: Either CallbackResult (Either InterruptReason CallbackResult)
      -> Eff serverEff (Maybe ())
    handleCallbackResult (Left AwaitNext) = return Nothing
    handleCallbackResult (Left (StopServer r)) =
      exitBecause SP (NotRecovered r)
    handleCallbackResult (Right (Right AwaitNext)) = return Nothing
    handleCallbackResult (Right (Right (StopServer r))) =
      intCb r >>= handleCallbackResult . Left
    handleCallbackResult (Right (Left r)) =
      intCb r >>= handleCallbackResult . Left

-- | A command to the server loop started e.g. by 'server' or 'spawnServerWithEffects'.
-- Typically returned by an 'ApiHandler' member to indicate if the server
-- should continue or stop.
--
-- @since 0.13.2
data CallbackResult where
  -- | Tell the server to keep the server loop running
  AwaitNext :: CallbackResult
  -- | Tell the server to exit, this will make 'serve' stop handling requests without
  -- exitting the process. '_terminateCallback' will be invoked with the given
  -- optional reason.
  StopServer :: InterruptReason -> CallbackResult
  --  SendReply :: reply -> CallbackResult () -> CallbackResult (reply -> Eff eff ())
  deriving ( Typeable )


-- | An existential wrapper around  a 'MessageSelector' and a function that
-- handles the selected message. The @api@ type parameter is a phantom type.
--
-- The return value if the handler function is a 'CallbackResult'.
--
-- @since 0.13.2
data MessageCallback api eff where
   MessageCallback :: MessageSelector a -> (a -> Eff eff CallbackResult) -> MessageCallback api eff

instance Semigroup (MessageCallback api eff) where
 (MessageCallback selL runL) <> (MessageCallback selR runR) =
    MessageCallback (Left <$> selL <|> Right <$> selR) (either runL runR)

instance Monoid (MessageCallback api eff) where
  mappend = (<>)
  mempty = MessageCallback selectAnyMessageLazy (const (pure AwaitNext))

instance Default (MessageCallback api eff) where
  def = mempty

-- | A smart constructor for 'MessageCallback's
--
-- @since 0.13.2
handleMessages
  :: forall eff a
   . (HasCallStack, NFData a, Typeable a)
  => (a -> Eff eff CallbackResult)
  -> MessageCallback '[] eff
handleMessages = MessageCallback selectMessage

-- | A smart constructor for 'MessageCallback's
--
-- @since 0.13.2
handleSelectedMessages
  :: forall eff a
   . HasCallStack
  => MessageSelector a
  -> (a -> Eff eff CallbackResult)
  -> MessageCallback '[] eff
handleSelectedMessages = MessageCallback

-- | A smart constructor for 'MessageCallback's
--
-- @since 0.13.2
handleAnyMessages
  :: forall eff
   . HasCallStack
  => (Dynamic -> Eff eff CallbackResult)
  -> MessageCallback '[] eff
handleAnyMessages = MessageCallback selectAnyMessageLazy

-- | A smart constructor for 'MessageCallback's
--
-- @since 0.13.2
handleCasts
  :: forall api eff
   . (HasCallStack, Typeable api, Typeable (Api api 'Asynchronous))
  => (Api api 'Asynchronous -> Eff eff CallbackResult)
  -> MessageCallback api eff
handleCasts h = MessageCallback
  (selectMessageWithLazy
    (\case
      cr@(Cast _ :: Request api) -> Just cr
      _callReq                   -> Nothing
    )
  )
  (\(Cast req :: Request api) -> h req)

-- | A smart constructor for 'MessageCallback's
--
-- > handleCalls SP
-- >   (\ (RentBook bookId customerId) runCall ->
-- >      runCall $ do
-- >          rentalIdE <- rentBook bookId customerId
-- >          case rentalIdE of
-- >            -- on fail we just don't send a reply, let the caller run into
-- >            -- timeout
-- >            Left err -> return (Nothing, AwaitNext)
-- >            Right rentalId -> return (Just rentalId, AwaitNext))
--
-- @since 0.13.2
handleCalls
  :: forall api eff effScheduler
   . ( HasCallStack
     , Typeable api
     , SetMember Process (Process effScheduler) eff
     , Member Interrupts eff
     )
  => SchedulerProxy effScheduler
  -> (  forall secret reply
      . (Typeable reply, Typeable (Api api ( 'Synchronous reply)))
     => Api api ( 'Synchronous reply)
     -> (Eff eff (Maybe reply, CallbackResult) -> secret)
     -> secret
     )
  -> MessageCallback api eff
handleCalls px h = MessageCallback
  (selectMessageWithLazy
    (\case
      (Cast _ :: Request api) -> Nothing
      callReq                 -> Just callReq
    )
  )
  (\(Call callRef fromPid req :: Request api) -> h
    req
    (\resAction -> do
      (mReply, cbResult) <- resAction
      traverse_ (sendReply fromPid callRef) mReply
      return cbResult
    )
  )
 where
  sendReply :: (Typeable reply) => ProcessId -> Int -> reply -> Eff eff ()
  sendReply fromPid callRef reply =
    sendMessage px fromPid (Response (Proxy @api) callRef $! reply)


-- | A smart constructor for 'MessageCallback's
--
-- @since 0.13.2
handleCastsAndCalls
  :: forall api eff effScheduler
   . ( HasCallStack
     , Typeable api
     , Typeable (Api api 'Asynchronous)
     , SetMember Process (Process effScheduler) eff
     , Member Interrupts eff
     )
  => SchedulerProxy effScheduler
  -> (Api api 'Asynchronous -> Eff eff CallbackResult)
  -> (  forall secret reply
      . (Typeable reply, Typeable (Api api ( 'Synchronous reply)))
     => Api api ( 'Synchronous reply)
     -> (Eff eff (Maybe reply, CallbackResult) -> secret)
     -> secret
     )
  -> MessageCallback api eff
handleCastsAndCalls px onCast onCall =
  handleCalls px onCall <> handleCasts onCast

-- | A smart constructor for 'MessageCallback's
--
-- @since 0.13.2
handleProcessDowns
  :: forall eff
   . HasCallStack
  => (MonitorReference -> Eff eff CallbackResult)
  -> MessageCallback '[] eff
handleProcessDowns k = MessageCallback selectMessage (k . downReference)

-- | Compose two 'Api's to a type-leve pair of them.
--
-- > handleCalls api1calls ^: handleCalls api2calls ^:
--
-- @since 0.13.2
(^:)
  :: forall (api1 :: Type) (apis2 :: [Type]) eff
   . HasCallStack
  => MessageCallback api1 eff
  -> MessageCallback apis2 eff
  -> MessageCallback (api1 ': apis2) eff
(MessageCallback selL runL) ^: (MessageCallback selR runR) =
  MessageCallback (Left <$> selL <|> Right <$> selR) (either runL runR)

infixr 5 ^:

-- | Make a fallback handler, i.e. a handler to which no other can be composed
-- to from the right.
--
-- @since 0.13.2
fallbackHandler
  :: forall api eff
   . HasCallStack
  => MessageCallback api eff
  -> MessageCallback '[] eff
fallbackHandler (MessageCallback s r) = MessageCallback s r

-- | A 'fallbackHandler' that drops the left-over messages.
--
-- @since 0.13.2
dropUnhandledMessages :: forall eff . HasCallStack => MessageCallback '[] eff
dropUnhandledMessages =
  MessageCallback selectAnyMessageLazy (const (return AwaitNext))

-- | A 'fallbackHandler' that terminates if there are unhandled messages.
--
-- @since 0.13.2
exitOnUnhandled :: forall eff . HasCallStack => MessageCallback '[] eff
exitOnUnhandled = MessageCallback selectAnyMessageLazy $ \msg ->
  return (StopServer (ProcessError ("unhandled message " ++ show msg)))

-- | A 'fallbackHandler' that drops the left-over messages.
--
-- @since 0.13.2
logUnhandledMessages
  :: forall eff
   . (Member (Logs LogMessage) eff, HasCallStack)
  => MessageCallback '[] eff
logUnhandledMessages = MessageCallback selectAnyMessageLazy $ \msg -> do
  logWarning ("ignoring unhandled message " ++ show msg)
  return AwaitNext

-- | Helper type class for the return values of 'spawnApiServer' et al.
--
-- @since 0.13.2
class ToServerPids (t :: k) where
  type ServerPids t
  toServerPids :: proxy t -> ProcessId -> ServerPids t

instance ToServerPids '[] where
  type ServerPids '[] = ProcessId
  toServerPids _ = id

instance
  forall (api1 :: Type) (api2 :: [Type])
  . (ToServerPids api1, ToServerPids api2)
  => ToServerPids (api1 ': api2) where
  type ServerPids (api1 ': api2) = (ServerPids api1, ServerPids api2)
  toServerPids _ p =
    (toServerPids (Proxy @api1) p, toServerPids (Proxy @api2) p)

instance
  forall (api1 :: Type)
  . (ToServerPids api1)
  => ToServerPids api1 where
  type ServerPids api1 = Server api1
  toServerPids _ = asServer

-- | Just a wrapper around a function that will be applied to the result of
-- a 'MessageCallback's 'StopServer' clause, or an 'InterruptReason' caught during
-- the execution of @receive@ or a 'MessageCallback'
--
-- @since 0.13.2
data InterruptCallback eff where
   InterruptCallback ::
     (InterruptReason -> Eff eff CallbackResult) -> InterruptCallback eff

instance Default (InterruptCallback eff) where
  def = stopServerOnInterrupt

-- | A smart constructor for 'InterruptCallback's
--
-- @since 0.13.2
stopServerOnInterrupt :: forall eff . HasCallStack => InterruptCallback eff
stopServerOnInterrupt = InterruptCallback (pure . StopServer)
