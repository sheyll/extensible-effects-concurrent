-- | A better, more safe implementation of the Erlang/OTP gen_server behaviour.
--
-- @since 0.24.0
module Control.Eff.Concurrent.Protocol.Server
  ( Server(..)
  , ServerLoopEvent(..)
  , spawnProtocolServer
  , spawnLinkProtocolServer
  , protocolServerLoop
  , GenServer(..)
  , GenIO
  , GenServerId(..)
  , GenServerState
  , GenServerReader
  , ServerArgument(..)
  , StatelessGenServer
  , simpleGenServer
  )
  where

import Control.Applicative
import Control.DeepSeq
import Control.Eff
import Control.Eff.Extend
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Request
import Control.Eff.Log
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Data.Coerce
import Data.Default
import Data.Kind
import Data.Typeable
import Data.Type.Pretty
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import GHC.Generics

-- | A type class for 'Pdu' values that have an implementation
-- which handles the corresponding protocol.
--
-- @since 0.24.0
class
  ( Typeable (Protocol a)
  , Typeable a
  , Tangible (ServerEnv a)
  , Tangible (ServerState a)
  ) =>
      Server (a :: Type) e
  where
  data ServerArgument a e
  type Protocol a :: Type
  type Protocol a = a
  type ServerState a :: Type
  type ServerState a = ()
  type ServerEnv a :: Type
  type ServerEnv a = ()

  serverInit ::
       ServerArgument a e
    -> Eff e (ServerState a, ServerEnv a)

  default serverInit ::
       (Default (ServerState a), Default (ServerEnv a))
    => ServerArgument a e
    -> Eff e (ServerState a, ServerEnv a)
  serverInit _ = pure (def, def)

  stepServerLoop ::
       ServerArgument a e
    -> ServerLoopEvent (Protocol a)
    -> Eff (State (ServerState a) ': Reader (ServerEnv a) ': e) ()

  recoverFromInterrupt ::
       ServerArgument a e
    -> Interrupt 'Recoverable
    -> Eff (State (ServerState a) ': Reader (ServerEnv a) ': e) ()

  default recoverFromInterrupt ::
       (SetMember Process (Process q) e)
    => ServerArgument a e
    -> Interrupt 'Recoverable
    -> Eff (State (ServerState a) ': Reader (ServerEnv a) ': e) ()
  recoverFromInterrupt _ = exitBecause . interruptToExit


-- | Execute the server loop.
--
-- @since 0.24.0
spawnProtocolServer
  :: forall a q h
  . ( Server a (InterruptableProcess q)
    , LogsTo h (InterruptableProcess q)
    , HasCallStack)
  => ServerArgument a (InterruptableProcess q) -> Eff (InterruptableProcess q) (Endpoint (Protocol a))
spawnProtocolServer a = asEndpoint <$> spawn (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
spawnLinkProtocolServer
  :: forall a q h . (Server a (InterruptableProcess q), LogsTo h (InterruptableProcess q), HasCallStack)
  => ServerArgument a (InterruptableProcess q) -> Eff (InterruptableProcess q) (Endpoint (Protocol a))
spawnLinkProtocolServer a = asEndpoint <$> spawnLink (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
protocolServerLoop
     :: forall q e h a
     . ( Server a e
       , SetMember Process (Process q) e
       , Member Interrupts e
       , LogsTo h e
       )
  => ServerArgument a e -> Eff e ()
protocolServerLoop a = do
  (st, env) <- serverInit a
  _ <- runReader env (runState st (receiveSelectedLoop sel mainLoop))
  return ()
  where
    sel :: MessageSelector (ServerLoopEvent (Protocol a))
    sel =
          ServerLoopRequest      <$> selectMessage @(Request (Protocol a))
      <|> ServerLoopProcessDown  <$> selectMessage @ProcessDown
      <|> ServerLoopTimerElapsed <$> selectMessage @TimerElapsed
      <|> ServerLoopOtherMessage <$> selectAnyMessage
    handleInt i = do
      recoverFromInterrupt a i
      pure (Just ())
    mainLoop ::
         (Typeable a)
      => Either (Interrupt 'Recoverable) (ServerLoopEvent (Protocol a))
      -> Eff (State (ServerState a) ': Reader (ServerEnv a) ': e) (Maybe ())
    mainLoop (Left i) = handleInt i
    mainLoop (Right i) = do
      stepServerLoop a i
      pure (Just ())


-- | The underlying effects for 'GenServer's:
--     * Logging
--     * Interrupts
--     * Processes
--     * IO
--
-- @since 0.24.0
-- type GenIO = InterruptableProcess LoggingAndIo
type GenIO e q h =
  ( Member Interrupts e
  , SetMember Process (Process q) e
  , LogsTo h q
  , LogsTo h e
  , Lifted IO e
  , Lifted IO q
  )

-- | Internal protocol to communicate incoming messages and other events to the
-- instances of 'Server'.
--
-- Note that this is required to receive any kind of messages in 'protocolServerLoop'.
--
-- @since 0.24.0
data ServerLoopEvent a =
    ServerLoopRequest (Request a)
  | ServerLoopProcessDown ProcessDown
  | ServerLoopTimerElapsed TimerElapsed
  | ServerLoopOtherMessage StrictDynamic
  deriving (Show,Generic,Typeable)

instance NFData a => NFData (ServerLoopEvent a) where
   rnf = \case
       ServerLoopRequest r      -> rnf r
       ServerLoopProcessDown r  -> rnf r
       ServerLoopTimerElapsed r -> rnf r
       ServerLoopOtherMessage r -> r `seq` ()


type instance ToPretty (ServerLoopEvent a) = PutStr "sever-loop-event<" <++> ToPretty a <++> PutStr ">"


-- * GenServer

-- | A helper for 'Server's that are directly based on logging and 'IO': 'GenIO'
--
-- A record that contains callbacks to provide a 'Server' instance for the
-- @tag@ parameter, .
--
-- The name prefix @Gen@ indicates the inspiration from Erlang's @gen_server@ module.
--
-- @since 0.24.0
data GenServer tag e where
  MkGenServer :: GenIO e h q =>
      { _serverInitCallback :: GenServerId tag -> Eff e (ServerState (GenServer tag e), ServerEnv (GenServer tag e))
      , _stepServerLoopCallback
          :: GenServerId tag
          -> ServerLoopEvent (GenServerProtocol tag)
          -> Eff (State (GenServerState tag) ': Reader (GenServerReader tag) ': e) ()
      , _recoverFromInterruptCallback
          :: GenServerId tag
          -> Interrupt 'Recoverable
          -> Eff (State (GenServerState tag) ': Reader (GenServerReader tag) ': e) ()
      } -> GenServer tag e

-- | The name/id of a 'GenServer' for logging purposes.
--
-- @since 0.24.0
newtype GenServerId tag =
  MkGenServerId { _fromGenServerId :: T.Text }
  deriving (Typeable, NFData, Ord, Eq)

instance Show (GenServerId tag) where
  showsPrec _d (MkGenServerId x) = showString (T.unpack x)

-- | The 'Protocol' un-wrapper type function.
--
-- @since 0.24.0
type family GenServerProtocol tag

-- | Type of state for 'GenServer' based 'Server's
--
-- @since 0.24.0
type family GenServerState tag

-- | Type of the environment for 'GenServer' based 'Server's
--
-- @since 0.24.0
type family GenServerReader tag

instance ( Tangible (GenServerState tag)
         , Tangible (GenServerReader tag)
         , Typeable (GenServerProtocol tag)
         , Typeable tag
         , Typeable e
         , GenIO e h q
         ) => Server (GenServer (tag :: Type) e) e where
  type Protocol (GenServer tag e) = GenServerProtocol tag
  type ServerState (GenServer tag e) = GenServerState tag
  type ServerEnv (GenServer tag e) = GenServerReader tag
  data instance ServerArgument (GenServer tag e) e =
        MkGenServerArgument
          { _genServerId :: GenServerId tag
          , _genServerCallbacks :: GenServer tag e
          }
          deriving Typeable
  serverInit (MkGenServerArgument x cb) = _serverInitCallback cb x
  stepServerLoop (MkGenServerArgument x cb) req = _stepServerLoopCallback cb x req
  recoverFromInterrupt (MkGenServerArgument x cb) i = _recoverFromInterruptCallback cb x i

instance NFData (ServerArgument (GenServer tag e) e) where
  rnf (MkGenServerArgument x _) = rnf x

instance Typeable tag => Show (ServerArgument (GenServer tag e) e) where
  showsPrec d (MkGenServerArgument x _) =
    showParen (d>=10)
      ( showsPrec 11 x
      . showChar ' ' . showsTypeRep (typeRep (Proxy @tag))
      . showString " gen-server"
      )

-- ** 'GenServer' based Server constructors

-- | The type-level tag indicating a stateless 'Server' instance.
--
-- There are 'GenServerState', 'GenServerReader' and 'GenServerProtocol' as well as
-- 'ToPretty' instances for this type.
--
-- @since 0.24.0
data StatelessGenServer tag deriving Typeable

type instance GenServerState (StatelessGenServer tag) = ()
type instance GenServerReader (StatelessGenServer tag) = ()
type instance GenServerProtocol (StatelessGenServer tag) = tag
type instance ToPretty (StatelessGenServer t) = ToPretty t

-- | Create a simple 'GenServer' from a single 'Pdu' callback.
--
-- @since 0.24.0
simpleGenServer
  :: ( Typeable tag
     , HasCallStack
     , GenIO e h q
     , Server (GenServer (StatelessGenServer tag) e) e
     , PrettyTypeShow (ToPretty tag)
     )
  => (GenServerId (StatelessGenServer tag) -> ServerLoopEvent tag -> Eff e ())
  -> GenServer (StatelessGenServer tag) e
simpleGenServer stepCb =
  MkGenServer { _serverInitCallback = const (pure ((), ()))
              , _stepServerLoopCallback = runStep
              , _recoverFromInterruptCallback = noRecovery
              }
   where
    runStep i loopEvent =  raise . raise $ stepCb i (coerce loopEvent)
    noRecovery _ i = do
      let e = T.pack (show i)
      logError e
      exitBecause (ExitUnhandledError e)