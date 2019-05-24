-- | A better, more safe implementation of the Erlang/OTP gen_server behaviour.
--
-- @since 0.24.0
module Control.Eff.Concurrent.Protocol.Server
  ( Server(..)
  , ToServerEffects
  , State(..)
  , Reader(..)
  , Event(..)
  , spawnProtocolServer
  , spawnLinkProtocolServer
  , protocolServerLoop
  , GenServer(..)
  , GenIO
  , GenServerId(..)
  , GenServerState
  , GenServerReader
  , StartArgument(..)
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
import Data.String
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
  ( Typeable (ServerPdu a)
  , Typeable a
  , Tangible (ServerEnv a)
  , Tangible (ServerState a)
  ) =>
      Server (a :: Type) e
  where
  data StartArgument a e
  type ServerPdu a :: Type
  type ServerPdu a = a
  type ServerState a :: Type
  type ServerState a = ()
  type ServerEnv a :: Type
  type ServerEnv a = ()

  setup ::
       StartArgument a e
    -> Eff e (ServerState a, ServerEnv a)

  default setup ::
       (Default (ServerState a), Default (ServerEnv a))
    => StartArgument a e
    -> Eff e (ServerState a, ServerEnv a)
  setup _ = pure (def, def)

  update ::
       StartArgument a e
    -> Event (ServerPdu a)
    -> Eff (ToServerEffects a e) ()

-- | /Cons/ (i.e. prepend) the 'Server' 'State' and 'Reader' effects.
type ToServerEffects a e =
  State (ServerState a) ': Reader (ServerEnv a) ': e

-- | Execute the server loop.
--
-- @since 0.24.0
spawnProtocolServer
  :: forall a q h
  . ( Server a (InterruptableProcess q)
    , LogsTo h (InterruptableProcess q)
    , HasCallStack)
  => StartArgument a (InterruptableProcess q) -> Eff (InterruptableProcess q) (Endpoint (ServerPdu a))
spawnProtocolServer a = asEndpoint <$> spawn (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
spawnLinkProtocolServer
  :: forall a q h . (Server a (InterruptableProcess q), LogsTo h (InterruptableProcess q), HasCallStack)
  => StartArgument a (InterruptableProcess q) -> Eff (InterruptableProcess q) (Endpoint (ServerPdu a))
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
  => StartArgument a e -> Eff e ()
protocolServerLoop a = do
  (st, env) <- setup a
  _ <- runReader env (runState st (receiveSelectedLoop sel mainLoop))
  return ()
  where
    sel :: MessageSelector (Event (ServerPdu a))
    sel =
          OnRequest <$> selectMessage @(Request (ServerPdu a))
      <|> OnDown    <$> selectMessage @ProcessDown
      <|> OnTimeOut <$> selectMessage @TimerElapsed
      <|> OnMessage <$> selectAnyMessage
    handleInt i = do
      update a (OnInterrupt i)
      pure (Just ())
    mainLoop ::
         (Typeable a)
      => Either (Interrupt 'Recoverable) (Event (ServerPdu a))
      -> Eff (ToServerEffects a e) (Maybe ())
    mainLoop (Left i) = handleInt i
    mainLoop (Right i) = do
      update a i
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
data Event a =
    OnRequest (Request a)
  | OnInterrupt (Interrupt 'Recoverable)
  | OnDown ProcessDown
  | OnTimeOut TimerElapsed
  | OnMessage StrictDynamic
  deriving (Show,Generic,Typeable)

instance NFData a => NFData (Event a) where
   rnf = \case
       OnRequest r      -> rnf r
       OnInterrupt r    -> rnf r
       OnDown r  -> rnf r
       OnTimeOut r -> rnf r
       OnMessage r -> r `seq` ()

type instance ToPretty (Event a) = ToPretty a <+> PutStr "event"

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
      { _setupCallback :: Eff e (ServerState (GenServer tag e), ServerEnv (GenServer tag e))
      , _updateCallback
          :: Event (GenServerProtocol tag)
          -> Eff (State (GenServerState tag) ': Reader (GenServerReader tag) ': e) ()
      } -> GenServer tag e

-- | The name/id of a 'GenServer' for logging purposes.
--
-- @since 0.24.0
newtype GenServerId tag =
  MkGenServerId { _fromGenServerId :: T.Text }
  deriving (Typeable, NFData, Ord, Eq, IsString)

instance Show (GenServerId tag) where
  showsPrec _d (MkGenServerId x) = showString (T.unpack x)

-- | The 'ServerPdu' un-wrapper type function.
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
  type ServerPdu (GenServer tag e) = GenServerProtocol tag
  type ServerState (GenServer tag e) = GenServerState tag
  type ServerEnv (GenServer tag e) = GenServerReader tag
  data instance StartArgument (GenServer tag e) e =
        MkGenStartArgument
         { _genServerId :: GenServerId tag
         , _genServerCallbacks :: GenServer tag e
         } deriving Typeable
  setup (MkGenStartArgument _ cb) = _setupCallback cb
  update (MkGenStartArgument _ cb) req = _updateCallback cb req

instance NFData (StartArgument (GenServer tag e) e) where
  rnf (MkGenStartArgument x _) = rnf x

instance Typeable tag => Show (StartArgument (GenServer tag e) e) where
  showsPrec d (MkGenStartArgument x _) =
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

-- | Create a 'Stateless' 'GenServer'.
--
-- This requires only the callback for 'Event's
-- and a 'GenServerId'.
--
-- This is Haskell, so if this functions is partially applied
-- to some 'Event' callback, you get a function back,
-- that generates 'StartArgument's from 'GenServerId's, like a /factory/
--
-- @since 0.24.0
simpleGenServer
  :: ( Typeable tag
     , HasCallStack
     , GenIO e h q
     , Server (GenServer (StatelessGenServer tag) e) e
     , PrettyTypeShow (ToPretty tag)
     )
  => (GenServerId (StatelessGenServer tag) -> Event tag -> Eff e ())
  -> GenServerId (StatelessGenServer tag)
  -> StartArgument (GenServer (StatelessGenServer tag) e) e
simpleGenServer stepCb i =
  MkGenStartArgument
    { _genServerId = i
    , _genServerCallbacks =
        MkGenServer { _setupCallback = pure ((), ())
                    , _updateCallback = runStep
                    }
    }
   where
    runStep loopEvent =  raise . raise $ stepCb i (coerce loopEvent)
