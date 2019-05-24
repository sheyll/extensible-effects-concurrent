-- | A better, more safe implementation of the Erlang/OTP gen_server behaviour.
--
-- @since 0.24.0
module Control.Eff.Concurrent.Protocol.Server
  ( Server(..)
  , StartArgument(..)
  , ToServerEffects
  , ModelState
  , modifyModel
  , getAndModifyModel
  , modifyAndGetModel
  , getModel
  , putModel
  , getAndPutModel
  , useModel
  , zoomModel
  , SettingsReader
  , askSettings
  , viewSettings
  , Event(..)
  , start
  , startLink
  , protocolServerLoop
  , GenServer(..)
  , ToGenServerEffects
  , GenIO
  , GenServerId(..)
  , GenServerProtocol
  , GenServerModel
  , GenServerSettings
  , genServer
  , Stateless
  , ToStatelessEffects
  , statelessGenServer
  -- * Re-exports
  , Request(..)
  , sendReply
  , RequestOrigin(..)
  )
  where

import Control.Applicative
import Control.DeepSeq
import Control.Eff
import Control.Eff.Extend ()
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Request
import Control.Eff.Log
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Lens
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
  ( Typeable a
  , Typeable (Protocol a)
  , Tangible (Settings a)
  , Tangible (Model a)
  ) =>
      Server (a :: Type) e
  where
  -- | The value that defines what is required to initiate a 'Server'
  -- loop.
  data StartArgument a e
  -- | The index type of the 'Event's that this server processes.
  -- This is the first parameter to the 'Request' and therefore of
  -- the 'Pdu' family.
  type Protocol a :: Type
  type Protocol a = a
  -- | Type of the /model/ data, given to every invocation of 'update'
  -- via the 'ModelState' effect.
  -- The /model/ of a server loop is changed through incoming 'Event's.
  -- It is initially calculated by 'setup'.
  type Model a :: Type
  type Model a = ()
  -- | Type of read-only state.
  type Settings a :: Type
  type Settings a = ()

  setup ::
       StartArgument a e
    -> Eff e (Model a, Settings a)

  default setup ::
       (Default (Model a), Default (Settings a))
    => StartArgument a e
    -> Eff e (Model a, Settings a)
  setup _ = pure (def, def)

  update ::
       StartArgument a e
    -> Event (Protocol a)
    -> Eff (ToServerEffects a e) ()

-- | /Cons/ (i.e. prepend) 'ModelState' and 'SettingsReader' to an
-- effect list.
--
-- @since 0.24.0
type ToServerEffects a e =
  ModelState a ': SettingsReader a ': e

-- | The 'Eff'ect type of mutable 'Model' in a 'Server' instance.
--
-- @since 0.24.0
type ModelState a = State (Model a)

-- | Modify the 'Model' of a 'Server'.
--
-- @since 0.24.0
modifyModel :: forall a e . Member (ModelState a) e => (Model a -> Model a) -> Eff e ()
modifyModel f = getModel @a >>= putModel @a . f

-- | Modify the 'Model' of a 'Server' and return the old value.
--
-- @since 0.24.0
getAndModifyModel :: forall a e . Member (ModelState a) e => (Model a -> Model a) -> Eff e (Model a)
getAndModifyModel f = getModel @a <* modify f

-- | Modify the 'Model' of a 'Server' and return the new value.
--
-- @since 0.24.0
modifyAndGetModel :: forall a e . Member (ModelState a) e => (Model a -> Model a) -> Eff e (Model a)
modifyAndGetModel f = modifyModel @a f *> getModel @a

-- | Return the 'Model' of a 'Server'.
--
-- @since 0.24.0
getModel :: forall a e . Member (ModelState a) e => Eff e (Model a)
getModel = get

-- | Return a element selected by a 'Lens' of the 'Model' of a 'Server'.
--
-- @since 0.24.0
useModel :: forall a b e . Member (ModelState a) e => Getting b (Model a) b -> Eff e b
useModel l = view l <$> getModel @a

-- | Overwrite the 'Model' of a 'Server'.
--
-- @since 0.24.0
putModel :: forall a e . Member (ModelState a) e => Model a -> Eff e ()
putModel = put

-- | Overwrite the 'Model' of a 'Server', return the old value.
--
-- @since 0.24.0
getAndPutModel :: forall a e . Member (ModelState a) e => Model a -> Eff e (Model a)
getAndPutModel m = getModel @a <* putModel @a m

-- | Run an action that modifies portions of the 'Model' of a 'Server' defined by the given 'Lens'.
--
-- @since 0.24.0
zoomModel :: forall a b c e. Member (ModelState a) e => Lens' (Model a) b -> Eff (State b ': e) c -> Eff e c
zoomModel l a = do
  m0 <- getModel @a
  (c, m1) <- runState (view l m0) a
  modifyModel @a (l .~ m1)
  return c

-- | The 'Eff'ect type of readonly 'Settings' in a 'Server' instance.
--
-- @since 0.24.0
type SettingsReader a = Reader (Settings a)

-- | Return the read-only 'Settings' of a 'Server'
--
-- @since 0.24.0
askSettings :: forall a e . Member (SettingsReader a) e => Eff e (Settings a)
askSettings = ask

-- | Return the read-only 'Settings' of a 'Server' as viewed through a 'Lens'
--
-- @since 0.24.0
viewSettings :: forall a b e . Member (SettingsReader a) e =>  Getting b (Settings a) b -> Eff e b
viewSettings l = view l <$> askSettings @a

-- | Execute the server loop.
--
-- @since 0.24.0
start
  :: forall a q h
  . ( Server a (InterruptableProcess q)
    , LogsTo h (InterruptableProcess q)
    , HasCallStack)
  => StartArgument a (InterruptableProcess q) -> Eff (InterruptableProcess q) (Endpoint (Protocol a))
start a = asEndpoint <$> spawn (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
startLink
  :: forall a q h . (Server a (InterruptableProcess q), LogsTo h (InterruptableProcess q), HasCallStack)
  => StartArgument a (InterruptableProcess q) -> Eff (InterruptableProcess q) (Endpoint (Protocol a))
startLink a = asEndpoint <$> spawnLink (protocolServerLoop a)

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
    sel :: MessageSelector (Event (Protocol a))
    sel =
          OnRequest <$> selectMessage @(Request (Protocol a))
      <|> OnDown    <$> selectMessage @ProcessDown
      <|> OnTimeOut <$> selectMessage @TimerElapsed
      <|> OnMessage <$> selectAnyMessage
    handleInt i = do
      update a (OnInterrupt i)
      pure (Just ())
    mainLoop ::
         (Typeable a)
      => Either (Interrupt 'Recoverable) (Event (Protocol a))
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
type GenIO e q =
  ( Member Interrupts e
  , SetMember Process (Process q) e
  , LogsTo IO q
  , LogsTo IO e
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
  MkGenServer :: GenIO e q =>
      { _setupCallback :: Eff e (Model (GenServer tag e), Settings (GenServer tag e))
      , _updateCallback
          :: Event (GenServerProtocol tag)
          -> Eff (ToGenServerEffects tag e) ()
      } -> GenServer tag e

-- | Prepend the 'ModelState' for 'GenServerModel' and 'SettingsReader' for 'GenServerSettings' of a 'GenServer'
-- 'Server' to an effect list.
--
-- @since 0.24.0
type ToGenServerEffects tag e = ToServerEffects (GenServer tag e) e

-- | The name/id of a 'GenServer' for logging purposes.
--
-- @since 0.24.0
newtype GenServerId tag =
  MkGenServerId { _fromGenServerId :: T.Text }
  deriving (Typeable, NFData, Ord, Eq, IsString)

instance Show (GenServerId tag) where
  showsPrec _d (MkGenServerId x) = showString (T.unpack x)

-- | The 'Protocol' un-wrapper type function.
--
-- @since 0.24.0
type family GenServerProtocol tag

-- | Type of state for 'GenServer' based 'Server's
--
-- @since 0.24.0
type family GenServerModel tag

-- | Type of the environment for 'GenServer' based 'Server's
--
-- @since 0.24.0
type family GenServerSettings tag

instance ( Tangible (GenServerModel tag)
         , Tangible (GenServerSettings tag)
         , Typeable (GenServerProtocol tag)
         , Typeable tag
         , Typeable e
         , GenIO e q
         ) => Server (GenServer (tag :: Type) e) e where
  type Protocol (GenServer tag e) = GenServerProtocol tag
  type Model (GenServer tag e) = GenServerModel tag
  type Settings (GenServer tag e) = GenServerSettings tag
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


-- | Create a 'GenServer'.
--
-- This requires the callback for 'Event's, a initial 'Model' and a 'GenServerId'.
--
-- There must be a 'GenServerModel' instance.
-- There must be a 'GenServerSettings' instance.
-- There must be a 'GenServerProtocol' instance.
--
-- This is Haskell, so if this functions is partially applied
-- to some 'Event' callback, you get a function back,
-- that generates 'StartArgument's from 'GenServerId's, like a /factory/
--
-- @since 0.24.0
genServer
  :: forall tag e q .
     ( Typeable tag
     , HasCallStack
     , GenIO e q
     , Server (GenServer tag e) e
     , PrettyTypeShow (ToPretty tag)
     )
  => (GenServerId tag -> Eff e (GenServerModel tag, GenServerSettings tag))
  -> (GenServerId tag -> Event (GenServerProtocol tag) -> Eff (ToGenServerEffects tag e) ())
  -> GenServerId tag
  -> StartArgument (GenServer tag e) e
genServer initCb stepCb i =
  MkGenStartArgument
    { _genServerId = i
    , _genServerCallbacks =
        MkGenServer { _setupCallback = initCb i
                    , _updateCallback = stepCb i . coerce
                    }
    }

-- | The type-level tag indicating a stateless 'Server' instance.
--
-- There are 'GenServerModel', 'GenServerSettings' and 'GenServerProtocol' as well as
-- 'ToPretty' instances for this type.
--
-- See also 'ToStatelessEffects'.
--
-- @since 0.24.0
data Stateless tag deriving Typeable

-- | Prepend the 'ModelState' and 'SettingsReader' of a 'Stateless'
-- 'Server' to an effect list. The 'Model' and 'Settings' of a 'Stateless'
-- 'Server' are just @()@ /unit/.
--
-- @since 0.24.0
type ToStatelessEffects e = State () ': Reader () ': e

type instance GenServerModel (Stateless tag) = ()
type instance GenServerSettings (Stateless tag) = ()
type instance GenServerProtocol (Stateless tag) = GenServerProtocol tag
type instance ToPretty (Stateless t) = ToPretty t

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
statelessGenServer
  :: forall tag e q . ( Typeable tag
     , HasCallStack
     , GenIO e q
     , Server (GenServer (Stateless tag) e) e
     , PrettyTypeShow (ToPretty tag)
     )
  => (GenServerId tag -> Event (GenServerProtocol tag) -> Eff (ToStatelessEffects e) ())
  -> GenServerId tag
  -> StartArgument (GenServer (Stateless tag) e) e
statelessGenServer stepCb (MkGenServerId i) =
  genServer (const (pure ((), ()))) runStep (MkGenServerId i)
   where
    runStep :: GenServerId (Stateless tag) -> Event (GenServerProtocol (Stateless tag)) -> Eff (ToStatelessEffects e) ()
    runStep (MkGenServerId i') loopEvent = stepCb (MkGenServerId i') (coerce loopEvent)
