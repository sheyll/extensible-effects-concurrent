-- | Utilities to implement /effectful server-loops/.
--
-- @since 0.24.0
module Control.Eff.Concurrent.Protocol.EffectfulServer
  ( Server(..)
  , Event(..)
  , start
  , startLink
  , protocolServerLoop
  -- * GenServer
  , TangibleGenServer
  , GenServer
  , GenServerId(..)
  , genServer
  -- * Re-exports
  , RequestOrigin(..)
  , Reply(..)
  , sendReply
  , toEmbeddedOrigin
  , embedReplySerializer
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
import Control.Lens
import Data.Kind
import Data.String
import Data.Typeable
import Data.Type.Pretty
import qualified Data.Text as T
import GHC.Stack (HasCallStack)

-- | A type class for effectful server loops.
--
-- This type class serves as interface for other abstractions, for example /process supervision/
--
-- The methods of this class handle 'Event's 'Request's for 'Pdu' instance.
--
-- Instances can by /index types/ for 'Pdu' family directly, or indirectly via the 'ServerPdu' type family.
--
-- To builder servers serving multiple protocols, use the generic 'Pdu' instances, for which 'EmbedProtocol'
-- instances exist, like 2-,3-,4-, or 5-tuple.
--
-- @since 0.24.1
class Server (a :: Type) (e :: [Type -> Type])
  where
  -- | The value that defines what is required to initiate a 'Server'
  -- loop.
  data Init a e

  -- | The index type of the 'Event's that this server processes.
  -- This is the first parameter to the 'Request' and therefore of
  -- the 'Pdu' family.
  type ServerPdu a :: Type
  type ServerPdu a = a

  -- | Effects of the implementation
  --
  -- @since 0.24.1
  type Effects a e :: [Type -> Type]
  type Effects a e = e

  -- | Return the 'ProcessTitle'.
  --
  -- Usually you should rely on the default implementation
  serverTitle :: Init a e -> ProcessTitle

  default serverTitle :: Typeable (ServerPdu a) => Init a e -> ProcessTitle
  serverTitle _ = fromString $ prettyTypeableShows (typeRep (Proxy @(ServerPdu a))) "-server"

  -- | Process the effects of the implementation
  runEffects :: Init a e -> Eff (Effects a e) x -> Eff e x

  default runEffects :: Effects a e ~ e => Init a e -> Eff (Effects a e) x -> Eff e x
  runEffects = const id

  -- | Update the 'Model' based on the 'Event'.
  onEvent :: Init a e -> Event (ServerPdu a) -> Eff (Effects a e) ()

  default onEvent :: (Show (Init a e),  Member Logs (Effects a e)) => Init a e -> Event (ServerPdu a) -> Eff (Effects a e) ()
  onEvent i e = logInfo ("unhandled: " <> T.pack (show i) <> " " <> T.pack (show e))


-- | Execute the server loop.
--
-- @since 0.24.0
start
  :: forall a q h
  . ( Server a (InterruptableProcess q)
    , Typeable a
    , Typeable (ServerPdu a)
    , LogsTo h (InterruptableProcess q)
    , SetMember Process (Process q) (Effects a (InterruptableProcess q))
    , Member Interrupts             (Effects a (InterruptableProcess q))
    , HasCallStack)
  => Init a (InterruptableProcess q)
  -> Eff (InterruptableProcess q) (Endpoint (ServerPdu a))
start a = asEndpoint <$> spawn (serverTitle a) (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
startLink
  :: forall a q h
  . ( Typeable a
    , Typeable (ServerPdu a)
    , Server a (InterruptableProcess q)
    , LogsTo h (InterruptableProcess q)
    , SetMember Process (Process q) (Effects a (InterruptableProcess q))
    , Member Interrupts (Effects a (InterruptableProcess q))
    , HasCallStack)
  => Init a (InterruptableProcess q)
  -> Eff (InterruptableProcess q) (Endpoint (ServerPdu a))
startLink a = asEndpoint <$> spawnLink (serverTitle a) (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
protocolServerLoop
     :: forall q h a
     . ( Server a (InterruptableProcess q)
       , LogsTo h (InterruptableProcess q)
       , SetMember Process (Process q) (Effects a (InterruptableProcess q))
       , Member Interrupts (Effects a (InterruptableProcess q))
       , Typeable a
       , Typeable (ServerPdu a)
       )
  => Init a (InterruptableProcess q) -> Eff (InterruptableProcess q) ()
protocolServerLoop a = do
  myEp <- T.pack . show . asEndpoint @(ServerPdu a) <$> self
  censorLogs (lmAddEp myEp) $ do
    logDebug ("starting")
    runEffects a (receiveSelectedLoop sel mainLoop)
    return ()
  where
    lmAddEp myEp = lmProcessId ?~ myEp
    sel :: MessageSelector (Event (ServerPdu a))
    sel = onRequest <$> selectMessage @(Request (ServerPdu a))
      <|> OnDown    <$> selectMessage @ProcessDown
      <|> OnTimeOut <$> selectMessage @TimerElapsed
      <|> OnMessage <$> selectAnyMessage
      where
        onRequest :: Request (ServerPdu a) -> Event (ServerPdu a)
        onRequest (Call o m) = OnCall (MkSerializer toStrictDynamic) o m
        onRequest (Cast m) = OnCast m
    handleInt i = onEvent a (OnInterrupt i) *> pure Nothing
    mainLoop :: (Typeable a)
      => Either (Interrupt 'Recoverable) (Event (ServerPdu a))
      -> Eff (Effects a (InterruptableProcess q)) (Maybe ())
    mainLoop (Left i) = handleInt i
    mainLoop (Right i) = onEvent a i *> pure Nothing

-- | Internal protocol to communicate incoming messages and other events to the
-- instances of 'Server'.
--
-- Note that this is required to receive any kind of messages in 'protocolServerLoop'.
--
-- @since 0.24.0
data Event a where
  -- | A 'Synchronous' message was received. If an implementation wants to delegate nested 'Pdu's, it can
  -- 'contramap' the reply 'Serializer' such that the 'Reply' received by the caller has the correct type.
  --
  -- @since 0.24.1
  OnCall :: forall a r. (Tangible r, TangiblePdu a ('Synchronous r)) => Serializer (Reply a r) -> RequestOrigin a r -> Pdu a ('Synchronous r) -> Event a
  OnCast :: forall a. TangiblePdu a 'Asynchronous => Pdu a 'Asynchronous -> Event a
  OnInterrupt  :: (Interrupt 'Recoverable) -> Event a
  OnDown  :: ProcessDown -> Event a
  OnTimeOut  :: TimerElapsed -> Event a
  OnMessage  :: StrictDynamic -> Event a
  deriving Typeable

instance Show (Event a) where
  showsPrec d e =
    showParen (d>=10) $
      showString "event: "
      . case e of
          OnCall _ o p -> shows (Call o p)
          OnCast p -> shows (Cast p)
          OnInterrupt r -> shows r
          OnDown r -> shows r
          OnTimeOut r -> shows r
          OnMessage r -> shows r

instance NFData a => NFData (Event a) where
   rnf = \case
       OnCall _ o p -> rnf o `seq` rnf p
       OnCast p -> rnf p
       OnInterrupt r -> rnf r
       OnDown r  -> rnf r
       OnTimeOut r -> rnf r
       OnMessage r -> r `seq` ()

type instance ToPretty (Event a) = ToPretty a <+> PutStr "event"

-- * GenServer

-- | Make a 'Server' from a /data record/ instead of type-class instance.
--
-- Sometimes it is much more concise to create an inline server-loop. In those cases
-- it might not be practical to go through all this type class boilerplate.
--
-- In these cases specifying a server by from a set of callback functions seems
-- much more appropriate.
--
-- This is such a helper. The @GenServer@ is a record with to callbacks,
-- and a 'Server' instance that simply invokes the given callbacks.
--
-- 'Server's that are directly based on 'LogIo' and 'InterruptableProcess' effects.
--
-- The name prefix @Gen@ indicates the inspiration from Erlang's @gen_server@ module.
--
-- @since 0.24.1
data GenServer tag eLoop e where
  MkGenServer
    :: (TangibleGenServer tag eLoop e, HasCallStack) =>
    { genServerRunEffects :: forall x . (Eff eLoop x -> Eff (InterruptableProcess e) x)
    , genServerOnEvent :: Event tag -> Eff eLoop ()
    } -> GenServer tag eLoop e
  deriving Typeable

-- | The constraints for a /tangible/ 'GenServer' instance.
--
-- @since 0.24.1
type TangibleGenServer tag eLoop e =
       ( LogIo e
       , SetMember Process (Process e) eLoop
       , Member Interrupts eLoop
       , Typeable e
       , Typeable eLoop
       , Typeable tag
       )

-- | The name/id of a 'GenServer' for logging purposes.
--
-- @since 0.24.0
newtype GenServerId tag =
  MkGenServerId { _fromGenServerId :: T.Text }
  deriving (Typeable, NFData, Ord, Eq, IsString)

instance (Typeable k, Typeable (tag :: k)) => Show (GenServerId tag) where
  showsPrec d px@(MkGenServerId x) =
    showParen
      (d >= 10)
      (showString (T.unpack x)
      . showString " :: "
      . prettyTypeableShows (typeOf px)
      )

instance (TangibleGenServer tag eLoop e) => Server (GenServer (tag :: Type) eLoop e) (InterruptableProcess e) where
  type ServerPdu (GenServer tag eLoop e) = tag
  type Effects (GenServer tag eLoop e) (InterruptableProcess e) = eLoop
  data instance Init (GenServer tag eLoop e) (InterruptableProcess e) =
        GenServerInit
         { genServerCallbacks :: GenServer tag eLoop e
         , genServerId :: GenServerId tag
         } deriving Typeable
  runEffects (GenServerInit cb cId) m =
    censorLogs
      (lmMessage <>~ (" | " <> _fromGenServerId cId))
      (genServerRunEffects cb m)
  onEvent (GenServerInit cb _cId) req = genServerOnEvent cb req

instance NFData (Init (GenServer tag eLoop e) (InterruptableProcess e)) where
  rnf (GenServerInit _ x) = rnf x

instance Typeable tag => Show (Init (GenServer tag eLoop e) (InterruptableProcess e)) where
  showsPrec d (GenServerInit _ x) =
    showParen (d>=10)
      ( showsPrec 11 x
      . showChar ' ' . prettyTypeableShows (typeRep (Proxy @tag))
      . showString " gen-server"
      )

-- | Create a 'GenServer'.
--
-- This requires the callback for 'Event's, a initial 'Model' and a 'GenServerId'.
--
-- There must be a 'GenServerProtocol' instance.
--
-- This is Haskell, so if this functions is partially applied
-- to some 'Event' callback, you get a function back,
-- that generates 'Init's from 'GenServerId's, like a /factory/
--
-- @since 0.24.0
genServer
  :: forall tag eLoop e .
     ( HasCallStack
     , TangibleGenServer tag eLoop e
     , Server (GenServer tag eLoop e) (InterruptableProcess e)
     )
  => (forall x . GenServerId tag -> Eff eLoop x -> Eff (InterruptableProcess e) x)
  -> (GenServerId tag -> Event tag -> Eff eLoop ())
  -> GenServerId tag
  -> Init (GenServer tag eLoop e) (InterruptableProcess e)
genServer initCb stepCb i =
  GenServerInit
    { genServerId = i
    , genServerCallbacks =
        MkGenServer { genServerRunEffects = initCb i
                    , genServerOnEvent = stepCb i
                    }
    }
