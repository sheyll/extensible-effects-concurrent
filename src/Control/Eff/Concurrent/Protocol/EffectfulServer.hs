-- | Utilities to implement /effectful server-loops/.
--
-- @since 0.24.0
module Control.Eff.Concurrent.Protocol.EffectfulServer
  ( Server(..)
  , Event(..)
  , start
  , startLink
  , protocolServerLoop
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
  type ServerEffects a e :: [Type -> Type]
  type ServerEffects a e = e

  -- | Return the 'ProcessTitle'.
  --
  -- Usually you should rely on the default implementation
  serverTitle :: Init a e -> ProcessTitle

  default serverTitle :: Typeable (ServerPdu a) => Init a e -> ProcessTitle
  serverTitle _ = fromString $ prettyTypeableShows (typeRep (Proxy @(ServerPdu a))) "-server"

  -- | Process the effects of the implementation
  runEffects :: Endpoint (ServerPdu a) -> Init a e -> Eff (ServerEffects a e) x -> Eff e x

  default runEffects :: ServerEffects a e ~ e => Endpoint (ServerPdu a) -> Init a e -> Eff (ServerEffects a e) x -> Eff e x
  runEffects _ = const id

  -- | Update the 'Model' based on the 'Event'.
  onEvent :: Endpoint (ServerPdu a) -> Init a e -> Event (ServerPdu a) -> Eff (ServerEffects a e) ()

  default onEvent :: (Show (Init a e),  Member Logs (ServerEffects a e)) => Endpoint (ServerPdu a) -> Init a e -> Event (ServerPdu a) -> Eff (ServerEffects a e) ()
  onEvent _ i e = logInfo ("unhandled: " <> T.pack (show i) <> " " <> T.pack (show e))


-- | Execute the server loop.
--
-- @since 0.24.0
start
  :: forall a q h
  . ( Server a (Processes q)
    , Typeable a
    , Typeable (ServerPdu a)
    , LogsTo h (Processes q)
    , SetMember Process (Process q) (ServerEffects a (Processes q))
    , Member Interrupts             (ServerEffects a (Processes q))
    , HasCallStack)
  => Init a (Processes q)
  -> Eff (Processes q) (Endpoint (ServerPdu a))
start a = asEndpoint <$> spawn (serverTitle a) (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
startLink
  :: forall a q h
  . ( Typeable a
    , Typeable (ServerPdu a)
    , Server a (Processes q)
    , LogsTo h (Processes q)
    , SetMember Process (Process q) (ServerEffects a (Processes q))
    , Member Interrupts (ServerEffects a (Processes q))
    , HasCallStack)
  => Init a (Processes q)
  -> Eff (Processes q) (Endpoint (ServerPdu a))
startLink a = asEndpoint <$> spawnLink (serverTitle a) (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
protocolServerLoop
     :: forall q h a
     . ( Server a (Processes q)
       , LogsTo h (Processes q)
       , SetMember Process (Process q) (ServerEffects a (Processes q))
       , Member Interrupts (ServerEffects a (Processes q))
       , Typeable a
       , Typeable (ServerPdu a)
       )
  => Init a (Processes q) -> Eff (Processes q) ()
protocolServerLoop a = do
  myEp <- asEndpoint @(ServerPdu a) <$> self
  let myEpTxt = T.pack . show $ myEp
  censorLogs (lmAddEp myEpTxt) $ do
    logDebug ("starting")
    runEffects  myEp a (receiveSelectedLoop sel (mainLoop myEp))
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
        onRequest (Call o m) = OnCall (replyTarget (MkSerializer toStrictDynamic) o) m
        onRequest (Cast m) = OnCast m
    handleInt myEp i = onEvent myEp a (OnInterrupt i) *> pure Nothing
    mainLoop :: (Typeable a)
      => Endpoint (ServerPdu a)
      -> Either (Interrupt 'Recoverable) (Event (ServerPdu a))
      -> Eff (ServerEffects a (Processes q)) (Maybe ())
    mainLoop myEp (Left i) = handleInt myEp i
    mainLoop myEp (Right i) = onEvent myEp a i *> pure Nothing

-- | This event sum-type is used to communicate incoming messages and other events to the
-- instances of 'Server'.
--
-- @since 0.24.0
data Event a where
  -- | A 'Synchronous' message was received. If an implementation wants to delegate nested 'Pdu's, it can
  -- use 'toEmbeddedReplyTarget' to convert a 'ReplyTarget' safely to the embedded protocol.
  --
  -- @since 0.24.1
  OnCall :: forall a r. (Tangible r, TangiblePdu a ('Synchronous r)) => ReplyTarget a r -> Pdu a ('Synchronous r) -> Event a
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
          OnCall o p -> shows (Call (view replyTargetOrigin o) p)
          OnCast p -> shows (Cast p)
          OnInterrupt r -> shows r
          OnDown r -> shows r
          OnTimeOut r -> shows r
          OnMessage r -> shows r

instance NFData a => NFData (Event a) where
   rnf = \case
       OnCall o p -> rnf o `seq` rnf p
       OnCast p -> rnf p
       OnInterrupt r -> rnf r
       OnDown r  -> rnf r
       OnTimeOut r -> rnf r
       OnMessage r -> r `seq` ()

type instance ToPretty (Event a) = ToPretty a <+> PutStr "event"
