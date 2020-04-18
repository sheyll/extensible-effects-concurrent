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
import Control.Eff.Concurrent.Misc
import Control.Eff.Extend ()
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Wrapper
import Control.Eff.Log
import Control.Lens
import Data.Kind
import Data.String
import Data.Typeable
import Data.Type.Pretty
import GHC.Stack (HasCallStack)

-- | A type class for effectful server loops.
--
-- This type class serves as interface for other abstractions, for example /process supervision/
--
-- The methods of this class handle 'Event's 'Request's for 'Pdu' instance.
--
-- Instances can by /index types/ for 'Pdu' family directly, or indirectly via the 'ServerPdu' type family.
--
-- To builder servers serving multiple protocols, use the generic 'Pdu' instances, for which 'Embeds'
-- instances exist, like 2-,3-,4-, or 5-tuple.
--
-- @since 0.24.1
class ToTypeLogMsg a => Server (a :: Type) (e :: [Type -> Type])
  where
  -- | The value that defines what is required to initiate a 'Server'
  -- loop.
  data Init a

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
  serverTitle :: Init a -> ProcessTitle

  default serverTitle :: Typeable a => Init a -> ProcessTitle
  serverTitle _ = fromString $ showSTypeable @a ""

  -- | Process the effects of the implementation
  runEffects :: Endpoint (ServerPdu a) -> Init a -> Eff (ServerEffects a e) x -> Eff e x

  default runEffects :: ServerEffects a e ~ e => Endpoint (ServerPdu a) -> Init a -> Eff (ServerEffects a e) x -> Eff e x
  runEffects _ = const id

  -- | Update the 'Model' based on the 'Event'.
  onEvent :: Endpoint (ServerPdu a) -> Init a -> Event (ServerPdu a) -> Eff (ServerEffects a e) ()

  default onEvent :: (Show (Init a),  Member Logs (ServerEffects a e)) => Endpoint (ServerPdu a) -> Init a -> Event (ServerPdu a) -> Eff (ServerEffects a e) ()
  onEvent _ i e = logInfo ("unhandled: " :: String)  (show i) (show e)


-- | Execute the server loop.
--
-- @since 0.24.0
start
  :: forall a r q
  . ( Server a (Processes q)
    , Typeable a
    , Typeable (ServerPdu a)
    , FilteredLogging (Processes q)
    , HasProcesses (ServerEffects a (Processes q)) q
    , HasProcesses r q
    , HasCallStack)
  => Init a
  -> Eff r (Endpoint (ServerPdu a))
start a = asEndpoint <$> spawn (serverTitle @_ @(Processes q) a) (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
startLink
  :: forall a r q
  . ( Typeable a
    , Typeable (ServerPdu a)
    , Server a (Processes q)
    , FilteredLogging (Processes q)
    , HasProcesses (ServerEffects a (Processes q)) q
    , HasProcesses r q
    , HasCallStack)
  => Init a
  -> Eff r (Endpoint (ServerPdu a))
startLink a = asEndpoint <$> spawnLink (serverTitle @_ @(Processes q) a) (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
protocolServerLoop
     :: forall q a
     . ( Server a (Processes q)
       , FilteredLogging (Processes q)
       , HasProcesses (ServerEffects a (Processes q)) q
       , Typeable a
       , Typeable (ServerPdu a)
       )
  => Init a -> Eff (Processes q) ()
protocolServerLoop a = do
  myEp <- asEndpoint @(ServerPdu a) <$> self
  logDebug ("starting" :: String)
  runEffects myEp a (receiveSelectedLoop sel (mainLoop myEp))
  return ()
  where
    sel :: MessageSelector (Event (ServerPdu a))
    sel =  onRequest <$> selectMessage @(Request (ServerPdu a))
       <|> OnDown    <$> selectMessage @ProcessDown
       <|> OnTimeOut <$> selectMessage @TimerElapsed
       <|> OnMessage <$> selectAnyMessage
      where
        onRequest :: Request (ServerPdu a) -> Event (ServerPdu a)
        onRequest (Call o m) = OnCall (replyTarget (MkSerializer toStrictDynamic) o) m
        onRequest (Cast m) = OnCast m
    handleInt myEp i = onEvent @_ @(Processes q) myEp a (OnInterrupt i) *> pure Nothing
    mainLoop :: (Typeable a)
      => Endpoint (ServerPdu a)
      -> Either (Interrupt 'Recoverable) (Event (ServerPdu a))
      -> Eff (ServerEffects a (Processes q)) (Maybe ())
    mainLoop myEp (Left i) = handleInt myEp i
    mainLoop myEp (Right i) = onEvent @_ @(Processes q) myEp a i *> pure Nothing

-- | This event sum-type is used to communicate incoming messages and other events to the
-- instances of 'Server'.
--
-- @since 0.24.0
data Event a where
  -- | A 'Synchronous' message was received. If an implementation wants to delegate nested 'Pdu's, it can
  -- use 'toEmbeddedReplyTarget' to convert a 'ReplyTarget' safely to the embedded protocol.
  --
  -- @since 0.24.1
  OnCall
    :: forall a r.
       ( Tangible r
       , TangiblePdu a ('Synchronous r)
       , ToLogMsg (Pdu a ('Synchronous r))
       )
    => ReplyTarget a r
    -> Pdu a ('Synchronous r)
    -> Event a
  OnCast
    :: forall a.
       ( TangiblePdu a 'Asynchronous
       , ToLogMsg (Pdu a 'Asynchronous)
       )
    => Pdu a 'Asynchronous
    -> Event a
  OnInterrupt :: Interrupt 'Recoverable -> Event a
  OnDown :: ProcessDown -> Event a
  OnTimeOut :: TimerElapsed -> Event a
  OnMessage :: StrictDynamic -> Event a
  deriving Typeable

instance ToLogMsg a => ToLogMsg (Event a) where
  toLogMsg x =
    packLogMsg "event: " <>
    case x of
      OnCall o p -> toLogMsg (Call (view replyTargetOrigin o) p)
      OnCast p -> toLogMsg (Cast p)
      OnInterrupt r -> toLogMsg r
      OnDown r -> toLogMsg r
      OnTimeOut r -> toLogMsg r
      OnMessage r -> packLogMsg "message: " <> packLogMsg (show r)

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
