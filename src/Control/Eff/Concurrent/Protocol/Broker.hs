{-# LANGUAGE UndecidableInstances #-}
-- | A process broker spawns and monitors child processes.
--
-- The child processes are mapped to symbolic identifier values: Child-IDs.
--
-- This is the barest, most minimal version of a broker. Children
-- can be started, but not restarted.
--
-- Children can efficiently be looked-up by an id-value, and when
-- the broker is shutdown, all children will be shutdown these
-- are actually all the features of this broker implementation.
--
-- Also, this minimalist broker only knows how to spawn a
-- single kind of child process.
--
-- When a broker spawns a new child process, it expects the
-- child process to return a 'ProcessId'. The broker will
-- 'monitor' the child process.
--
-- This is in stark contrast to how Erlang/OTP handles things;
-- In the OTP Supervisor, the child has to link to the parent.
-- This allows the child spec to be more flexible in that no
-- @pid@ has to be passed from the child start function to the
-- broker process, and also, a child may break free from
-- the broker by unlinking.
--
-- Now while this seems nice at first, this might actually
-- cause surprising results, since it is usually expected that
-- stopping a broker also stops the children, or that a child
-- exit shows up in the logging originating from the former
-- broker.
--
-- The approach here is to allow any child to link to the
-- broker to realize when the broker was violently killed,
-- and otherwise give the child no chance to unlink itself from
-- its broker.
--
-- This module is far simpler than the Erlang/OTP counter part,
-- of a @simple_one_for_one@ supervisor.
--
-- The future of this broker might not be a-lot more than
-- it currently is. The ability to restart processes might be
-- implemented outside of this broker module.
--
-- One way to do that is to implement the restart logic in
-- a separate module, since the child-id can be reused when a child
-- exits.
--
-- @since 0.23.0
module Control.Eff.Concurrent.Protocol.Broker
  ( startLink
  , statefulChild
  , stopBroker
  , isBrokerAlive
  , monitorBroker
  , getDiagnosticInfo
  , spawnChild
  , spawnOrLookup
  , lookupChild
  , callById
  , castById
  , stopChild
  , ChildNotFound(..)
  , Broker()
  , Pdu(StartC, StopC, LookupC, GetDiagnosticInfo)
  , ChildId
  , Stateful.StartArgument(MkBrokerConfig)
  , brokerConfigChildStopTimeout
  , SpawnErr(AlreadyStarted)
  , ChildEvent(OnChildSpawned, OnChildDown, OnBrokerShuttingDown)
  ) where

import Control.DeepSeq (NFData(rnf))
import Control.Eff as Eff
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Client
import Control.Eff.Concurrent.Protocol.Wrapper
import Control.Eff.Concurrent.Protocol.Observer
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import Control.Eff.Concurrent.Protocol.Broker.InternalState
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Extend (raise)
import Control.Eff.Log
import Control.Eff.State.Strict as Eff
import Control.Lens hiding ((.=), use)
import Data.Default
import Data.Dynamic
import Data.Foldable
import Data.Kind
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Type.Pretty
import GHC.Generics (Generic)
import GHC.Stack
import Control.Applicative ((<|>))

-- * Broker Server API

-- ** Functions

-- | Start and link a new broker process with the given 'SpawnFun'unction.
--
-- To spawn new child processes use 'spawnChild'.
--
-- @since 0.23.0
startLink
  :: forall p e
  . ( HasCallStack
    , IoLogging (Processes e)
    , TangibleBroker p
    , Stateful.Server (Broker p) (Processes e)
    )
  => Stateful.StartArgument (Broker p)
  -> Eff (Processes e) (Endpoint (Broker p))
startLink = Stateful.startLink

-- | A smart constructor for 'MkBrokerConfig' that makes it easy to start a 'Stateful.Server' instance.
--
-- The user needs to instantiate @'ChildId' p@.
--
-- @since 0.30.0
statefulChild
  :: forall p e
  . ( HasCallStack
    , IoLogging e
    , TangibleBroker (Stateful.Stateful p)
    , Stateful.Server (Broker (Stateful.Stateful p)) e
    )
  => Timeout
  -> (ChildId p -> Stateful.StartArgument p)
  -> Stateful.StartArgument (Broker (Stateful.Stateful p))
statefulChild t f = MkBrokerConfig t (Stateful.Init . f)

-- | Stop the broker and shutdown all processes.
--
-- Block until the broker has finished.
--
-- @since 0.23.0
stopBroker
  :: ( HasCallStack
     , HasProcesses e q
     , Member Logs e
     , Lifted IO e
     , TangibleBroker p
     )
  => Endpoint (Broker p)
  -> Eff e ()
stopBroker ep = do
  logInfo "stopping broker: " ep
  mr <- monitor (_fromEndpoint ep)
  sendInterrupt (_fromEndpoint ep) NormalExitRequested
  r <- receiveSelectedMessage (selectProcessDown mr)
  logInfo "broker stopped: " ep " " (show r)

-- | Check if a broker process is still alive.
--
-- @since 0.23.0
isBrokerAlive
  :: forall p q0 e .
     ( HasCallStack
     , Member Logs e
     , Typeable p
     , HasProcesses e q0
     )
  => Endpoint (Broker p)
  -> Eff e Bool
isBrokerAlive x = isProcessAlive (_fromEndpoint x)

-- | Monitor a broker process.
--
-- @since 0.23.0
monitorBroker
  :: forall p q0 e .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleBroker p
     )
  => Endpoint (Broker p)
  -> Eff e MonitorReference
monitorBroker x = monitor (_fromEndpoint x)

-- | Start and monitor a new child process using the 'SpawnFun' passed
-- to 'startLink'.
--
-- @since 0.23.0
spawnChild
  :: forall p q0 e .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleBroker p
     , Typeable (Effectful.ServerPdu p)
     )
  => Endpoint (Broker p)
  -> ChildId p
  -> Eff e (Either (SpawnErr p) (Endpoint (Effectful.ServerPdu p)))
spawnChild ep cId = call ep (StartC cId)

-- | Start and monitor a new child process using the 'SpawnFun' passed
-- to 'startLink'.
--
-- Call 'spawnChild' and unpack the 'Either' result,
-- ignoring the 'AlreadyStarted' error.
--
-- @since 0.29.2
spawnOrLookup
  :: forall p q0 e .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleBroker p
     , Typeable (Effectful.ServerPdu p)
     )
  => Endpoint (Broker p)
  -> ChildId p
  -> Eff e (Endpoint (Effectful.ServerPdu p))
spawnOrLookup supEp cId =
  do res <- spawnChild supEp cId
     pure $
      case res of
        Left (AlreadyStarted _ ep) -> ep
        Right ep -> ep

-- | Lookup the given child-id and return the output value of the 'SpawnFun'
--   if the client process exists.
--
-- @since 0.23.0
lookupChild ::
    forall p e q0 .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleBroker p
     , Typeable (Effectful.ServerPdu p)
     )
  => Endpoint (Broker p)
  -> ChildId p
  -> Eff e (Maybe (Endpoint (Effectful.ServerPdu p)))
lookupChild ep cId = call ep (LookupC @p cId)

-- | Stop a child process, and block until the child has exited.
--
-- Return 'True' if a process with that ID was found, 'False' if no process
-- with the given ID was running.
--
-- @since 0.23.0
stopChild ::
    forall p e q0 .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleBroker p
     )
  => Endpoint (Broker p)
  -> ChildId p
  -> Eff e Bool
stopChild ep cId = call ep (StopC @p cId (TimeoutMicros 4000000))

callById ::
    forall destination protocol result e q0 .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , Lifted IO e
     , Lifted IO q0
     , TangibleBroker protocol
     , TangiblePdu destination ( 'Synchronous result)
     , TangiblePdu protocol ( 'Synchronous result)
     , Embeds (Effectful.ServerPdu destination) protocol
     , Ord (ChildId destination)
     , Tangible (ChildId destination)
     , Typeable (Effectful.ServerPdu destination)
     , Tangible result
     , NFData (Pdu protocol ( 'Synchronous result))
     , NFData (Pdu (Effectful.ServerPdu destination) ( 'Synchronous result))
     , Show (Pdu (Effectful.ServerPdu destination) ( 'Synchronous result))
     )
  => Endpoint (Broker destination)
  -> ChildId destination
  -> Pdu protocol ( 'Synchronous result)
  -> Timeout
  -> Eff e result
callById broker cId pdu tMax =
  lookupChild broker cId
   >>=
    maybe
      (do logError "callById failed for: " (show pdu)
          interrupt (InterruptedBy (ChildNotFound cId broker))
      )
      (\cEp -> callWithTimeout cEp pdu tMax)

data ChildNotFound child where
  ChildNotFound :: ChildId child -> Endpoint (Broker child) -> ChildNotFound child
  deriving (Typeable)

instance (Show (ChildId child), Typeable child) => Show (ChildNotFound child) where
  showsPrec d (ChildNotFound cId broker)
    = showParen (d >= 10) ( showString "child not found: "
                           . showsPrec 10 cId
                           . showString " at: "
                           . showsPrec 10 broker
                           )

instance NFData (ChildId c) => NFData (ChildNotFound c) where
  rnf (ChildNotFound cId broker) = rnf cId `seq` rnf broker `seq` ()

castById ::
    forall destination protocol e q0 .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleBroker protocol
     , TangiblePdu destination 'Asynchronous
     , TangiblePdu protocol 'Asynchronous
     )
  => Endpoint (Broker destination)
  -> ChildId destination
  -> Pdu protocol 'Asynchronous
  -> Eff e ()
castById = error "TODO"


-- | Return a 'Text' describing the current state of the broker.
--
-- @since 0.23.0
getDiagnosticInfo
  :: forall p e q0 .
     ( HasCallStack
     , HasProcesses e q0
     , TangibleBroker p
     )
  => Endpoint (Broker p)
  -> Eff e Text
getDiagnosticInfo s = call s (GetDiagnosticInfo @p)

-- ** Types

-- | The index type of 'Server' supervisors.
--
-- A @'Broker' p@ manages the life cycle of the processes, running the @'Server' p@
-- methods of that specific type.
--
-- The broker maps an identifier value of type @'ChildId' p@ to an @'Endpoint' p@.
--
-- @since 0.24.0
data Broker (p :: Type) deriving Typeable

instance Typeable p => HasPdu (Broker p) where
  type instance EmbeddedPduList (Broker p) = '[ObserverRegistry (ChildEvent p)]
  -- | The 'Pdu' instance contains methods to start, stop and lookup a child
  -- process, as well as a diagnostic callback.
  --
  -- @since 0.23.0
  data instance  Pdu (Broker p) r where
          StartC :: ChildId p -> Pdu (Broker p) ('Synchronous (Either (SpawnErr p) (Endpoint (Effectful.ServerPdu p))))
          StopC :: ChildId p -> Timeout -> Pdu (Broker p) ('Synchronous Bool)
          LookupC :: ChildId p -> Pdu (Broker p) ('Synchronous (Maybe (Endpoint (Effectful.ServerPdu p))))
          GetDiagnosticInfo :: Pdu (Broker p) ('Synchronous Text)
          ChildEventObserverRegistry :: Pdu (ObserverRegistry (ChildEvent p)) r -> Pdu (Broker p) r
      deriving Typeable

instance (Typeable p, Show (ChildId p)) => Show (Pdu (Broker p) r) where
  showsPrec d (StartC c) = showParen (d >= 10) (showString "StartC " . showsPrec 10 c)
  showsPrec d (StopC c t) = showParen (d >= 10) (showString "StopC " . showsPrec 10 c . showChar ' ' . showsPrec 10 t)
  showsPrec d (LookupC c) = showParen (d >= 10) (showString "LookupC " . showsPrec 10 c)
  showsPrec _ GetDiagnosticInfo = showString "GetDiagnosticInfo"
  showsPrec d (ChildEventObserverRegistry c) = showParen (d >= 10) (showString "ChildEventObserverRegistry " . showsPrec 10 c)

instance (NFData (ChildId p)) => NFData (Pdu (Broker p) r) where
  rnf (StartC ci) = rnf ci
  rnf (StopC ci t) = rnf ci `seq` rnf t
  rnf (LookupC ci) = rnf ci
  rnf GetDiagnosticInfo = ()
  rnf (ChildEventObserverRegistry x) = rnf x

instance Typeable p => HasPduPrism (Broker p) (ObserverRegistry (ChildEvent p)) where
 embedPdu = ChildEventObserverRegistry
 fromPdu (ChildEventObserverRegistry x) = Just x
 fromPdu _ = Nothing

type instance ToPretty (Broker p) = "broker" <:> ToPretty p

-- | The event type to indicate that a child was started or stopped.
--
-- The need for this type originated for the watchdog functionality introduced
-- in 0.30.0.
-- The watch dog shall restart a crashed child, and in order to do so, it
-- must somehow monitor the child.
-- Since no order is specified in which processes get the 'ProcessDown' events,
-- a watchdog cannot monitor a child and restart it immediately, because it might
-- have received the process down event before the broker.
-- So instead the watchdog can simply use the broker events, and monitor only the
-- broker process.
--
-- @since 0.30.0
data ChildEvent p where
  OnChildSpawned :: Endpoint (Broker p) -> ChildId p -> Endpoint (Effectful.ServerPdu p) -> ChildEvent p
  OnChildDown :: Endpoint (Broker p) -> ChildId p -> Endpoint (Effectful.ServerPdu p) -> Interrupt 'NoRecovery -> ChildEvent p
  OnBrokerShuttingDown :: Endpoint (Broker p) -> ChildEvent p -- ^ The broker is shutting down and will soon begin stopping/killing its children
  deriving (Typeable, Generic)

instance (NFData (ChildId p)) => NFData (ChildEvent p)

instance (Typeable p, Typeable (Effectful.ServerPdu p), Show (ChildId p)) => Show (ChildEvent p) where
  showsPrec d x =
    case x of
     OnChildSpawned s i e ->
        showParen (d >= 10)
          (shows s . showString ": child-spawned: " . shows i . showChar ' ' . shows e)
     OnChildDown s i e r ->
        showParen (d >= 10)
          (shows s . showString ": child-down: " . shows i . showChar ' ' . shows e . showChar ' ' . showsPrec 10 r)
     OnBrokerShuttingDown s ->
        shows s . showString ": shutting down"

-- | The type of value used to index running 'Server' processes managed by a 'Broker'.
--
-- Note, that the type you provide must be 'Tangible'.
--
-- @since 0.24.0
type family ChildId p

type instance ChildId (Stateful.Stateful p) = ChildId p

-- | Constraints on the parameters to 'Broker'.
--
-- @since 0.24.0
type TangibleBroker p =
  ( Tangible (ChildId p)
  , Ord (ChildId p)
  , Typeable p
  )


instance
  ( IoLogging q
  , TangibleBroker p
  , Tangible (ChildId p)
  , Typeable (Effectful.ServerPdu p)
  , Effectful.Server p (Processes q)
  , HasProcesses (Effectful.ServerEffects p (Processes q)) q
  ) => Stateful.Server (Broker p) (Processes q) where

  -- | Options that control the 'Broker p' process.
  --
  -- This contains:
  --
  -- * a 'SpawnFun'
  -- * the 'Timeout' after requesting a normal child exit before brutally killing the child.
  --
  -- @since 0.24.0
  data instance StartArgument (Broker p) = MkBrokerConfig
    {
      brokerConfigChildStopTimeout :: Timeout
    , brokerConfigStartFun :: ChildId p -> Effectful.Init p
    }

  data instance Model (Broker p) =
    BrokerModel { _children :: Children (ChildId p) p
                , _childEventObserver :: ObserverRegistry (ChildEvent p)
                }
      deriving Typeable

  setup _ _cfg = pure (BrokerModel def emptyObserverRegistry, ())
  update _ _brokerConfig (Stateful.OnCast req) =
    case  req of
      ChildEventObserverRegistry x ->
        Stateful.zoomModel @(Broker p) childEventObserverLens (observerRegistryHandlePdu x)

  update me brokerConfig (Stateful.OnCall rt req) =
    case req of
      ChildEventObserverRegistry x ->
        logEmergency "unreachable: " x

      GetDiagnosticInfo -> zoomToChildren @p $ do
        p <- (pack . show <$> getChildren @(ChildId p) @p)
        sendReply rt p

      LookupC i -> zoomToChildren @p $ do
        p <- fmap (view childEndpoint) <$> lookupChildById @(ChildId p) @p i
        sendReply rt p

      StopC i t -> zoomToChildren @p $ do
        mExisting <- lookupAndRemoveChildById @(ChildId p) @p i
        case mExisting of
          Nothing -> sendReply rt False
          Just existingChild -> do
            reason <- stopOrKillChild i existingChild t
            sendReply rt True
            Stateful.zoomModel @(Broker p)
              childEventObserverLens
              (observerRegistryNotify @(ChildEvent p) (OnChildDown me i (existingChild^.childEndpoint) reason))

      StartC i -> do
        mExisting <- zoomToChildren @p $ lookupChildById @(ChildId p) @p i
        case mExisting of
          Nothing -> do
            childEp <- raise (raise (Effectful.startLink (brokerConfigStartFun brokerConfig i)))
            let childPid = _fromEndpoint childEp
            cMon <- monitor childPid
            zoomToChildren @p $ putChild i (MkChild @p childEp cMon)
            sendReply rt (Right childEp)
            Stateful.zoomModel @(Broker p)
              childEventObserverLens
              (observerRegistryNotify @(ChildEvent p) (OnChildSpawned me i childEp))
          Just existingChild ->
            sendReply rt (Left (AlreadyStarted i (existingChild ^. childEndpoint)))

  update me _brokerConfig (Stateful.OnDown pd) = do
      wasObserver <- Stateful.zoomModel @(Broker p)
        childEventObserverLens
        (observerRegistryRemoveProcess @(ChildEvent p) (downProcess pd))
      if wasObserver
       then logInfo "observer process died: " pd
       else do
        oldEntry <- zoomToChildren @p $ lookupAndRemoveChildByMonitor @(ChildId p) @p (downReference pd)
        case oldEntry of
          Nothing -> logWarning "unexpected: " pd
          Just (i, c) -> do
            logInfo pd " for child " i " => " (c^.childEndpoint)
            Stateful.zoomModel @(Broker p)
                childEventObserverLens
                (observerRegistryNotify @(ChildEvent p) (OnChildDown me i (c^.childEndpoint) (downReason pd)))
  update me brokerConfig (Stateful.OnInterrupt e) =
    case e of
      NormalExitRequested -> do
        logDebug "broker stopping: " e
        Stateful.zoomModel @(Broker p)
            childEventObserverLens
            (observerRegistryNotify @(ChildEvent p) (OnBrokerShuttingDown me))
        stopAllChildren @p me (brokerConfigChildStopTimeout brokerConfig)
        exitNormally
      LinkedProcessCrashed linked ->
        logNotice (pack (show linked))
      _ -> do
        logWarning "broker interrupted: " e
        Stateful.zoomModel @(Broker p)
            childEventObserverLens
            (observerRegistryNotify @(ChildEvent p) (OnBrokerShuttingDown me))
        stopAllChildren @p me (brokerConfigChildStopTimeout brokerConfig)
        exitBecause (interruptToExit e)

  update _ _brokerConfig o = logWarning "unexpected: " o


zoomToChildren :: forall p c e
  . Member (Stateful.ModelState (Broker p)) e
  => Eff (State (Children (ChildId p) p) ': e) c
  -> Eff e c
zoomToChildren = Stateful.zoomModel @(Broker p) childrenLens

childrenLens :: Lens' (Stateful.Model (Broker p)) (Children (ChildId p) p)
childrenLens = lens _children (\(BrokerModel _ o) c -> BrokerModel c o)

childEventObserverLens :: Lens' (Stateful.Model (Broker p)) (ObserverRegistry (ChildEvent p))
childEventObserverLens = lens _childEventObserver (\(BrokerModel o _) c -> BrokerModel o c)

-- | Runtime-Errors occurring when spawning child-processes.
--
-- @since 0.23.0
data SpawnErr p = AlreadyStarted (ChildId p) (Endpoint (Effectful.ServerPdu p))
  deriving (Typeable, Generic)

deriving instance Eq (ChildId p) => Eq (SpawnErr p)
deriving instance Ord (ChildId p) => Ord (SpawnErr p)
deriving instance (Typeable (Effectful.ServerPdu p), Show (ChildId p)) => Show (SpawnErr p)

instance NFData (ChildId p) => NFData (SpawnErr p)

-- Internal Functions

stopOrKillChild
  :: forall p e q0 .
     ( HasCallStack
     , HasProcesses e q0
     , Lifted IO e
     , Lifted IO q0
     , Member Logs e
     , Member (Stateful.ModelState (Broker p)) e
     , TangibleBroker p
     , ToLogMsg (ChildId p)
     , Typeable (Effectful.ServerPdu p)
     )
  => ChildId p
  -> Child p
  -> Timeout
  -> Eff e (Interrupt 'NoRecovery)
stopOrKillChild cId c stopTimeout =
      do
        broker <- asEndpoint @(Broker p) <$> self
        t <- startTimerWithTitle
                (MkProcessTitle (pack "child-exit-timer-" <> pack (show broker) <> pack "-" <> pack (show cId)))
                stopTimeout
        sendInterrupt (_fromEndpoint (c^.childEndpoint)) NormalExitRequested
        r1 <- receiveSelectedMessage (   Right <$> selectProcessDown (c^.childMonitoring)
                                     <|> Left  <$> selectTimerElapsed t )
        demonitor (c^.childMonitoring)
        unlinkProcess (_fromEndpoint (c^.childEndpoint))
        case r1 of
          Left timerElapsed -> do
            logWarning (show timerElapsed) ": child " cId " => " (c^.childEndpoint) " did not shutdown in time"
            let reason = interruptToExit (TimeoutInterrupt
                                           ("child did not shut down in time and was terminated by the "
                                             ++ show broker))
            sendShutdown (_fromEndpoint (c^.childEndpoint)) reason
            return reason
          Right downMsg -> do
            logInfo "child " cId " => " (c^.childEndpoint) " terminated: " (downReason downMsg)
            return (downReason downMsg)

stopAllChildren
  :: forall p e q0 .
     ( HasCallStack
     , HasProcesses e q0
     , Lifted IO e
     , Lifted IO q0
     , Member Logs e
     , Member (Stateful.ModelState (Broker p)) e
     , TangibleBroker p
     , Typeable (Effectful.ServerPdu p)
     , ToLogMsg (ChildId p)
     , ToLogMsg (Child p)
     )
  => Endpoint (Broker p) -> Timeout -> Eff e ()
stopAllChildren me stopTimeout =
  zoomToChildren @p
    (removeAllChildren @(ChildId p) @p)
  >>= pure . Map.assocs
  >>= traverse_ killAndNotify
  where
    killAndNotify (cId, c) = do
      reason <- provideInterrupts (stopOrKillChild cId c stopTimeout) >>= either crash return
      Stateful.zoomModel @(Broker p)
          childEventObserverLens
          (observerRegistryNotify @(ChildEvent p) (OnChildDown me cId (c^.childEndpoint) reason))

      where
        crash e = do
          logError e " while stopping child: " cId " " c
          return (interruptToExit e)
