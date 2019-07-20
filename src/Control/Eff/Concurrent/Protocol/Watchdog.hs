{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NumericUnderscores #-}
-- | Monitor a process and act when it is unresponsive.
--
-- Behaviour of the watchdog:
--
-- When a child crashes:
-- * if the allowed maximum number crashes per time span has been reached for the process,
-- ** cancel all other timers
-- ** don't start the child again
-- ** if this is a /linked/ watchdog crash the watchdog
-- * otherwise
-- ** tell the broker to start the child
-- ** record a crash and start a timer to remove the record later
-- ** monitor the child
--
-- When a child crash timer elapses:
-- * remove the crash record
--
-- @since 0.30.0
module Control.Eff.Concurrent.Protocol.Watchdog
  ( startLink
  , Watchdog
  , attachTemporary
  , attachPermanent
  , getCrashReports
  , CrashRate(..)
  , crashCount
  , crashTimeSpan
  , crashesPerSeconds
  , CrashCount
  , CrashTimeSpan
  , ChildWatch(..)
  , parent
  , crashes
  , ExonerationTimer(..)
  , CrashReport(..)
  , crashTime
  , crashReason
  , exonerationTimerReference
  ) where

import Control.DeepSeq
import Control.Eff (Eff, Member, lift, Lifted)
import Control.Eff.Concurrent.Misc
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Client
import Control.Eff.Concurrent.Protocol.Wrapper
import qualified Control.Eff.Concurrent.Protocol.Observer as Observer
import Control.Eff.Concurrent.Protocol.Observer (Observer)
import qualified Control.Eff.Concurrent.Protocol.Broker as Broker
import Control.Eff.Concurrent.Protocol.Broker (Broker)
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import Control.Lens
import Control.Eff.Log
import Data.Set (Set)
import Data.Typeable
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Kind (Type)
import Data.Default
import Data.Text (pack)
import GHC.Stack (HasCallStack)
import Data.Maybe (isJust)
import Data.Foldable (traverse_)
import Control.Monad (when)


-- | The phantom for watchdog processes, that watch the given type of servers
--
-- This type is used for the 'Effectful.Server' and 'HasPdu' instances.
--
-- @since 0.30.0
data Watchdog (child :: Type) deriving Typeable

-- | Start and link a new watchdog process.
--
-- The watchdog process will register itself to the 'Broker.ChildEvent's and
-- restart crashed children.
--
-- @since 0.30.0
startLink
  :: forall child q e h
  . ( HasCallStack
    , LogsTo h q
    , LogsTo h e
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , HasProcesses e q
    )
  => CrashRate -> Eff e (Endpoint (Watchdog child))
startLink = Stateful.startLink . StartWatchDog

-- | Restart children of the given broker.
--
-- When the broker exits, ignore the children of that broker.
--
-- @since 0.30.0
attachTemporary
  :: forall child q e
  . ( HasCallStack
    , LogIo q
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , HasProcesses e q
    , Lifted IO q
    , Lifted IO e
    , Member Logs e
    )
  => Endpoint (Watchdog child) -> Endpoint (Broker child) -> Eff e ()
attachTemporary wd broker =
  callWithTimeout wd (AttachTemporary broker) (TimeoutMicros 1_000_000)

-- | Restart children of the given broker.
--
-- When the broker exits, the watchdog process will exit, too.
--
-- @since 0.30.0
attachPermanent
  :: forall child q e
  . ( HasCallStack
    , LogIo q
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , HasProcesses e q
    , Lifted IO q
    , Lifted IO e
    , Member Logs e
    )
  => Endpoint (Watchdog child) -> Endpoint (Broker child) -> Eff e ()
attachPermanent wd broker =
  callWithTimeout wd (AttachPermanent broker) (TimeoutMicros 1_000_000)

-- | Return a list of 'CrashReport's.
--
-- Useful for diagnostics
--
-- @since 0.30.0
getCrashReports
  :: forall child q e
  . ( HasCallStack
    , LogIo q
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , HasProcesses e q
    , Lifted IO q
    , Lifted IO e
    , Member Logs e
    )
  => Endpoint (Watchdog child) -> Eff e (Map (Broker.ChildId child) (ChildWatch child))
getCrashReports wd = callWithTimeout wd GetCrashReports (TimeoutMicros 5_000_000)

instance Typeable child => HasPdu (Watchdog child) where
  type instance EmbeddedPduList (Watchdog child) = '[Observer (Broker.ChildEvent child)]
  data Pdu (Watchdog child) r where
    AttachTemporary :: Endpoint (Broker child) -> Pdu (Watchdog child) ('Synchronous ())
    AttachPermanent :: Endpoint (Broker child) -> Pdu (Watchdog child) ('Synchronous ())
    GetCrashReports :: Pdu (Watchdog child) ('Synchronous (Map (Broker.ChildId child) (ChildWatch child)))
    OnChildEvent :: Broker.ChildEvent child -> Pdu (Watchdog child) 'Asynchronous
      deriving Typeable

instance Typeable child => HasPduPrism (Watchdog child) (Observer (Broker.ChildEvent child)) where
  embedPdu (Observer.Observed e) = OnChildEvent e
  fromPdu (OnChildEvent x) = Just (Observer.Observed x)
  fromPdu _ = Nothing

instance (NFData (Broker.ChildId child)) => NFData (Pdu (Watchdog child) r) where
  rnf (AttachTemporary e) = rnf e
  rnf (AttachPermanent e) = rnf e
  rnf GetCrashReports = ()
  rnf (OnChildEvent o) = rnf o

instance
  ( Show (Broker.ChildId child)
  , Typeable child
  , Typeable (Effectful.ServerPdu child)
  )
  => Show (Pdu (Watchdog child) r) where
  showsPrec d (AttachTemporary e) = showParen (d>=10) (showString "attachTemporary: " . shows e)
  showsPrec d (AttachPermanent e) = showParen (d>=10) (showString "attachTemporary-linked: " . shows e)
  showsPrec _ GetCrashReports = showString "get-crash-reports"
  showsPrec d (OnChildEvent o) = showParen (d>=10) (showString "on-child-event: " . showsPrec 10 o)

-- ------------------ Broker Watches

newtype BrokerWatch =
  MkBrokerWatch (Maybe MonitorReference)
  deriving (Default)

instance Show BrokerWatch where
  showsPrec _ (MkBrokerWatch Nothing) = showString "broker-watch"
  showsPrec d (MkBrokerWatch (Just mon)) = showParen (d>=10) (showString "linked-broker-watch " . showsPrec 10 mon)

brokerMonitor :: Iso' BrokerWatch (Maybe MonitorReference)
brokerMonitor = iso (\(MkBrokerWatch m) -> m) MkBrokerWatch

-- --- Server Definition

instance
  ( Typeable child
  , HasPdu (Effectful.ServerPdu child)
  , Tangible (Broker.ChildId child)
  , Ord (Broker.ChildId child)
  , Eq (Broker.ChildId child)
  , LogIo e
  ) => Stateful.Server (Watchdog child) (Processes e) where

  data instance StartArgument (Watchdog child) (Processes e) =
    StartWatchDog { _crashRate :: CrashRate
                  }
      deriving Typeable

  data instance Model (Watchdog child) =
    WatchdogModel { _brokers :: Map (Endpoint (Broker child)) BrokerWatch
                  , _watched :: Map (Broker.ChildId child) (ChildWatch child)
                  }

  update me startArg =
    \case
      Effectful.OnCall rt (AttachTemporary broker) -> do
        logDebug ("attaching to: " <> pack (show broker))
        oldModel <- Stateful.getModel @(Watchdog child)
        traverse_ (\mr -> do
                             logDebug ("stop monitoring " <> pack (show broker) <> " " <> pack (show mr))
                             demonitor mr)
          (oldModel ^? brokers . at broker . _Just . brokerMonitor . _Just)
        let newBrokerWatch = MkBrokerWatch Nothing
        Stateful.modifyModel (brokers . at broker ?~ newBrokerWatch)
        Observer.registerObserver @(Broker.ChildEvent child) broker me
        sendReply rt ()

      Effectful.OnCall rt (AttachPermanent broker) -> do
        logDebug ("attaching and linking to: " <> pack (show broker))
        oldModel <- Stateful.getModel @(Watchdog child)
        let oldMonitor = oldModel ^? brokers . at broker . _Just . brokerMonitor . _Just
        case oldMonitor of
          Nothing -> do
            mr <- monitor (broker^.fromEndpoint)
            logDebug ("monitoring " <> pack (show broker) <> " " <> pack (show mr))
            let newBroker = MkBrokerWatch (Just mr)
            Stateful.modifyModel (brokers . at broker ?~ newBroker)
            Observer.registerObserver @(Broker.ChildEvent child) broker me
          Just mr ->
            logDebug ("already monitoring " <> pack (show broker) <> " - keeping: " <> pack (show mr))
        sendReply rt ()

      Effectful.OnCall rt GetCrashReports ->
        Stateful.useModel @(Watchdog child) watched >>= sendReply rt

      Effectful.OnCast (OnChildEvent e) ->
        case e of
          down@(Broker.OnBrokerShuttingDown broker) -> do
            logInfo ("received: " <> pack (show down))
            currentModel <- Stateful.getModel @(Watchdog child)
            if not (Set.member broker (currentModel ^. brokers . to Map.keysSet))
             then logWarning ("received child event for unknown broker: " <> pack (show down))
             else pure () -- TODO remove all child watches

          spawned@(Broker.OnChildSpawned broker cId _) -> do
            logInfo ("received: " <> pack (show spawned))
            currentModel <- Stateful.getModel @(Watchdog child)
            if not (Set.member broker (currentModel ^. brokers . to Map.keysSet))
             then logWarning ("received child event for unknown broker: " <> pack (show spawned))
             else removeAndCleanChild @child cId

          down@(Broker.OnChildDown broker cId _ ExitNormally) -> do
            logInfo ("received: " <> pack (show down))
            currentModel <- Stateful.getModel @(Watchdog child)
            if not (Set.member broker (currentModel ^. brokers . to Map.keysSet))
             then logWarning ("received child event for unknown broker: " <> pack (show down))
             else removeAndCleanChild @child cId

          down@(Broker.OnChildDown broker cId _ reason) -> do
            logInfo ("received: " <> pack (show down))
            currentModel <- Stateful.getModel @(Watchdog child)
            if not (Set.member broker (currentModel ^. brokers . to Map.keysSet))
             then
              logWarning ("received child event for unknown broker: " <> pack (show down))
             else do
              let recentCrashes = countRecentCrashes broker cId currentModel
                  rate = startArg ^. crashRate
                  maxCrashCount = rate ^. crashCount
              if recentCrashes < maxCrashCount then do
                logNotice ("restarting (" <> pack (show recentCrashes) <> "/" <> pack (show maxCrashCount) <> "): "
                            <> pack (show cId) <> " of " <> pack (show broker))
                res <- Broker.spawnChild broker cId
                logNotice ("restarted: " <> pack (show cId) <> " of "
                            <> pack (show broker) <> ": " <> pack (show res))
                crash <- startExonerationTimer @child cId reason (rate ^. crashTimeSpan)
                if isJust (currentModel ^? childWatchesById cId)
                  then do
                    logDebug ("recording crash for child: " <> pack (show cId) <> " of " <> pack (show broker))
                    Stateful.modifyModel (watched @child . at cId . _Just . crashes %~ Set.insert crash)
                  else do
                    logDebug ("recording crash for new child: " <> pack (show cId) <> " of " <> pack (show broker))
                    Stateful.modifyModel (watched @child . at cId .~ Just (MkChildWatch broker (Set.singleton crash)))
              else do
                logWarning ("restart rate exceeded: " <> pack (show rate)
                            <> ", for child: " <> pack (show cId)
                            <> " of " <> pack (show broker))
                removeAndCleanChild @child cId
                let bw = currentModel ^? brokers . at broker . _Just . brokerMonitor . _Just
                case bw of
                  Nothing ->
                    return ()
                  Just b  -> do
                    logError ("a child of a linked broker crashed too often, interrupting: " <> pack (show broker))
                    let r =  ExitUnhandledError "restart frequency exceeded"
                    demonitor b
                    sendShutdown (broker ^. fromEndpoint) r
                    exitBecause r

      Effectful.OnDown pd@(ProcessDown _mref _ pid) -> do
        logDebug ("received " <> pack (show pd))
        let broker = asEndpoint pid
        oldModel <- Stateful.getAndModifyModel @(Watchdog child)
                      ( (brokers . at broker .~ Nothing)
                      . (watched %~ Map.filter (\cw -> cw^.parent /= broker))
                      )
        let deadBroker = oldModel ^? brokers . at broker . _Just
        traverse_ (logNotice . ("dettach from dead broker: " <>) . pack . show) deadBroker
        let forgottenChildren = oldModel ^.. watched . itraversed . filtered (\cw -> cw^.parent /= broker)
        traverse_ (logNotice . ("forgetting children: " <>) . pack . show) forgottenChildren
        when (isJust (deadBroker >>= view brokerMonitor)) $ do
          logError ("linked broker exited: " <> pack (show broker))
          exitBecause (ExitOtherProcessNotRunning pid)

      Effectful.OnTimeOut t -> do
        logError ("received: " <> pack (show t))

      Effectful.OnMessage (fromStrictDynamic -> Just (MkExonerationTimer cId ref :: ExonerationTimer (Broker.ChildId child))) -> do
        logInfo ("exonerating: " <> pack (show cId))
        Stateful.modifyModel
          (watched @child . at cId . _Just . crashes %~ Set.filter (\c -> c^.exonerationTimerReference /= ref))

      Effectful.OnMessage t -> do
        logError ("received: " <> pack (show t))

      Effectful.OnInterrupt reason -> do
        logError ("received: " <> pack (show reason))

-- ------------------ Start Argument

crashRate :: Lens' (Stateful.StartArgument (Watchdog child) (Processes e)) CrashRate
crashRate = lens _crashRate (\m x -> m {_crashRate = x})

-- ----------------- Crash Rate

-- | The limit of crashes (see 'CrashCount') per time span (see 'CrashTimeSpan') that justifies restarting
-- child processes.
--
-- Used as parameter for 'startLink'.
--
-- Use 'crashesPerSeconds' to construct a value.
--
-- This governs how long the 'ExonerationTimer' runs before cleaning up a 'CrashReport' in a 'ChildWatch'.
--
-- @since 0.30.0
data CrashRate =
  CrashesPerSeconds { _crashCount :: CrashCount
                    , _crashTimeSpan :: CrashTimeSpan
                    }
  deriving (Typeable, Eq, Ord)

-- | The default is three crashes in 30 seconds.
--
-- @since 0.30.0
instance Default CrashRate where
  def = 3 `crashesPerSeconds` 30

instance Show CrashRate where
  showsPrec d (CrashesPerSeconds count time) =
    showParen (d>=7) (shows count . showString " crashes/" . shows time . showString " seconds")

instance NFData CrashRate where
  rnf (CrashesPerSeconds c t) = c `seq` t `seq` ()

-- | Number of crashes in 'CrashRate'.
--
-- @since 0.30.0
type CrashCount = Int

-- | Timespan in which crashes are counted in 'CrashRate'.
--
-- @since 0.30.0
type CrashTimeSpan = Int

-- | A smart constructor for 'CrashRate'.
--
-- The first parameter is the number of crashes allowed per number of seconds (second parameter)
-- until the watchdog should give up restarting a child.
--
-- @since 0.30.0
crashesPerSeconds :: CrashCount -> CrashTimeSpan -> CrashRate
crashesPerSeconds = CrashesPerSeconds

-- | A lens for '_crashCount'.
--
-- @since 0.30.0
crashCount :: Lens' CrashRate CrashCount
crashCount = lens _crashCount (\(CrashesPerSeconds _ time) count -> CrashesPerSeconds count time )

-- | A lens for '_crashTimeSpan'.
--
-- @since 0.30.0
crashTimeSpan :: Lens' CrashRate CrashTimeSpan
crashTimeSpan = lens _crashTimeSpan (\(CrashesPerSeconds count _) time -> CrashesPerSeconds count time)

-- ------------------ Crash

-- | An internal data structure that records a single crash of a child of an attached 'Broker'.
--
-- See 'attachPermanent' and 'attachTemporary'.
--
-- @since 0.30.0
data CrashReport a =
  MkCrashReport { _exonerationTimerReference :: TimerReference
                  -- ^ After a crash, an 'ExonerationTimer' according to the 'CrashRate' of the 'Watchdog'
                  -- is started, this is the reference
                , _crashTime :: UTCTime
                  -- ^ Recorded time of the crash
                , _crashReason :: Interrupt 'NoRecovery
                  -- ^ Recorded crash reason
                }
  deriving (Eq, Ord, Typeable)

instance (Show a, Typeable a) => Show (CrashReport a) where
  showsPrec d c =
    showParen (d>=10)
      ( showString "crash report: "
      . showString "child: " . showSPrecTypeable @a 10
      . showString "time: " . showsPrec 10 (c^.crashTime)
      . showString "reason: " . showsPrec 10 (c^.crashReason)
      . showString "exoneration timer: " . showsPrec 10 (c^.exonerationTimerReference)
      )

instance NFData (CrashReport a) where
  rnf (MkCrashReport !a !b !c) = rnf a `seq` rnf b `seq` rnf c `seq` ()

-- | Lens for '_crashTime'
--
-- @since 0.30.0
crashTime :: Lens' (CrashReport a) UTCTime
crashTime = lens _crashTime (\c t -> c { _crashTime = t})

-- | Lens for '_crashReason'
--
-- @since 0.30.0
crashReason :: Lens' (CrashReport a) (Interrupt 'NoRecovery)
crashReason = lens _crashReason (\c t -> c { _crashReason = t})

-- | Lens for '_exonerationTimerReference'
--
-- @since 0.30.0
exonerationTimerReference :: Lens' (CrashReport a) TimerReference
exonerationTimerReference = lens _exonerationTimerReference (\c t -> c { _exonerationTimerReference = t})

startExonerationTimer :: forall child a q e .
     (HasProcesses e q, Lifted IO q, Lifted IO e, Show a, NFData a, Typeable a, Typeable child)
     => a -> Interrupt 'NoRecovery -> CrashTimeSpan -> Eff e (CrashReport a)
startExonerationTimer cId r t = do
  let title = MkProcessTitle ("ExonerationTimer<" <> pack (showSTypeable @child ">") <> pack (show cId))
  me <- self
  ref <- sendAfterWithTitle title me (TimeoutMicros (t * 1_000_000)) (MkExonerationTimer cId)
  now <- lift getCurrentTime
  return (MkCrashReport ref now r)

-- | The timer started based on the 'CrashRate' '_crashTimeSpan' when a 'CrashReport' is recorded.
--
-- After this timer elapses, the 'Watchdog' server will remove the 'CrashReport' from the 'ChildWatch' of
-- that child.
--
-- @since 0.30.0
data ExonerationTimer a =  MkExonerationTimer !a !TimerReference
  deriving (Eq, Ord, Typeable)

instance NFData a => NFData (ExonerationTimer a) where
  rnf (MkExonerationTimer !x !r) = rnf r `seq` rnf x `seq` ()

instance Show a => Show (ExonerationTimer a) where
  showsPrec d (MkExonerationTimer x r) =
    showParen (d >= 10)
      ( showString "exonerate: " . showsPrec 10 x
      . showString " after: " .  showsPrec 10 r
      )

-- --------------------------- Child Watches

-- | An internal data structure that keeps the 'CrashReport's of a child of an attached 'Broker' monitored by a 'Watchdog'.
--
-- See 'attachPermanent' and 'attachTemporary', 'ExonerationTimer', 'CrashRate'.
--
-- @since 0.30.0
data ChildWatch child =
  MkChildWatch
    { _parent :: Endpoint (Broker child)
      -- ^ The attached 'Broker' that started the child
    , _crashes :: Set (CrashReport (Broker.ChildId child))
      -- ^ The crashes of the child. If the number of crashes
      -- surpasses the allowed number of crashes before the
      -- 'ExonerationTimer's clean them, the child is finally crashed.
    }
   deriving Typeable

instance NFData (ChildWatch child) where
  rnf (MkChildWatch p c) =
    rnf p `seq` rnf c `seq` ()

instance (Typeable child, Typeable (Broker.ChildId child), Show (Broker.ChildId child)) => Show (ChildWatch child) where
  showsPrec d (MkChildWatch p c) =
    showParen (d>=10) ( showString "child-watch: parent: "
                      . showsPrec 10 p
                      . showString " crashes: "
                      . foldr (.) id (showsPrec 10 <$> Set.toList c)
                      )

-- | A lens for '_parent'.
--
-- @since 0.30.0
parent :: Lens' (ChildWatch child) (Endpoint (Broker child))
parent = lens _parent (\m x -> m {_parent = x})

-- | A lens for '_crashes'
--
-- @since 0.30.0
crashes :: Lens' (ChildWatch child) (Set (CrashReport (Broker.ChildId child)))
crashes = lens _crashes (\m x -> m {_crashes = x})

-- ------------------ Model

instance Default (Stateful.Model (Watchdog child)) where
  def = WatchdogModel def Map.empty

-- -------------------------- Model -> Child Watches

watched :: Lens' (Stateful.Model (Watchdog child)) (Map (Broker.ChildId child) (ChildWatch child))
watched = lens _watched (\m x -> m {_watched = x})

childWatches :: IndexedTraversal' (Broker.ChildId child) (Stateful.Model (Watchdog child)) (ChildWatch child)
childWatches = watched . itraversed

childWatchesById ::
     Eq (Broker.ChildId child)
  => Broker.ChildId child
  -> Traversal' (Stateful.Model (Watchdog child)) (ChildWatch child)
childWatchesById theCId = childWatches . ifiltered (\cId _ -> cId == theCId)

childWatchesByParenAndId ::
     Eq (Broker.ChildId child)
  => Endpoint (Broker child)
  -> Broker.ChildId child
  -> Traversal' (Stateful.Model (Watchdog child)) (ChildWatch child)
childWatchesByParenAndId theParent theCId =
  childWatches . ifiltered (\cId cw -> cw ^. parent == theParent && cId == theCId)

countRecentCrashes ::
     Eq (Broker.ChildId child)
  => Endpoint (Broker child)
  -> Broker.ChildId child
  -> Stateful.Model (Watchdog child)
  -> CrashCount
countRecentCrashes theParent theCId theModel =
 length (theModel ^.. childWatchesByParenAndId theParent theCId . crashes . folded)

-- --------------------------- Model -> Broker Watches

brokers :: Lens' (Stateful.Model (Watchdog child)) (Map (Endpoint (Broker child)) BrokerWatch)
brokers = lens _brokers (\m x -> m {_brokers = x})

-- -------------------------- Server Implementation Helpers

removeAndCleanChild ::
  forall child q e.
  ( HasProcesses e q
  , Typeable child
  , Typeable (Broker.ChildId child)
  , Ord (Broker.ChildId child)
  , Show (Broker.ChildId child)
  , Member (Stateful.ModelState (Watchdog child)) e
  , Lifted IO e
  , Lifted IO q
  , Member Logs e
  )
  => Broker.ChildId child
  -> Eff e ()
removeAndCleanChild cId = do
    oldModel <- Stateful.modifyAndGetModel (watched @child . at cId .~ Nothing)
    forMOf_ (childWatchesById cId) oldModel $ \w -> do
      logDebug ("removing client entry: " <> pack (show cId))
      forMOf_ (crashes . folded . exonerationTimerReference) w cancelTimer
      logDebug (pack (show w))

