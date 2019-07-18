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
  , CrashRate()
  , crashesPerSeconds
  , CrashCount
  , CrashTimeSpan
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
import Data.Maybe (isJust, isNothing)
import Data.Foldable (traverse_)
import Control.Monad (when)


data Watchdog (child :: Type) deriving Typeable


-- | Start and link a new watchdog process.
--
-- The watchdog process will register itself to the 'Broker.ChildEvent's and
-- restart crashed children.
--
-- @since 0.30.0
startLink
  :: forall child q e
  . ( HasCallStack
    , LogIo q
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , HasProcesses e q
    )
  => Eff e (Endpoint (Watchdog child))
startLink =
  Stateful.startLink (StartWatchDog (3 `crashesPerSeconds` 30))

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
    )
  => Endpoint (Watchdog child) -> Endpoint (Broker child) -> Eff e ()
attachTemporary wd broker = call wd (AttachTemporary broker)

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
    )
  => Endpoint (Watchdog child) -> Endpoint (Broker child) -> Eff e ()
attachPermanent wd broker = call wd (AttachPermanent broker)

instance Typeable child => HasPdu (Watchdog child) where
  type instance EmbeddedPduList (Watchdog child) = '[Observer (Broker.ChildEvent child)]
  data Pdu (Watchdog child) r where
    AttachTemporary :: Endpoint (Broker child) -> Pdu (Watchdog child) ('Synchronous ())
    AttachPermanent :: Endpoint (Broker child) -> Pdu (Watchdog child) ('Synchronous ())
    OnChildEvent :: Broker.ChildEvent child -> Pdu (Watchdog child) 'Asynchronous
      deriving Typeable

instance Typeable child => HasPduPrism (Watchdog child) (Observer (Broker.ChildEvent child)) where
  embedPdu (Observer.Observed e) = OnChildEvent e
  fromPdu (OnChildEvent x) = Just (Observer.Observed x)
  fromPdu _ = Nothing

instance (NFData (Broker.ChildId child)) => NFData (Pdu (Watchdog child) r) where
  rnf (AttachTemporary e) = rnf e
  rnf (AttachPermanent e) = rnf e
  rnf (OnChildEvent o) = rnf o

instance
  ( Show (Broker.ChildId child)
  , Typeable child
  , Typeable (Effectful.ServerPdu child)
  )
  => Show (Pdu (Watchdog child) r) where
  showsPrec d (AttachTemporary e) = showParen (d>=10) (showString "attachTemporary: " . shows e)
  showsPrec d (AttachPermanent e) = showParen (d>=10) (showString "attachTemporary-linked: " . shows e)
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

      Effectful.OnCast (OnChildEvent e) ->
        case e of
          Broker.OnBrokerShuttingDown broker -> do
            logInfo ("linked broker " <> pack (show broker) <> " is shutting down.")


          down@(Broker.OnChildSpawned broker cId _) -> do
            logInfo ("received: " <> pack (show down))
            m <- Stateful.getModel @(Watchdog child)
            when (isNothing (m ^? childWatchesById cId)) $ do
              logDebug ("inserting new child watch: " <> pack (show cId) <> " of " <> pack (show broker))
              Stateful.modifyModel (watched @child . at cId .~ Just (MkChildWatch broker Set.empty))

          down@(Broker.OnChildDown _broker cId _ ExitNormally) -> do
            logInfo ("received: " <> pack (show down))
            removeAndCleanChild @child cId

          down@(Broker.OnChildDown broker cId _ reason) -> do
            logNotice ("received: " <> pack (show down))
            currentModel <- Stateful.getModel @(Watchdog child)
            let recentCrashes = countRecentCrashes broker cId currentModel
                rate = startArg ^. crashRate
                maxCrashCount = rate ^. crashCount
            if recentCrashes < maxCrashCount then do
              logNotice ("restarting (" <> pack (show recentCrashes) <> "/" <> pack (show maxCrashCount) <> "): "
                          <> pack (show cId) <> " of " <> pack (show broker))
              res <- Broker.spawnChild broker cId
              logNotice ("restarted: " <> pack (show cId) <> " of "
                          <> pack (show broker) <> ": " <> pack (show res))
              crash <- newCrash reason (rate ^. crashTimeSpan)
              Stateful.modifyModel (watched @child . at cId . _Just . crashes %~ Set.insert crash)
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
        logDebug ("received: " <> pack (show t))

      Effectful.OnInterrupt reason@(LinkedProcessCrashed child) -> do
        logDebug ("received: " <> pack (show reason))

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
-- @since 0.30.0
data CrashRate =
  CrashesPerSeconds { _crashCount :: CrashCount
                    , _crashTimeSpan :: CrashTimeSpan
                    }
  deriving (Typeable, Eq, Ord)

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

crashCount :: Lens' CrashRate CrashCount
crashCount = lens _crashCount (\(CrashesPerSeconds _ time) count -> CrashesPerSeconds count time )

crashTimeSpan :: Lens' CrashRate CrashTimeSpan
crashTimeSpan = lens _crashTimeSpan (\(CrashesPerSeconds count _) time -> CrashesPerSeconds count time)

-- ------------------ Crash

data Crash =
  MkCrash { _crashCleanupTimer :: TimerReference
          , _crashTime :: UTCTime
          , _crashReason :: Interrupt 'NoRecovery
          }
  deriving (Eq, Ord)

crashTime :: Lens' Crash UTCTime
crashTime = lens _crashTime (\c t -> c { _crashTime = t})

crashReason :: Lens' Crash (Interrupt 'NoRecovery)
crashReason = lens _crashReason (\c t -> c { _crashReason = t})

crashCleanupTimer :: Lens' Crash TimerReference
crashCleanupTimer = lens _crashCleanupTimer (\c t -> c { _crashCleanupTimer = t})

newCrash :: (HasProcesses e q, Lifted IO q, Lifted IO e) => Interrupt 'NoRecovery -> CrashTimeSpan -> Eff e Crash
newCrash r t = do
  ref <- startTimer (TimeoutMicros (t * 1_000_000))
  now <- lift getCurrentTime
  return (MkCrash ref now r)

instance Show Crash where
  showsPrec d c =
    showParen (d>=10)
      ( showString "crash: "
      . showString "time: " . showsPrec 10 (c^.crashTime)
      . showString "reason: " . showsPrec 10 (c^.crashReason)
      . showString "cleanup-timer: " . showsPrec 10 (c^.crashCleanupTimer)
      )

-- --------------------------- Child Watches

data ChildWatch child =
  MkChildWatch
    { _parent :: Endpoint (Broker child)
    , _crashes :: Set Crash
    }

instance Typeable child => Show (ChildWatch child) where
  showsPrec d (MkChildWatch p c) =
    showParen (d>=10) ( showString "child-watch: parent: "
                      . showsPrec 10 p
                      . showString " crashes: "
                      . foldr (.) id (showsPrec 10 <$> Set.toList c)
                      )

parent :: Lens' (ChildWatch child) (Endpoint (Broker child))
parent = lens _parent (\m x -> m {_parent = x})

crashes :: Lens' (ChildWatch child) (Set Crash)
crashes = lens _crashes (\m x -> m {_crashes = x})

childWatches :: IndexedTraversal' (Broker.ChildId child) (Stateful.Model (Watchdog child)) (ChildWatch child)
childWatches = watched . itraversed
--
--childWatchesByParent ::
--     Endpoint (Broker child)
--  -> IndexedTraversal' (Broker.ChildId child) (Stateful.Model (Watchdog child)) (ChildWatch child)
--childWatchesByParent theParent = childWatches . ifiltered (\_cId cw -> cw ^. parent == theParent)

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

-- ------------------ Model

instance Default (Stateful.Model (Watchdog child)) where
  def = WatchdogModel def Map.empty

-- -------------------------- Model -> Child Watches

watched :: Lens' (Stateful.Model (Watchdog child)) (Map (Broker.ChildId child) (ChildWatch child))
watched = lens _watched (\m x -> m {_watched = x})

-- --------------------------- Model -> Broker Watches

brokers :: Lens' (Stateful.Model (Watchdog child)) (Map (Endpoint (Broker child)) BrokerWatch)
brokers = lens _brokers (\m x -> m {_brokers = x})

-- -------------------------- Server Implementation Helpers

removeAndCleanChild ::
  forall child q e.
  ( HasProcesses e q
  , Typeable child
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
      forMOf_ (crashes . folded . crashCleanupTimer) w cancelTimer
      logDebug (pack (show w))

