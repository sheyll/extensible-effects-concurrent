{-# LANGUAGE UndecidableInstances #-}
-- | Monitor a process and act when it is unresponsive.
--
-- @since 0.30.0
module Control.Eff.Concurrent.Protocol.Watchdog
  ( startLink
  , crashesPerSeconds
  , Watchdog
  , attachTemporary
  , attachPermanent
  , CrashRate(..)
  ) where

import Control.DeepSeq
import Control.Eff (Eff, Member)
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
import Data.Ratio
import Data.Fixed
import Data.Time.Clock
import Data.Kind (Type)
import Data.Default
import Data.String
import Data.Text (pack)
import GHC.Stack (HasCallStack)
import Data.Maybe (fromMaybe, isJust)
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

-- | The limit of crashes per time span that justifies restarting
-- child processes.
--
-- Used as parameter for 'startLink'.
--
-- Use 'crashesPerSeconds' to construct a value.
--
-- @since 0.30.0
newtype CrashRate = CrashesPerPicos { crashesPerPicos :: Rational }

-- | A smart constructor for 'CrashRate'.
--
-- @since 0.30.0
crashesPerSeconds :: Integer -> Integer -> CrashRate
crashesPerSeconds occurrences seconds =
  let picos :: Pico
      picos = fromInteger seconds

      picos' :: Integer
      picos' = ceiling (picos * fromInteger (resolution (1 :: Pico)))
  in CrashesPerPicos (occurrences % picos')

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

instance
  ( Typeable child
  , HasPdu (Effectful.ServerPdu child)
  , Tangible (Broker.ChildId child)
  , Ord (Broker.ChildId child)
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

  update me _startArg =
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
            exitNormally
          down@(Broker.OnChildSpawned broker _ _) ->
            logInfo (pack (show down))
          down@(Broker.OnChildDown broker _ _ ExitNormally) ->
            logNotice (pack (show down))
          down@(Broker.OnChildDown broker cId _ _) -> do
            logNotice (pack (show down))
            newModel <- Stateful.getModel @(Watchdog child)

            logNotice ("restarting: " <> pack (show cId))
            res <- Broker.spawnChild broker cId
            logNotice ("restarted: "  <> pack (show cId) <> ": " <> pack (show res))

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


crashRate :: Lens' (Stateful.StartArgument (Watchdog child) (Processes e)) CrashRate
crashRate = lens _crashRate (\m x -> m {_crashRate = x})

instance Default (Stateful.Model (Watchdog child)) where
  def = WatchdogModel def Map.empty

watched :: Lens' (Stateful.Model (Watchdog child)) (Map (Broker.ChildId child) (ChildWatch child))
watched = lens _watched (\m x -> m {_watched = x})

brokers :: Lens' (Stateful.Model (Watchdog child)) (Map (Endpoint (Broker child)) BrokerWatch)
brokers = lens _brokers (\m x -> m {_brokers = x})

newtype BrokerWatch =
  MkBrokerWatch (Maybe MonitorReference)
  deriving (Default)

instance Show BrokerWatch where
  showsPrec _ (MkBrokerWatch Nothing) = showString "broker-watch"
  showsPrec d (MkBrokerWatch (Just mon)) = showParen (d>=10) (showString "linked-broker-watch " . showsPrec 10 mon)

brokerMonitor :: Iso' BrokerWatch (Maybe MonitorReference)
brokerMonitor = iso (\(MkBrokerWatch m) -> m) MkBrokerWatch

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

--  When a child crashes,
--   - if the allowed maximum number crashes per time span has been reached for the process,
--       - cancel all other timers
--       - don't start the child again
--       - if this is a /linked/ watchdog crash the watchdog
--     otherwise
--       - tell the broker to start the child
--       - record a crash and start a timer to remove the record later
--       - monitor the child
--  When a child crash timer elapses,
--   - remove the crash record
data Crash =
  Crash { _crashCleanupTimer :: TimerReference
        , _crashTime :: UTCTime
        , _crashReason :: Interrupt 'NoRecovery
        }

crashCleanupTimer :: Lens' Crash TimerReference
crashCleanupTimer = lens _crashCleanupTimer (\c t -> c { _crashCleanupTimer = t})

crashTime :: Lens' Crash UTCTime
crashTime = lens _crashTime (\c t -> c { _crashTime = t})

crashReason :: Lens' Crash (Interrupt 'NoRecovery)
crashReason = lens _crashReason (\c t -> c { _crashReason = t})

instance Show Crash where
  showsPrec d c =
    showParen (d>=10)
      ( showString "crash: "
      . showString "time: " . showsPrec 10 (c^.crashTime)
      . showString "reason: " . showsPrec 10 (c^.crashReason)
      . showString "cleanup-timer: " . showsPrec 10 (c^.crashCleanupTimer)
      )

