{-# LANGUAGE UndecidableInstances #-}
-- | Monitor a process and act when it is unresponsive.
--
-- @since 0.30.0
module Control.Eff.Concurrent.Protocol.Watchdog (startLink, attach, attachLinked, Watchdog) where

import Control.DeepSeq
import Control.Eff (Eff, Member)
import Control.Eff.Concurrent.Misc
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Client
import Control.Eff.Concurrent.Protocol.Wrapper
import qualified Control.Eff.Concurrent.Protocol.Observer as Observer
import qualified Control.Eff.Concurrent.Protocol.Broker as Broker
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
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_)


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
attach
  :: forall child q e
  . ( HasCallStack
    , LogIo q
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , HasProcesses e q
    )
  => Endpoint (Watchdog child) -> Endpoint (Broker.Broker child) -> Eff e ()
attach wd broker = call wd (Attach broker)


-- | Restart children of the given broker.
--
-- When the broker exits, the watchdog process will exit, too.
--
-- @since 0.30.0
attachLinked
  :: forall child q e
  . ( HasCallStack
    , LogIo q
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , HasProcesses e q
    )
  => Endpoint (Watchdog child) -> Endpoint (Broker.Broker child) -> Eff e ()
attachLinked wd broker = call wd (AttachLinked broker)

--  When a child crashes,
--   - if the allowed maximum number crashes per time span has been reached for the process,
--       - cancel all other timers
--       - don't start the child again
--       - if this is a /sensitive/ watchdog crash the watchdog
--     otherwise
--       - tell the broker to start the child
--       - record a crash and start a timer to remove the record later
--       - monitor the child
--  When a child crash timer elapses,
--   - remove the crash record
newtype Crash =
  Crash { crashCleanupTimer :: TimerReference }

newtype CrashRate = CrashesPerPicos { crashesPerPicos :: Rational }

crashesPerSeconds :: Integer -> Integer -> CrashRate
crashesPerSeconds occurrences seconds =
  let picos :: Pico
      picos = fromInteger seconds

      picos' :: Integer
      picos' = ceiling (picos * fromInteger (resolution (1 :: Pico)))
  in CrashesPerPicos (occurrences % picos')

instance Typeable child => HasPdu (Watchdog child) where
  type instance EmbeddedPduList (Watchdog child) = '[Observer.Observer (Broker.ChildEvent child)]
  data Pdu (Watchdog child) r where
    Attach :: Endpoint (Broker.Broker child) -> Pdu (Watchdog child) ('Synchronous ())
    AttachLinked :: Endpoint (Broker.Broker child) -> Pdu (Watchdog child) ('Synchronous ())
    OnChildEvent :: Broker.ChildEvent child -> Pdu (Watchdog child) 'Asynchronous
      deriving Typeable

instance Typeable child => HasPduPrism (Watchdog child) (Observer.Observer (Broker.ChildEvent child)) where
  embedPdu (Observer.Observed e) = OnChildEvent e
  fromPdu (OnChildEvent x) = Just (Observer.Observed x)
  fromPdu _ = Nothing

instance (NFData (Broker.ChildId child)) => NFData (Pdu (Watchdog child) r) where
  rnf (Attach e) = rnf e
  rnf (AttachLinked e) = rnf e
  rnf (OnChildEvent o) = rnf o

instance
  ( Show (Broker.ChildId child)
  , Typeable child
  , Typeable (Effectful.ServerPdu child)
  )
  => Show (Pdu (Watchdog child) r) where
  showsPrec d (Attach e) = showParen (d>=10) (showString "attach: " . shows e)
  showsPrec d (AttachLinked e) = showParen (d>=10) (showString "attach-linked: " . shows e)
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

  newtype instance Model (Watchdog child) =
    WatchdogModel { _brokers :: Map (Endpoint (Broker.Broker child)) (Broker child) -- Just monitor means that the broker is linked
                  }
                  deriving (Default)

  update _me startArg =
    \case
      Effectful.OnCall rt (Attach broker) -> do
        logDebug ("attaching to: " <> pack (show broker))
        doAttach broker
        sendReply rt ()

      Effectful.OnCall rt (AttachLinked broker) -> do
        logDebug ("attaching and linking to: " <> pack (show broker))
        doAttachAndLink broker
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
            logNotice ("==== restarting: " <> pack (show cId))
            res <- Broker.spawnChild broker cId
            logNotice ("restarted: "  <> pack (show cId) <> ": " <> pack (show res))

      Effectful.OnDown pd@(ProcessDown mref _ pid) -> do
        logDebug ("received " <> pack (show pd))
--        brokerMonRef <- Stateful.askSettings @(Watchdog child)
--        if mref == brokerMonRef then do
--          logError "attached broker exited unexpectedly"
--          exitBecause (ExitOtherProcessNotRunning pid)
--        else
--          logWarning ("unexpected process down: " <> pack (show pd))


doAttach
  :: forall child e q .
     ( Typeable child
     , HasProcesses e q
     , Member Logs e
     , Member (Stateful.ModelState (Watchdog child)) e
     )
  => Endpoint (Broker.Broker child)
  -> Eff e ()
doAttach broker = do
  oldModel <- Stateful.getModel @(Watchdog child)
  let oldMonitor = oldModel ^? brokers . at broker . _Just . brokerMonitor . _Just
  traverse_ (\mr -> do
    logDebug ("stop monitoring " <> pack (show broker) <> " " <> pack (show mr))
    demonitor mr)
    oldMonitor
  let newBroker = MkBroker Nothing (fromMaybe def (oldModel ^? brokers . at broker . _Just . crashes))
  Stateful.modifyModel (brokers . at broker ?~ newBroker)

doAttachAndLink
  :: forall child e q .
     ( Typeable child
     , HasProcesses e q
     , Member Logs e
     , Member (Stateful.ModelState (Watchdog child)) e
     )
  => Endpoint (Broker.Broker child)
  -> Eff e ()
doAttachAndLink broker = do
  oldModel <- Stateful.getModel @(Watchdog child)
  let oldMonitor = oldModel ^? brokers . at broker . _Just . brokerMonitor . _Just
  mr <- case oldMonitor of
    Nothing -> do
      mr <- monitor (broker^.fromEndpoint)
      logDebug ("monitoring " <> pack (show broker) <> " " <> pack (show mr))
      return mr

    Just mr -> do
      logDebug ("already monitoring " <> pack (show broker) <> " - keeping: " <> pack (show mr))
      return mr

  let newBroker = MkBroker (Just mr) (fromMaybe def (oldModel ^? brokers . at broker . _Just . crashes))
  Stateful.modifyModel (brokers . at broker ?~ newBroker)


data Broker child =
  MkBroker { _brokerMonitor :: Maybe MonitorReference
           , _crashes :: Map (Broker.ChildId child) (Set Crash)
           }

instance Default (Broker child) where
  def = MkBroker def def

brokerMonitor :: Lens' (Broker child) (Maybe MonitorReference)
brokerMonitor = lens _brokerMonitor (\m x -> m {_brokerMonitor = x})

crashes :: Lens' (Broker child) (Map (Broker.ChildId child) (Set Crash))
crashes = lens _crashes (\m x -> m {_crashes = x})

brokers :: Lens' (Stateful.Model (Watchdog child)) (Map (Endpoint (Broker.Broker child)) (Broker child))
brokers = lens _brokers (\m x -> m {_brokers = x})

--crashRate :: Lens' (Stateful.StartArgument (Watchdog child) (Processes e)) CrashRate
--crashRate = lens _crashRate (\m x -> m {_crashRate = x})
