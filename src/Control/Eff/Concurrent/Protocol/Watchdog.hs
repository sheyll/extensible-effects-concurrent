{-# LANGUAGE UndecidableInstances #-}
-- | Monitor a process and act when it is unresponsive.
--
-- @since 0.29.3
module Control.Eff.Concurrent.Protocol.Watchdog (startLink, attach, attachLinked, Watchdog) where

import Control.DeepSeq
import Control.Eff (Eff)
import Control.Eff.Concurrent.Misc
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Client
import qualified Control.Eff.Concurrent.Protocol.Observer as Observer
import qualified Control.Eff.Concurrent.Protocol.Supervisor as Supervisor
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


data Watchdog (child :: Type) deriving Typeable


-- | Start and link a new watchdog process.
--
-- The watchdog process will register itself to the 'Supervisor.ChildEvent's and
-- restart crashed children.
--
-- @since 0.29.3
startLink
  :: forall child q e
  . ( HasCallStack
    , LogIo q
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Supervisor.ChildId child)
    , Ord (Supervisor.ChildId child)
    , HasProcesses e q
    )
  => Eff e (Endpoint (Watchdog child))
startLink =
  Stateful.startLink (StartWatchDog (3 `crashesPerSeconds` 30))

-- | Restart children of the given supervisor.
--
-- When the supervisor exits, ignore the children of that supervisor.
--
-- @since 0.29.3
attach
  :: forall child q e
  . ( HasCallStack
    , LogIo q
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Supervisor.ChildId child)
    , Ord (Supervisor.ChildId child)
    , HasProcesses e q
    )
  => Endpoint (Watchdog child) -> Endpoint (Supervisor.Sup child) -> Eff e ()
attach wd sup = call wd (Attach sup)


-- | Restart children of the given supervisor.
--
-- When the supervisor exits, the watchdog process will exit, too.
--
-- @since 0.29.3
attachLinked
  :: forall child q e
  . ( HasCallStack
    , LogIo q
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Supervisor.ChildId child)
    , Ord (Supervisor.ChildId child)
    , HasProcesses e q
    )
  => Endpoint (Watchdog child) -> Endpoint (Supervisor.Sup child) -> Eff e ()
attachLinked wd sup = call wd (AttachLinked sup)

--  When a child crashes,
--   - if the allowed maximum number crashes per time span has been reached for the process,
--       - cancel all other timers
--       - don't start the child again
--       - if this is a /sensitive/ watchdog crash the watchdog
--     otherwise
--       - tell the supervisor to start the child
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
  type instance EmbeddedPduList (Watchdog child) = '[Observer.Observer (Supervisor.ChildEvent child)]
  data Pdu (Watchdog child) r where
    Attach :: Endpoint (Supervisor.Sup child) -> Pdu (Watchdog child) ('Synchronous ())
    AttachLinked :: Endpoint (Supervisor.Sup child) -> Pdu (Watchdog child) ('Synchronous ())
    OnChildEvent :: Supervisor.ChildEvent child -> Pdu (Watchdog child) 'Asynchronous
      deriving Typeable

instance Typeable child => HasPduPrism (Watchdog child) (Observer.Observer (Supervisor.ChildEvent child)) where
  embedPdu (Observer.Observed e) = OnChildEvent e
  fromPdu (OnChildEvent x) = Just (Observer.Observed x)
  fromPdu _ = Nothing

instance (NFData (Supervisor.ChildId child)) => NFData (Pdu (Watchdog child) r) where
  rnf (Attach e) = rnf e
  rnf (AttachLinked e) = rnf e
  rnf (OnChildEvent o) = rnf o

instance
  ( Show (Supervisor.ChildId child)
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
  , Tangible (Supervisor.ChildId child)
  , Ord (Supervisor.ChildId child)
  , LogIo e
  ) => Stateful.Server (Watchdog child) (Processes e) where

  data instance StartArgument (Watchdog child) (Processes e) =
    StartWatchDog { _crashRate :: CrashRate
                  }
      deriving Typeable

  newtype instance Model (Watchdog child) =
    WatchdogModel { _supervisors :: Map (Endpoint (Supervisor.Sup child)) (Maybe MonitorReference) -- Just monitor means that the supervisor is linked
                  }
                  deriving (Default)

  update _me startArg =
    \case
      Effectful.OnCall rt (Attach sup) -> do
        logDebug ("attaching to: " <> pack (show sup))


      Effectful.OnCall rt (AttachLinked sup) -> do
        logDebug ("attaching and linking to: " <> pack (show sup))


      Effectful.OnCast (OnChildEvent e) ->
        case e of
          Supervisor.OnSupervisorShuttingDown sup -> do
            logInfo ("linked supervisor " <> pack (show sup) <> " is shutting down.")
            exitNormally
          down@(Supervisor.OnChildSpawned sup _ _) ->
            logInfo (pack (show down))
          down@(Supervisor.OnChildDown sup _ _ ExitNormally) ->
            logNotice (pack (show down))
          down@(Supervisor.OnChildDown sup cId _ _) -> do
            logNotice (pack (show down))
            logNotice ("==== restarting: " <> pack (show cId))
            res <- Supervisor.spawnChild sup cId
            logNotice ("restarted: "  <> pack (show cId) <> ": " <> pack (show res))

      Effectful.OnDown pd@(ProcessDown mref _ pid) -> do
        logDebug ("received " <> pack (show pd))
--        supMonRef <- Stateful.askSettings @(Watchdog child)
--        if mref == supMonRef then do
--          logError "attached supervisor exited unexpectedly"
--          exitBecause (ExitOtherProcessNotRunning pid)
--        else
--          logWarning ("unexpected process down: " <> pack (show pd))


data Supervisor child =
  MkSupervisor { _supervisorMonitor :: Maybe MonitorReference
               , _crashes :: Map (Supervisor.ChildId child) (Set Crash)
               }

instance Default (Supervisor child) where
  def = MkSupervisor def def

supervisorMonitor :: Lens' (Supervisor child) (Maybe MonitorReference)
supervisorMonitor = lens _supervisorMonitor (\m x -> m {_supervisorMonitor = x})

crashes :: Lens' (Supervisor child) (Map (Supervisor.ChildId child) (Set Crash))
crashes = lens _crashes (\m x -> m {_crashes = x})

supervisors :: Lens' (Stateful.Model (Watchdog child)) (Map (Endpoint (Supervisor.Sup child)) (Maybe MonitorReference))
supervisors = lens _supervisors (\m x -> m {_supervisors = x})

--crashRate :: Lens' (Stateful.StartArgument (Watchdog child) (Processes e)) CrashRate
--crashRate = lens _crashRate (\m x -> m {_crashRate = x})
