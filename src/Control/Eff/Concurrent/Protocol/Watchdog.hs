{-# LANGUAGE UndecidableInstances #-}
-- | Monitor a process and act when it is unresponsive.
--
-- @since 0.29.3
module Control.Eff.Concurrent.Protocol.Watchdog (startLink) where

import Control.Eff.Concurrent.Misc
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Log
import qualified Control.Eff.Concurrent.Protocol.Supervisor as Supervisor
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import Control.Lens
import Control.DeepSeq
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
import Control.Eff (Eff)


data Watchdog (child :: Type) deriving Typeable

instance Typeable child => HasPdu (Watchdog child) where
  data Pdu (Watchdog child) r where
    GetChild :: Pdu (Watchdog child) ('Synchronous (Endpoint (Effectful.ServerPdu child)))
      deriving Typeable

instance NFData (Pdu (Watchdog child) r) where
  rnf GetChild = ()

instance Show (Pdu (Watchdog child) r) where
  showsPrec _ GetChild = showString "get-child"


-- | Start and link a new watchdog process.
--
-- The watchdog process will register itself to the 'Supervisor.ChildEvent's and
-- restart crashed children.
--
-- @since 0.29.3
startLink
  :: forall p e
  . ( HasCallStack
    , LogIo e
    , Typeable p
    )
  => Endpoint (Supervisor.Sup p)
  -> Eff (Processes e) (Endpoint (Watchdog p))
startLink sup =
  Stateful.startLink (StartWatchDog sup (3 `crashesPerSeconds` 30))

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

instance (Typeable child, LogIo e) => Stateful.Server (Watchdog child) (Processes e) where

  data instance StartArgument (Watchdog child) (Processes e) =
    StartWatchDog { _supervisor :: Endpoint (Supervisor.Sup child)
                  , _crashRate :: CrashRate
                  }
      deriving Typeable

  type instance Settings (Watchdog child) = MonitorReference

  data instance Model (Watchdog child) =
    WatchdogModel { _crashMap :: Map (Supervisor.ChildId child) (Set Crash)
                  }

  setup _ep (StartWatchDog sup _) = do
    logInfo ("Watchdog attaching to: " <> pack (show sup))
    mref <- monitor (sup ^. fromEndpoint)
    return (def, mref)

  update _me startArg =
    \case
      Effectful.OnCall rt GetChild -> error "TODO"
      Effectful.OnDown pd@(ProcessDown mref reason _) -> do
        supMonRef <- Stateful.askSettings @(Watchdog child)
        if mref == supMonRef then do
          logError "attached supervisor exited unexpectedly"
          exitBecause (ExitOtherProcessNotRunning (startArg ^. supervisor . fromEndpoint))
        else
          logWarning ("unexpected process down: " <> pack (show pd))


instance Typeable child => Default (Stateful.Model (Watchdog child)) where
  def = WatchdogModel def

supervisor :: Lens' (Stateful.StartArgument (Watchdog child) (Processes e)) (Endpoint (Supervisor.Sup child))
supervisor = lens _supervisor (\m x -> m {_supervisor = x})

--crashRate :: Lens' (Stateful.StartArgument (Watchdog child) (Processes e)) CrashRate
--crashRate = lens _crashRate (\m x -> m {_crashRate = x})
