{-# LANGUAGE UndecidableInstances #-}
-- | Monitor a process and act when it is unresponsive.
--
-- @since 0.29.3
module Control.Eff.Concurrent.Protocol.Watchdog  where

import Control.Eff.Concurrent.Misc
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Log
import qualified Control.Eff.Concurrent.Protocol.Supervisor  as Supervisor
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


newtype WatchdogChildId child = MkWatchdogChildId (Supervisor.ChildId child)

instance Show (Supervisor.ChildId child) => Show (WatchdogChildId child) where
  showsPrec d (MkWatchdogChildId x) = showParen (d>=10) (showString "watchdog-child-id: " . shows x)

deriving instance Eq (Supervisor.ChildId child) => Eq (WatchdogChildId child)
deriving instance Ord (Supervisor.ChildId child) => Ord (WatchdogChildId child)
deriving instance NFData (Supervisor.ChildId child) => NFData (WatchdogChildId child)


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

instance (Typeable child, LogIo e, Effectful.Server child (Processes e))
  => Stateful.Server (Watchdog child) (Processes e)  where
  newtype instance StartArgument (Watchdog child) (Processes e) =
    StartWatchDog (Endpoint (Supervisor.Sup child))
      deriving Typeable
  data instance Model (Watchdog child) =
    WatchdogModel { _crashMap :: Map (Supervisor.ChildId child) (Set Crash)
                  , _crashRate :: CrashRate
                  }
  title _ = fromString (showSTypeable @child "watchdog-")
  update _me _init =
    \case
      Effectful.OnCall rt GetChild -> error "TODO"

instance Typeable child => Default (Stateful.Model (Watchdog child)) where
  def = WatchdogModel def (crashesPerSeconds 1 10)

data Watchdog (child :: Type) deriving Typeable

instance Typeable child => HasPdu (Watchdog child) where
  data Pdu (Watchdog child) r where
    GetChild :: Pdu (Watchdog child) ('Synchronous (Endpoint (Effectful.ServerPdu child)))
      deriving Typeable

instance NFData (Pdu (Watchdog child) r) where
  rnf GetChild = ()

instance Show (Pdu (Watchdog child) r) where
  showsPrec _ GetChild = showString "get-child"

type instance Supervisor.ChildId (Watchdog child) = Supervisor.ChildId child
