-- | Concurrent, communicating processes, executed using a single-threaded
-- scheduler, with support for 'IO' and 'Logs'.
--
-- This module re-exports most of the library.
--
-- There are several /scheduler/ implementations to choose from.
--
-- This module re-exports the impure parts of "Control.Eff.Concurrent.Process.SingleThreadedScheduler".
--
-- To use another scheduler implementation, don't import this module, but instead
-- import one of:
--
-- * "Control.Eff.Concurrent"
-- * "Control.Eff.Concurrent.Pure"
--
-- @since 0.25.0
module Control.Eff.Concurrent.SingleThreaded
  ( -- * Generic functions and type for Processes and Messages
    module Control.Eff.Concurrent,

    -- * Scheduler
    schedule,
    defaultMain,
    defaultMainWithLogWriter,
    Effects,
    SafeEffects,
    BaseEffects,
    HasBaseEffects,

    -- * External Libraries
  )
where

import Control.Eff
import Control.Eff.Concurrent hiding
  ( BaseEffects,
    Effects,
    HasBaseEffects,
    SafeEffects,
    defaultMain,
    defaultMainWithLogWriter,
    schedule,
  )
import Control.Eff.Concurrent.Process.SingleThreadedScheduler
import GHC.Stack (HasCallStack)

-- | Run the 'Effects' using a single threaded, coroutine based, scheduler
-- from "Control.Eff.Concurrent.Process.SingleThreadedScheduler".
--
-- @since 0.25.0
schedule ::
  HasCallStack =>
  LogWriter ->
  Eff Effects a ->
  IO (Either ShutdownReason a)
schedule = scheduleIOWithLogging

-- | The effect list for 'Process' effects in the single threaded scheduler.
--
-- See 'EffectsIo'
--
-- @since 0.25.0
type Effects = EffectsIo

-- | The effect list for 'Process' effects in the single threaded scheduler.
--  This is like 'SafeProcesses', no 'Interrupts' are present.
--
-- See 'SafeEffectsIo'
--
-- @since 0.25.0
type SafeEffects = SafeEffectsIo

-- | The effect list for the underlying scheduler.
--
-- See 'BaseEffectsIo'
--
-- @since 0.25.0
type BaseEffects = BaseEffectsIo

-- | Constraint for the existence of the underlying scheduler effects.
--
-- See 'HasBaseEffectsIo'
--
-- @since 0.25.0
type HasBaseEffects e = HasBaseEffectsIo e
