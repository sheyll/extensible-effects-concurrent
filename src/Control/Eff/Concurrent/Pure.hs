-- | Concurrent, communicating processes, executed using a pure, single-threaded scheduler.
--
-- This module re-exports most of the library.
--
-- There are several /scheduler/ implementations to choose from.
--
-- This module re-exports the pure parts of "Control.Eff.Concurrent.Process.SingleThreadedScheduler".
--
-- To use another scheduler implementation, don't import this module, but instead
-- import one of:
--
-- * "Control.Eff.Concurrent"
-- * "Control.Eff.Concurrent.SingleThreaded"
--
-- @since 0.25.0
module Control.Eff.Concurrent.Pure
  ( -- * Generic functions and type for Processes and Messages
    module Control.Eff.Concurrent
    -- * Scheduler
  , schedule
  , Effects
  , SafeEffects
  , BaseEffects
  , HasBaseEffects
  -- * External Libraries
  ) where

import Control.Eff
import Control.Eff.Concurrent                  hiding
                                                ( schedule
                                                , defaultMain
                                                , defaultMainWithLogWriter
                                                , Effects
                                                , SafeEffects
                                                , BaseEffects
                                                , HasBaseEffects
                                                )

import Control.Eff.Concurrent.Process.SingleThreadedScheduler

-- | Run the 'Effects' using a single threaded, coroutine based, pure scheduler
-- from "Control.Eff.Concurrent.Process.SingleThreadedScheduler".
--
-- @since 0.25.0
schedule :: Eff Effects a -> Either ShutdownReason a
schedule = schedulePure

-- | The effect list for 'Process' effects in the single threaded pure scheduler.
--
-- See 'PureEffects'
--
-- @since 0.25.0
type Effects = PureEffects

-- | The effect list for 'Process' effects in the single threaded pure scheduler.
--  This is like 'SafeProcesses', no 'Interrupts' are present.
--
-- See 'PureSafeEffects'
--
-- @since 0.25.0
type SafeEffects = PureSafeEffects

-- | The effect list for the underlying scheduler.
--
-- @since 0.25.0
type BaseEffects = PureBaseEffects

-- | Constraint for the existence of the underlying scheduler effects.
--
-- @since 0.25.0
type HasBaseEffects e = HasPureBaseEffects e
