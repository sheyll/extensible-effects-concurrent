-- | Erlang style processes with message passing concurrency based on
-- (more) @extensible-effects@.
--
-- This module re-exports most of the library.
--
-- There are several /scheduler/ implementations to choose from.
--
-- This module re-exports "Control.Eff.Concurrent.Process.ForkIOScheduler".
--
-- To use another scheduler implementation, don't import this module, but instead
-- import one of:
--
-- * "Control.Eff.Concurrent.Pure"
-- * "Control.Eff.Concurrent.SingleThreaded"
--
module Control.Eff.Concurrent
  (
    -- * Concurrent Processes with Message Passing Concurrency
    module Control.Eff.Concurrent.Process
  ,
    -- * /Scheduler/ Process Effect Handler
    -- ** Concurrent Scheduler
    module Control.Eff.Concurrent.Process.ForkIOScheduler
  ,
    -- * Timers and Timeouts
    module Control.Eff.Concurrent.Process.Timer
  ,
    -- * Data Types and Functions for APIs (aka Protocols)
    module Control.Eff.Concurrent.Protocol
  ,
    -- ** /Client/ Functions for Consuming APIs
    module Control.Eff.Concurrent.Protocol.Client
  ,
    -- ** /Observer/ Functions for Events and Event Listener
    module Control.Eff.Concurrent.Protocol.Observer
  ,
    -- *** Capture /Observation/ in a FIFO Queue
    module Control.Eff.Concurrent.Protocol.Observer.Queue
  ,
    -- * Utilities
    -- ** Logging Effect
    module Control.Eff.Log
  ,
    -- ** Log Writer
    -- *** Asynchronous
    module Control.Eff.LogWriter.Async
  ,
    -- *** Console
    module Control.Eff.LogWriter.Console
  ,
    -- *** File
    module Control.Eff.LogWriter.File
  ,
    -- *** UDP
    module Control.Eff.LogWriter.UDP

  , -- *** Non-IO Log Message Capturing
    module Control.Eff.LogWriter.Capture

  , -- *** "Debug.Trace"
    module Control.Eff.LogWriter.DebugTrace

  , -- *** Generic IO
    module Control.Eff.LogWriter.IO

  , -- *** Unix Domain Socket
    module Control.Eff.LogWriter.UnixSocket
  ,
    -- ** Preventing Space Leaks
    module Control.Eff.Loop
  )
where

import           Control.Eff.Concurrent.Process ( Process(..)
                                                , ProcessTitle(..)
                                                , fromProcessTitle
                                                , ProcessDetails(..)
                                                , fromProcessDetails
                                                , StrictDynamic()
                                                , toStrictDynamic
                                                , fromStrictDynamic
                                                , unwrapStrictDynamic
                                                , Serializer(..)
                                                , ProcessId(..)
                                                , fromProcessId
                                                , SafeProcesses
                                                , ResumeProcess(..)
                                                , ProcessState(..)
                                                , yieldProcess
                                                , sendMessage
                                                , sendAnyMessage
                                                , sendShutdown
                                                , sendInterrupt
                                                , makeReference
                                                , getProcessState
                                                , updateProcessDetails
                                                , receiveAnyMessage
                                                , receiveMessage
                                                , receiveSelectedMessage
                                                , flushMessages
                                                , receiveAnyLoop
                                                , receiveLoop
                                                , receiveSelectedLoop
                                                , MessageSelector
                                                  ( runMessageSelector
                                                  )
                                                , selectMessage
                                                , selectMessage
                                                , filterMessage
                                                , filterMessage
                                                , selectMessageWith
                                                , selectMessageWith
                                                , selectDynamicMessage
                                                , selectDynamicMessage
                                                , selectAnyMessage
                                                , self
                                                , isProcessAlive
                                                , spawn
                                                , spawn_
                                                , spawnLink
                                                , spawnRaw
                                                , spawnRaw_
                                                , exitBecause
                                                , exitNormally
                                                , exitWithError
                                                , linkProcess
                                                , unlinkProcess
                                                , monitor
                                                , demonitor
                                                , ProcessDown(..)
                                                , selectProcessDown
                                                , becauseProcessIsDown
                                                , MonitorReference(..)
                                                , withMonitor
                                                , receiveWithMonitor
                                                , provideInterruptsShutdown
                                                , handleInterrupts
                                                , tryUninterrupted
                                                , exitOnInterrupt
                                                , logInterrupts
                                                , provideInterrupts
                                                , mergeEitherInterruptAndExitReason
                                                , interrupt
                                                , executeAndResume
                                                , executeAndResumeOrExit
                                                , executeAndResumeOrThrow
                                                , Interrupt(..)
                                                , interruptToExit
                                                , ExitRecovery(..)
                                                , RecoverableInterrupt
                                                , Interrupts
                                                , Processes
                                                , ExitSeverity(..)
                                                , SomeExitReason(SomeExitReason)
                                                , toExitRecovery
                                                , isRecoverable
                                                , toExitSeverity
                                                , isProcessDownInterrupt
                                                , isCrash
                                                , toCrashReason
                                                , fromSomeExitReason
                                                , logProcessExit
                                                )
import           Control.Eff.Concurrent.Process.Timer
                                                ( Timeout(fromTimeoutMicros)
                                                , TimerReference()
                                                , TimerElapsed(fromTimerElapsed)
                                                , sendAfter
                                                , startTimer
                                                , selectTimerElapsed
                                                , cancelTimer
                                                , receiveAfter
                                                , receiveSelectedAfter
                                                , receiveSelectedWithMonitorAfter
                                                )

import           Control.Eff.Concurrent.Protocol
                                                ( Pdu(..)
                                                , Synchronicity(..)
                                                , ProtocolReply
                                                , Tangible
                                                , TangiblePdu
                                                , Endpoint(..)
                                                , fromEndpoint
                                                , proxyAsEndpoint
                                                , asEndpoint
                                                , EmbedProtocol(..)
                                                , toEmbeddedEndpoint
                                                , fromEmbeddedEndpoint
                                                )
import           Control.Eff.Concurrent.Protocol.Client
                                                ( cast
                                                , call
                                                , callWithTimeout
                                                , castSingleton
                                                , castEndpointReader
                                                , callSingleton
                                                , callEndpointReader
                                                , ServesProtocol
                                                , runEndpointReader
                                                , askEndpoint
                                                , EndpointReader
                                                )
import           Control.Eff.Concurrent.Protocol.Observer
                                                ( Observer(..)
                                                , Pdu
                                                  ( RegisterObserver
                                                  , ForgetObserver
                                                  , Observed
                                                  )
                                                , registerObserver
                                                , forgetObserver
                                                , handleObservations
                                                , toObserver
                                                , toObserverFor
                                                , ObserverRegistry
                                                , ObserverState
                                                , Observers()
                                                , emptyObservers
                                                , handleObserverRegistration
                                                , manageObservers
                                                , observed
                                                )
import           Control.Eff.Concurrent.Protocol.Observer.Queue
                                                ( ObservationQueue()
                                                , ObservationQueueReader
                                                , readObservationQueue
                                                , tryReadObservationQueue
                                                , flushObservationQueue
                                                , withObservationQueue
                                                , spawnLinkObservationQueueWriter
                                                )
import           Control.Eff.Concurrent.Process.ForkIOScheduler
                                                ( schedule
                                                , defaultMain
                                                , defaultMainWithLogWriter
                                                , SafeEffects
                                                , Effects
                                                , BaseEffects
                                                , HasBaseEffects
                                                )
import           Control.Eff.Log
import           Control.Eff.LogWriter.Async
import           Control.Eff.LogWriter.Capture
import           Control.Eff.LogWriter.Console
import           Control.Eff.LogWriter.DebugTrace
import           Control.Eff.LogWriter.File
import           Control.Eff.LogWriter.IO
import           Control.Eff.LogWriter.UDP
import           Control.Eff.LogWriter.UnixSocket
import           Control.Eff.Loop
