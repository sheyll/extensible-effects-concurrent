-- | Erlang style processes with message passing concurrency based on
-- (more) @extensible-effects@.
module Control.Eff.Concurrent
  (
    -- * Concurrent Processes with Message Passing Concurrency
    module Control.Eff.Concurrent.Process
  ,
    -- * Timers and Timeouts
    module Control.Eff.Concurrent.Process.Timer
  ,
    -- * Data Types and Functions for APIs (aka Protocols)
    module Control.Eff.Concurrent.Api
  ,
    -- ** /Client/ Functions for Consuming APIs
    module Control.Eff.Concurrent.Api.Client
  ,
    -- ** /Server/ Functions for Providing APIs
    module Control.Eff.Concurrent.Api.Server
  ,
    -- ** Encapsulate 'Api's 'Cast's as well as 'Call's and their 'Reply's
    module Control.Eff.Concurrent.Api.Request
  ,
    -- ** /Observer/ Functions for Events and Event Listener
    module Control.Eff.Concurrent.Api.Observer
  ,
    -- *** Capture /Observation/ in a FIFO Queue
    module Control.Eff.Concurrent.Api.Observer.Queue
  ,
    -- * /Scheduler/ Process Effect Handler
    -- ** Concurrent Scheduler
    module Control.Eff.Concurrent.Process.ForkIOScheduler
  ,
    -- ** Single Threaded Scheduler
    module Control.Eff.Concurrent.Process.SingleThreadedScheduler
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
                                                , StrictDynamic()
                                                , toStrictDynamic
                                                , fromStrictDynamic
                                                , unwrapStrictDynamic
                                                , ProcessId(..)
                                                , fromProcessId
                                                , ConsProcess
                                                , ResumeProcess(..)
                                                , ProcessState(..)
                                                , yieldProcess
                                                , sendMessage
                                                , sendAnyMessage
                                                , sendShutdown
                                                , sendInterrupt
                                                , makeReference
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
                                                , InterruptableProcess
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

import           Control.Eff.Concurrent.Api     ( Api
                                                , Synchronicity(..)
                                                , Server(..)
                                                , Tangible
                                                , fromServer
                                                , proxyAsServer
                                                , asServer
                                                )
import           Control.Eff.Concurrent.Api.Client
                                                ( cast
                                                , call
                                                , callWithTimeout
                                                , castRegistered
                                                , callRegistered
                                                , ServesApi
                                                , registerServer
                                                , whereIsServer
                                                , ServerReader
                                                )
import           Control.Eff.Concurrent.Api.Request
                                                ( Request(..)
                                                , Reply(..)
                                                , mkRequestOrigin
                                                , RequestOrigin(..)
                                                , sendReply
                                                )
import           Control.Eff.Concurrent.Api.Observer
                                                ( Observer(..)
                                                , Api
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
                                                , handleObserverRegistration
                                                , manageObservers
                                                , observed
                                                )
import           Control.Eff.Concurrent.Api.Observer.Queue
                                                ( ObservationQueue()
                                                , ObservationQueueReader
                                                , readObservationQueue
                                                , tryReadObservationQueue
                                                , flushObservationQueue
                                                , withObservationQueue
                                                , spawnLinkObservationQueueWriter
                                                )
import           Control.Eff.Concurrent.Api.Server
                                                ( spawnApiServer
                                                , spawnLinkApiServer
                                                , spawnApiServerStateful
                                                , spawnApiServerEffectful
                                                , spawnLinkApiServerEffectful
                                                , CallbackResult(..)
                                                , MessageCallback(..)
                                                , handleCasts
                                                , handleCalls
                                                , handleCastsAndCalls
                                                , handleCallsDeferred
                                                , handleMessages
                                                , handleSelectedMessages
                                                , handleAnyMessages
                                                , handleProcessDowns
                                                , dropUnhandledMessages
                                                , exitOnUnhandled
                                                , logUnhandledMessages
                                                , (^:)
                                                , fallbackHandler
                                                , ToServerPids(..)
                                                , InterruptCallback(..)
                                                , stopServerOnInterrupt
                                                )
import           Control.Eff.Concurrent.Api.Supervisor
                                                ( Sup()
                                                , SpawnFun
                                                , SupConfig(MkSupConfig)
                                                , supConfigChildStopTimeout
                                                , supConfigSpawnFun
                                                , SpawnErr(AlreadyStarted)
                                                , startSupervisor
                                                , stopSupervisor
                                                , isSupervisorAlive
                                                , monitorSupervisor
                                                , getDiagnosticInfo
                                                , spawnChild
                                                , lookupChild
                                                , stopChild
                                                )
import           Control.Eff.Concurrent.Process.ForkIOScheduler
                                                ( schedule
                                                , defaultMain
                                                , defaultMainWithLogWriter
                                                , ProcEff
                                                , InterruptableProcEff
                                                , SchedulerIO
                                                , HasSchedulerIO
                                                )

import           Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                                ( schedulePure
                                                , defaultMainSingleThreaded
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
