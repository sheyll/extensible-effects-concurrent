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
    -- ** Preventing Space Leaks
    module Control.Eff.Loop
  )
where

import           Control.Eff.Concurrent.Process ( Process(..)
                                                , ProcessId(..)
                                                , fromProcessId
                                                , ConsProcess
                                                , ResumeProcess(..)
                                                , SchedulerProxy(..)
                                                , HasScheduler
                                                , getSchedulerProxy
                                                , withSchedulerProxy
                                                , thisSchedulerProxy
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
                                                , selectMessageLazy
                                                , selectMessageProxy
                                                , selectMessageProxyLazy
                                                , filterMessage
                                                , filterMessageLazy
                                                , selectMessageWith
                                                , selectMessageWithLazy
                                                , selectDynamicMessage
                                                , selectDynamicMessageLazy
                                                , selectAnyMessageLazy
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
                                                , exitOnInterrupt
                                                , logInterrupts
                                                , provideInterrupts
                                                , mergeEitherInterruptAndExitReason
                                                , interrupt
                                                , executeAndResume
                                                , executeAndResumeOrExit
                                                , executeAndResumeOrThrow
                                                , ExitReason(..)
                                                , ExitRecovery(..)
                                                , InterruptReason
                                                , Interrupts
                                                , InterruptableProcess
                                                , ExitSeverity(..)
                                                , SomeExitReason(SomeExitReason)
                                                , toExitRecovery
                                                , isRecoverable
                                                , toExitSeverity
                                                , isBecauseDown
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
                                                , receiveAfter
                                                , receiveSelectedAfter
                                                )

import           Control.Eff.Concurrent.Api     ( Api
                                                , Synchronicity(..)
                                                , Server(..)
                                                , fromServer
                                                , proxyAsServer
                                                , asServer
                                                )
import           Control.Eff.Concurrent.Api.Client
                                                ( cast
                                                , call
                                                , castRegistered
                                                , callRegistered
                                                , ServesApi
                                                , registerServer
                                                , whereIsServer
                                                , ServerReader
                                                )
import           Control.Eff.Concurrent.Api.Server
                                                ( serve
                                                , spawnServer
                                                , spawnServerWithEffects
                                                , ApiHandler(..)
                                                , castCallback
                                                , callCallback
                                                , terminateCallback
                                                , apiHandler
                                                , apiHandlerForever
                                                , castHandler
                                                , castHandlerForever
                                                , callHandler
                                                , callHandlerForever
                                                , castAndCallHandler
                                                , castAndCallHandlerForever
                                                , ApiServerCmd(..)
                                                , unhandledCallError
                                                , unhandledCastError
                                                , defaultTermination
                                                , Servable(..)
                                                , ServerCallback(..)
                                                , requestHandlerSelector
                                                , terminationHandler
                                                )
import           Control.Eff.Concurrent.Api.Observer
                                                ( Observer(..)
                                                , Observable(..)
                                                , ObserverState
                                                , notifyObserver
                                                , registerObserver
                                                , forgetObserver
                                                , SomeObserver(..)
                                                , notifySomeObserver
                                                , Observers()
                                                , manageObservers
                                                , addObserver
                                                , removeObserver
                                                , notifyObservers
                                                , CallbackObserver
                                                , spawnCallbackObserver
                                                , spawnLoggingObserver
                                                )
import           Control.Eff.Concurrent.Api.Observer.Queue
                                                ( ObservationQueue()
                                                , ObservationQueueReader
                                                , readObservationQueue
                                                , tryReadObservationQueue
                                                , flushObservationQueue
                                                , enqueueObservationsRegistered
                                                , enqueueObservations
                                                )
import           Control.Eff.Concurrent.Process.ForkIOScheduler
                                                ( schedule
                                                , defaultMain
                                                , defaultMainWithLogChannel
                                                , ProcEff
                                                , InterruptableProcEff
                                                , SchedulerIO
                                                , HasSchedulerIO
                                                , forkIoScheduler
                                                )

import           Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                                ( schedulePure
                                                , defaultMainSingleThreaded
                                                , singleThreadedIoScheduler
                                                )
import           Control.Eff.Log
import           Control.Eff.Loop
