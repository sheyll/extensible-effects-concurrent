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
    -- ** /Protocol-Server/ Support Functions for building protocol servers
    module Control.Eff.Concurrent.Protocol.Wrapper
  ,
    -- ** /Observer/ Functions for Events and Event Listener
    module Control.Eff.Concurrent.Protocol.Observer
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

import           Control.Eff.Concurrent.Process
                                                ( Process(..)
                                                , SafeProcesses
                                                , Processes
                                                , HasProcesses
                                                , HasSafeProcesses
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
                                                , yieldProcess
                                                , sendMessage
                                                , sendAnyMessage
                                                , makeReference
                                                , receiveMessage
                                                , receiveSelectedMessage
                                                , flushMessages
                                                , receiveAnyMessage
                                                , receiveLoop
                                                , receiveSelectedLoop
                                                , receiveAnyLoop
                                                , MessageSelector(runMessageSelector)
                                                , selectMessage
                                                , filterMessage
                                                , selectMessageWith
                                                , selectDynamicMessage
                                                , selectAnyMessage
                                                , self
                                                , isProcessAlive
                                                , getProcessState
                                                , updateProcessDetails
                                                , spawn
                                                , spawn_
                                                , spawnLink
                                                , spawnRaw
                                                , spawnRaw_
                                                , ResumeProcess(..)
                                                , executeAndResume
                                                , executeAndResumeOrExit
                                                , executeAndResumeOrThrow
                                                , interrupt
                                                , sendInterrupt
                                                , exitBecause
                                                , exitNormally
                                                , exitWithError
                                                , sendShutdown -- TODO rename to 'sendExit'
                                                , linkProcess
                                                , unlinkProcess
                                                , monitor
                                                , demonitor
                                                , ProcessDown(..)
                                                , selectProcessDown
                                                , selectProcessDownByProcessId
                                                , becauseProcessIsDown
                                                , MonitorReference(..)
                                                , withMonitor
                                                , receiveWithMonitor
                                                , Interrupt(..)
                                                , Interrupts
                                                , interruptToExit
                                                , ExitRecovery(..)
                                                , RecoverableInterrupt
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
                                                , provideInterruptsShutdown
                                                , handleInterrupts
                                                , tryUninterrupted
                                                , exitOnInterrupt
                                                , logInterrupts
                                                , provideInterrupts
                                                , mergeEitherInterruptAndExitReason
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
                                                ( HasPdu(..)
                                                , Embeds
                                                , Pdu(..)
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
                                                , ObservationSink
                                                , IsObservable
                                                , CanObserve
                                                , Pdu(RegisterObserver, ForgetObserver, Observed)
                                                , registerObserver
                                                , forgetObserver
                                                , forgetObserverUnsafe
                                                , ObserverRegistry(..)
                                                , ObserverRegistryState
                                                , evalObserverRegistryState
                                                , emptyObserverRegistry
                                                , observerRegistryHandlePdu
                                                , observerRegistryRemoveProcess
                                                , observerRegistryNotify
                                                )
import           Control.Eff.Concurrent.Protocol.Wrapper
                                                ( Request(..)
                                                , sendReply
                                                , ReplyTarget(..)
                                                , replyTarget
                                                , embeddedReplyTarget
                                                , replyTargetOrigin
                                                , replyTargetSerializer
                                                , toEmbeddedReplyTarget
                                                , RequestOrigin(..)
                                                , embedRequestOrigin
                                                , toEmbeddedOrigin
                                                , Reply(..)
                                                , embedReplySerializer
                                                , makeRequestOrigin
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
