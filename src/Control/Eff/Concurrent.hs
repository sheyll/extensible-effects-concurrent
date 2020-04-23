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
module Control.Eff.Concurrent
  ( -- * Concurrent Processes with Message Passing Concurrency
    module Control.Eff.Concurrent.Process,

    -- * /Scheduler/ Process Effect Handler

    -- ** Concurrent Scheduler
    module Control.Eff.Concurrent.Process.ForkIOScheduler,

    -- * Timers and Timeouts
    module Control.Eff.Concurrent.Process.Timer,

    -- * Data Types and Functions for APIs (aka Protocols)
    module Control.Eff.Concurrent.Protocol,

    -- ** /Client/ Functions for Consuming APIs
    module Control.Eff.Concurrent.Protocol.Client,

    -- ** /Protocol-Server/ Support Functions for building protocol servers
    module Control.Eff.Concurrent.Protocol.Wrapper,

    -- ** /Observer/ Functions for Events and Event Listener
    module Control.Eff.Concurrent.Protocol.Observer,

    -- * Utilities

    -- ** FilteredLogging Effect
    module Control.Eff.Log,

    -- ** Log Writer

    -- *** Asynchronous
    module Control.Eff.LogWriter.Async,

    -- *** Console
    module Control.Eff.LogWriter.Console,

    -- *** File
    module Control.Eff.LogWriter.File,

    -- *** UDP
    module Control.Eff.LogWriter.UDP,

    -- *** "Debug.Trace"
    module Control.Eff.LogWriter.DebugTrace,

    -- *** Generic IO
    module Control.Eff.LogWriter.Rich,

    -- *** Unix Domain Socket
    module Control.Eff.LogWriter.UnixSocket,

    -- ** Preventing Space Leaks
    module Control.Eff.Loop,
  )
where

import Control.Eff.Concurrent.Process
  ( -- TODO rename to 'sendExit'

    ExitSeverity (..),
    HasProcesses,
    HasSafeProcesses,
    InterruptOrShutdown (..),
    InterruptReason (..),
    Interrupts,
    MessageSelector (runMessageSelector),
    MonitorReference (..),
    Process (..),
    ProcessDetails (..),
    ProcessDown (..),
    ProcessId (..),
    ProcessTitle (..),
    Processes,
    ResumeProcess (..),
    SafeProcesses,
    Serializer (..),
    ShutdownReason (..),
    StrictDynamic (),
    Timeout (TimeoutMicros, fromTimeoutMicros),
    UnhandledProcessExit (..),
    UnhandledProcessInterrupt (..),
    becauseOtherProcessNotRunning,
    delay,
    demonitor,
    executeAndResume,
    executeAndResumeOrExit,
    executeAndResumeOrThrow,
    exitBecause,
    exitNormally,
    exitOnInterrupt,
    exitWithError,
    filterMessage,
    flushMessages,
    fromProcessDetails,
    fromProcessId,
    fromProcessTitle,
    fromStrictDynamic,
    getProcessState,
    handleInterrupts,
    interrupt,
    interruptToExit,
    isCrash,
    isLinkedProcessCrashed,
    isProcessAlive,
    linkProcess,
    logInterrupts,
    logProcessExit,
    makeReference,
    mergeEitherInterruptAndExitReason,
    monitor,
    provideInterrupts,
    provideInterruptsShutdown,
    receiveAnyLoop,
    receiveAnyMessage,
    receiveLoop,
    receiveMessage,
    receiveSelectedLoop,
    receiveSelectedMessage,
    receiveWithMonitor,
    selectAnyMessage,
    selectDynamicMessage,
    selectMessage,
    selectMessageWith,
    selectProcessDown,
    selectProcessDownByProcessId,
    self,
    sendAnyMessage,
    sendInterrupt,
    sendMessage,
    sendShutdown,
    spawn,
    spawnLink,
    spawnRaw,
    spawnRaw_,
    spawn_,
    toCrashReason,
    toExitSeverity,
    toProcessTitle,
    toStrictDynamic,
    tryUninterrupted,
    unlinkProcess,
    unwrapStrictDynamic,
    updateProcessDetails,
    withMonitor,
    yieldProcess,
  )
import Control.Eff.Concurrent.Process.ForkIOScheduler
  ( BaseEffects,
    Effects,
    HasBaseEffects,
    SafeEffects,
    defaultMain,
    defaultMainWithLogWriter,
    schedule,
  )
import Control.Eff.Concurrent.Process.Timer
  ( TimerElapsed (fromTimerElapsed),
    TimerReference (),
    cancelTimer,
    receiveAfter,
    receiveAfterWithTitle,
    receiveSelectedAfter,
    receiveSelectedAfterWithTitle,
    receiveSelectedWithMonitorAfter,
    receiveSelectedWithMonitorAfterWithTitle,
    selectTimerElapsed,
    sendAfter,
    sendAfterWithTitle,
    startTimer,
    startTimerWithTitle,
  )
import Control.Eff.Concurrent.Protocol
  ( Embeds,
    Endpoint (..),
    HasPdu (..),
    HasPduPrism (..),
    Pdu (..),
    ProtocolReply,
    Synchronicity (..),
    Tangible,
    TangiblePdu,
    asEndpoint,
    fromEmbeddedEndpoint,
    fromEndpoint,
    proxyAsEndpoint,
    toEmbeddedEndpoint,
  )
import Control.Eff.Concurrent.Protocol.Client
  ( EndpointReader,
    HasEndpointReader,
    askEndpoint,
    call,
    callEndpointReader,
    callSingleton,
    callWithTimeout,
    cast,
    castEndpointReader,
    castSingleton,
    runEndpointReader,
  )
import Control.Eff.Concurrent.Protocol.Observer
  ( CanObserve,
    IsObservable,
    ObservationSink,
    Observer (..),
    ObserverRegistry (..),
    ObserverRegistryState,
    Pdu (ForgetObserver, Observed, RegisterObserver),
    emptyObserverRegistry,
    evalObserverRegistryState,
    forgetObserver,
    forgetObserverUnsafe,
    observerRegistryHandlePdu,
    observerRegistryNotify,
    observerRegistryRemoveProcess,
    registerObserver,
  )
import Control.Eff.Concurrent.Protocol.Wrapper
  ( Reply (..),
    ReplyTarget (..),
    Request (..),
    RequestOrigin (..),
    embedReplySerializer,
    embedRequestOrigin,
    embeddedReplyTarget,
    makeRequestOrigin,
    replyTarget,
    replyTargetOrigin,
    replyTargetSerializer,
    sendReply,
    toEmbeddedOrigin,
    toEmbeddedReplyTarget,
  )
import Control.Eff.Log
import Control.Eff.LogWriter.Async
import Control.Eff.LogWriter.Console
import Control.Eff.LogWriter.DebugTrace
import Control.Eff.LogWriter.File
import Control.Eff.LogWriter.Rich
import Control.Eff.LogWriter.UDP
import Control.Eff.LogWriter.UnixSocket
import Control.Eff.Loop
