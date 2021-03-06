{-# LANGUAGE ImplicitParams #-}

-- | The message passing effect.
--
-- This module describes an abstract message passing effect, and a process
-- effect, mimicking Erlang's process and message semantics.
--
-- Two __scheduler__ implementations for the 'Process' effect are provided:
--
--  * A scheduler using @forkIO@, i.e. relying on the multi threaded GHC runtime:
--    "Control.Eff.Concurrent.Process.ForkIOScheduler"
--
--  * And a /pure/(rer) coroutine based scheduler in:
--    "Control.Eff.Concurrent.Process.SingleThreadedScheduler"
module Control.Eff.Concurrent.Process
  ( -- * Process Effect
    Process (..),

    -- ** Process Effect Aliases
    SafeProcesses,
    Processes,

    -- ** Process Effect Constraints
    HasProcesses,
    HasSafeProcesses,

    -- ** Process Info
    ProcessTitle (..),
    fromProcessTitle,
    toProcessTitle,
    ProcessDetails (..),
    fromProcessDetails,

    -- ** Process Timer
    Timeout (TimeoutMicros, fromTimeoutMicros),

    -- ** Message Data
    Message (),
    toMessage,
    fromMessage,
    unwrapMessage,
    Serializer (..),

    -- ** ProcessId Type
    ProcessId (..),
    fromProcessId,

    -- ** Process State
    ProcessState (..),

    -- ** Yielding
    yieldProcess,

    -- ** Waiting/Sleeping
    delay,

    -- ** Sending Messages
    sendMessage,
    sendAnyMessage,

    -- ** Utilities
    makeReference,

    -- ** Receiving Messages
    receiveMessage,
    receiveSelectedMessage,
    flushMessages,
    receiveAnyMessage,
    receiveLoop,
    receiveSelectedLoop,
    receiveAnyLoop,

    -- ** Selecting Messages to Receive
    MessageSelector (runMessageSelector),
    selectMessage,
    filterMessage,
    selectMessageWith,
    selectDynamicMessage,
    selectAnyMessage,

    -- ** Process State Reflection
    self,
    isProcessAlive,
    getProcessState,
    updateProcessDetails,

    -- ** Spawning
    spawn,
    spawn_,
    spawnLink,
    spawnRaw,
    spawnRaw_,

    -- ** Process Operation Execution
    ResumeProcess (..),
    executeAndResume,
    executeAndResumeOrExit,
    executeAndResumeOrThrow,

    -- ** Exits and Interrupts

    -- *** Interrupting Processes
    interrupt,
    sendInterrupt,

    -- *** Exiting Processes
    exitBecause,
    exitNormally,
    exitWithError,
    sendShutdown, -- TODO rename to 'sendExit'

    -- *** Linking Processes
    linkProcess,
    unlinkProcess,

    -- *** Monitor Processes
    monitor,
    demonitor,
    ProcessDown (..),
    selectProcessDown,
    selectProcessDownByProcessId,
    becauseOtherProcessNotRunning,
    MonitorReference (..),
    monitorIndex,
    monitoredProcess,
    withMonitor,
    receiveWithMonitor,

    -- *** Exit and Interrupt Reasons
    InterruptReason (..),
    ShutdownReason (..),
    Interrupts,
    interruptToExit,
    ExitSeverity (..),
    InterruptOrShutdown (..),
    UnhandledProcessInterrupt (..),
    UnhandledProcessExit (..),
    toExitSeverity,
    isLinkedProcessCrashed,
    isCrash,
    toCrashReason,
    logProcessExit,

    -- ** Control Flow

    -- *** Process Interrupt Recoverable Handling
    provideInterruptsShutdown,
    handleInterrupts,
    tryUninterrupted,
    exitOnInterrupt,
    logInterrupts,
    provideInterrupts,
    mergeEitherInterruptAndExitReason,
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Eff
import Control.Eff.Exception
import Control.Eff.Extend
import Control.Eff.Log.Handler
import Control.Eff.Log.Message
import qualified Control.Exception as Exc
import Control.Lens
import Control.Monad
  ( void,
    (>=>),
  )
import Data.Default
import Data.Dynamic
import Data.Functor.Contravariant ()
import Data.Kind
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
  ( Generic,
    Generic1,
  )
import GHC.Stack

-- | The process effect is the basis for message passing concurrency. This
-- effect describes an interface for concurrent, communicating isolated
-- processes identified uniquely by a process-id.
--
-- Processes can raise exceptions that can be caught, exit gracefully or with an
-- error, or be killed by other processes, with the option of ignoring the
-- shutdown request.
--
-- Process Scheduling is implemented in different modules. All scheduler
-- implementations should follow some basic rules:
--
-- * fair scheduling
--
-- * sending a message does not block
--
-- * receiving a message does block
--
-- * spawning a child blocks only a very moment
--
-- * a newly spawned process shall be scheduled before the parent process after
-- * the spawnRaw
--
-- * when the first process exists, all process should be killed immediately
data Process (r :: [Type -> Type]) b where
  -- | Remove all messages from the process' message queue
  FlushMessages :: Process r (ResumeProcess [Message])
  -- | In cooperative schedulers, this will give processing time to the
  -- scheduler. Every other operation implicitly serves the same purpose.
  --
  -- @since 0.12.0
  YieldProcess :: Process r (ResumeProcess ())
  -- | Simply wait until the time in the given 'Timeout' has elapsed and return.
  --
  -- @since 0.30.0
  Delay :: Timeout -> Process r (ResumeProcess ())
  -- | Return the current 'ProcessId'
  SelfPid :: Process r (ResumeProcess ProcessId)
  -- | Start a new process, the new process will execute an effect, the function
  -- will return immediately with a 'ProcessId'.
  Spawn :: ProcessTitle -> Eff (Process r ': r) () -> Process r (ResumeProcess ProcessId)
  -- | Start a new process, and 'Link' to it .
  --
  -- @since 0.12.0
  SpawnLink :: ProcessTitle -> Eff (Process r ': r) () -> Process r (ResumeProcess ProcessId)
  -- | Shutdown the process; irregardless of the exit reason, this function never
  -- returns,
  Shutdown :: ShutdownReason -> Process r a
  -- | Shutdown another process immediately, the other process has no way of handling this!
  SendShutdown :: ProcessId -> ShutdownReason -> Process r (ResumeProcess ())
  -- | Request that another a process interrupts. The targeted process is interrupted
  -- and gets an 'Interrupted', the target process may decide to ignore the
  -- interrupt and continue as if nothing happened.
  SendInterrupt :: ProcessId -> InterruptReason -> Process r (ResumeProcess ())
  -- | Send a message to a process addressed by the 'ProcessId'. Sending a
  -- message should __always succeed__ and return __immediately__, even if the
  -- destination process does not exist, or does not accept messages of the
  -- given type.
  SendMessage :: ProcessId -> Message -> Process r (ResumeProcess ())
  -- | Receive a message that matches a criteria.
  -- This should block until an a message was received. The message is returned
  -- as a 'ResumeProcess' value. The function should also return if an exception
  -- was caught or a shutdown was requested.
  ReceiveSelectedMessage :: forall r a. MessageSelector a -> Process r (ResumeProcess a)
  -- | Generate a unique 'Int' for the current process.
  MakeReference :: Process r (ResumeProcess Int)
  -- | Monitor another process. When the monitored process exits a
  --  'ProcessDown' is sent to the calling process.
  -- The return value is a unique identifier for that monitor.
  -- There can be multiple monitors on the same process,
  -- and a message for each will be sent.
  -- If the process is already dead, the 'ProcessDown' message
  -- will be sent immediately, without exit reason
  --
  -- @since 0.12.0
  Monitor :: ProcessId -> Process r (ResumeProcess MonitorReference)
  -- | Remove a monitor.
  --
  -- @since 0.12.0
  Demonitor :: MonitorReference -> Process r (ResumeProcess ())
  -- | Connect the calling process to another process, such that
  -- if one of the processes crashes (i.e. 'isCrash' returns 'True'), the other
  -- is shutdown with the 'Interrupt' 'LinkedProcessCrashed'.
  --
  -- You might wonder: Why not tearing down the linked process when exiting
  -- normally?
  -- I thought about this. If a process exits normally, it should have the
  -- opportunity to shutdown stuff explicitly.
  -- And if you want to make sure that there are no dangling child processes
  -- after e.g. a broker crash, you can always use 'monitor'.
  --
  -- @since 0.12.0
  Link :: ProcessId -> Process r (ResumeProcess ())
  -- | Unlink the calling process from the other process.
  --
  -- See 'Link' for a discussion on linking.
  --
  -- @since 0.12.0
  Unlink :: ProcessId -> Process r (ResumeProcess ())
  -- | Update the 'ProcessDetails' of a process
  UpdateProcessDetails :: ProcessDetails -> Process r (ResumeProcess ())
  -- | Get the 'ProcessState' (or 'Nothing' if the process is dead)
  GetProcessState :: ProcessId -> Process r (ResumeProcess (Maybe (ProcessTitle, ProcessDetails, ProcessState)))

-- | A short title for a 'Process' for logging purposes.
--
-- @since 0.24.1
newtype ProcessTitle = MkProcessTitle {_fromProcessTitle :: LogMsg}
  deriving (Eq, Ord, NFData, Generic, IsString, Typeable, Semigroup, Monoid)

-- | Construct a 'ProcessTitle' from a 'String'.
--
-- @since 1.0.0
toProcessTitle :: String -> ProcessTitle
toProcessTitle = fromString

-- | An isomorphism lens for the 'ProcessTitle'
--
-- @since 0.24.1
fromProcessTitle :: Lens' ProcessTitle LogMsg
fromProcessTitle = iso _fromProcessTitle MkProcessTitle

instance ToLogMsg ProcessTitle where
  toLogMsg = _fromProcessTitle

instance Show ProcessTitle where
  showsPrec _ (MkProcessTitle (MkLogMsg t)) = showString (T.unpack t)

-- | A multi-line text describing the __current__
-- state of a process for debugging purposes.
--
-- @since 0.24.1
newtype ProcessDetails = MkProcessDetails {_fromProcessDetails :: Text}
  deriving (Eq, Ord, NFData, Generic, IsString, Typeable, Semigroup, Monoid)

-- | An isomorphism lens for the 'ProcessDetails'
--
-- @since 0.24.1
fromProcessDetails :: Lens' ProcessDetails Text
fromProcessDetails = iso _fromProcessDetails MkProcessDetails

instance Show ProcessDetails where
  showsPrec _ (MkProcessDetails t) = showString (T.unpack t)

-- | A number of micro seconds.
--
-- @since 0.12.0
newtype Timeout = TimeoutMicros {fromTimeoutMicros :: Int}
  deriving (NFData, Ord, Eq, Num, Integral, Real, Enum, Typeable, Show)

instance ToLogMsg Timeout where
  toLogMsg (TimeoutMicros u) = packLogMsg (show u ++ "µs")

-- | Data flows between 'Process'es via these messages.
--
-- This is just a newtype wrapper around 'Dynamic'.
-- The reason this type exists is to force construction through the code in this
-- module, which always evaluates a message to /normal form/ __before__
-- sending it to another process.
--
-- @since 0.22.0
newtype Message = MkMessage Dynamic
  deriving (Typeable, Show)

instance ToTypeLogMsg Message where
  toTypeLogMsg _ = packLogMsg "Message"

instance NFData Message where
  rnf (MkMessage d) = d `seq` ()

instance ToLogMsg Message where
  toLogMsg (MkMessage d) = packLogMsg (show d)

-- | Serialize a @message@ into a 'Message' value to be sent via 'sendAnyMessage'.
--
-- This indirection allows, among other things, the composition of
-- 'Control.Eff.Concurrent.Protocol.Effectful.Server's.
--
-- @since 0.24.1
newtype Serializer message = MkSerializer
  { runSerializer :: message -> Message
  }
  deriving (Typeable)

instance NFData (Serializer message) where
  rnf (MkSerializer !s) = s `seq` ()

instance Contravariant Serializer where
  contramap f (MkSerializer b) = MkSerializer (b . f)

-- | Deeply evaluate the given value and wrap it into a 'Message'.
--
-- @since 0.22.0
toMessage :: (Typeable a, NFData a) => a -> Message
toMessage x = force x `seq` toDyn (force x) `seq` MkMessage (toDyn (force x))

-- | Convert a 'Message' back to a value.
--
-- @since 0.22.0
fromMessage :: Typeable a => Message -> Maybe a
fromMessage (MkMessage d) = fromDynamic d

-- | Convert a 'Message' back to an unwrapped 'Dynamic'.
--
-- @since 0.22.0
unwrapMessage :: Message -> Dynamic
unwrapMessage (MkMessage d) = d

-- | Every 'Process' action returns it's actual result wrapped in this type. It
-- will allow to signal errors as well as pass on normal results such as
-- incoming messages.
data ResumeProcess v
  = -- | The current operation of the process was interrupted with a
    -- 'InterruptReason'. The process may choose to continue or exit.
    Interrupted InterruptReason
  | -- | The process may resume to do work, using the given result.
    ResumeWith v
  deriving (Typeable, Generic, Generic1, Show)

instance NFData a => NFData (ResumeProcess a)

instance NFData1 ResumeProcess

-- | A function that decided if the next message will be received by
-- 'ReceiveSelectedMessage'. It conveniently is an instance of
-- 'Alternative' so the message selector can be combined:
-- >
-- > selectInt :: MessageSelector Int
-- > selectInt = selectMessage
-- >
-- > selectString :: MessageSelector String
-- > selectString = selectMessage
-- >
-- > selectIntOrString :: MessageSelector (Either Int String)
-- > selectIntOrString =
-- >   Left <$> selectTimeout<|> Right <$> selectString
newtype MessageSelector a = MessageSelector {runMessageSelector :: Message -> Maybe a}
  deriving (Semigroup, Monoid, Functor)

instance Applicative MessageSelector where
  pure = MessageSelector . pure . pure
  (MessageSelector f) <*> (MessageSelector x) =
    MessageSelector (\dyn -> f dyn <*> x dyn)

instance Alternative MessageSelector where
  empty = MessageSelector (const empty)
  (MessageSelector l) <|> (MessageSelector r) =
    MessageSelector (\dyn -> l dyn <|> r dyn)

-- | Create a message selector for a value that can be obtained by 'fromMessage'.
--
-- @since 0.9.1
selectMessage :: Typeable t => MessageSelector t
selectMessage = selectDynamicMessage fromMessage

-- | Create a message selector from a predicate.
--
-- @since 0.9.1
filterMessage :: Typeable a => (a -> Bool) -> MessageSelector a
filterMessage predicate =
  selectDynamicMessage
    ( \d -> case fromMessage d of
        Just a | predicate a -> Just a
        _ -> Nothing
    )

-- | Select a message of type @a@ and apply the given function to it.
-- If the function returns 'Just' The 'ReceiveSelectedMessage' function will
-- return the result (sans @Maybe@).
--
-- @since 0.9.1
selectMessageWith ::
  Typeable a => (a -> Maybe b) -> MessageSelector b
selectMessageWith f = selectDynamicMessage (fromMessage >=> f)

-- | Create a message selector.
--
-- @since 0.9.1
selectDynamicMessage :: (Message -> Maybe a) -> MessageSelector a
selectDynamicMessage = MessageSelector

-- | Create a message selector that will match every message. This is /lazy/
-- because the result is not 'force'ed.
--
-- @since 0.9.1
selectAnyMessage :: MessageSelector Message
selectAnyMessage = MessageSelector Just

-- | The state that a 'Process' is currently in.
data ProcessState
  = -- | The process has just been started but not
    --   scheduled yet.
    ProcessBooting
  | -- | The process yielded it's time slice
    ProcessIdle
  | -- | The process is busy with a non-blocking operation
    ProcessBusy
  | -- | The process is sleeping until the 'Timeout' given to 'Delay'
    ProcessBusySleeping
  | -- | The process is busy with 'UpdateProcessDetails'
    ProcessBusyUpdatingDetails
  | -- | The process is busy with sending a message
    ProcessBusySending
  | -- | The process is busy with killing
    ProcessBusySendingShutdown
  | -- | The process is busy with killing
    ProcessBusySendingInterrupt
  | -- | The process is blocked by 'receiveAnyMessage'
    ProcessBusyReceiving
  | -- | The process is blocked by 'linkProcess'
    ProcessBusyLinking
  | -- | The process is blocked by 'unlinkProcess'
    ProcessBusyUnlinking
  | -- | The process is blocked by 'monitor'
    ProcessBusyMonitoring
  | -- | The process is blocked by 'demonitor'
    ProcessBusyDemonitoring
  | -- | The process was interrupted
    ProcessInterrupted
  | -- | The process was shutdown or crashed
    ProcessShuttingDown
  deriving (Read, Show, Ord, Eq, Enum, Generic)

instance NFData ProcessState

instance Default ProcessState where
  def = ProcessBooting

instance ToLogMsg ProcessState where
  toLogMsg = \case
    ProcessBooting -> packLogMsg "Booting (the process has just been started but not scheduled yet)"
    ProcessIdle -> packLogMsg "Idle (the process yielded it's time slice)"
    ProcessBusy -> packLogMsg "Busy (the process is busy with a non-blocking operation)"
    ProcessBusySleeping -> packLogMsg "BusySleeping (the process is sleeping until the 'Timeout' given to 'Delay')"
    ProcessBusyUpdatingDetails -> packLogMsg "BusyUpdatingDetails (the process is busy with 'UpdateProcessDetails')"
    ProcessBusySending -> packLogMsg "BusySending (the process is busy with sending a message)"
    ProcessBusySendingShutdown -> packLogMsg "BusySendingShutdown (the process is busy with killing)"
    ProcessBusySendingInterrupt -> packLogMsg "BusySendingInterrupt (the process is busy with killing)"
    ProcessBusyReceiving -> packLogMsg "BusyReceiving (the process is waiting for a message)"
    ProcessBusyLinking -> packLogMsg "BusyLinking (the process is blocked by 'linkProcess')"
    ProcessBusyUnlinking -> packLogMsg "BusyUnlinking (the process is blocked by 'unlinkProcess')"
    ProcessBusyMonitoring -> packLogMsg "BusyMonitoring (the process is blocked by 'monitor')"
    ProcessBusyDemonitoring -> packLogMsg "BusyDemonitoring (the process is blocked by 'demonitor')"
    ProcessInterrupted -> packLogMsg "Interrupted (the process was interrupted)"
    ProcessShuttingDown -> packLogMsg "ShuttingDown (the process is shutting down)"

-- | A sum-type with reasons for why a process operation, such as receiving messages,
-- is interrupted in the scheduling loop.
--
-- This includes errors/exceptions that can be handled by the process.
--
-- @since 1.0.0
data InterruptReason where
  -- | A process has finished a unit of work and might exit or work on
  --   something else. This is primarily used for interrupting infinite
  --   server loops, allowing for additional cleanup work before
  --   exiting (e.g. with 'ExitNormally')
  --
  -- @since 0.13.2
  NormalExitRequested ::
    InterruptReason
  -- | A process that should be running was not running.
  OtherProcessNotRunning ::
    ProcessId ->
    InterruptReason
  -- | A 'Recoverable' timeout has occurred.
  TimeoutInterrupt ::
    LogMsg ->
    InterruptReason
  -- | A linked process is down, see 'Link' for a discussion on linking.
  LinkedProcessCrashed ::
    ProcessId ->
    InterruptReason
  -- | An exit reason that has an error message and is 'Recoverable'.
  ErrorInterrupt ::
    LogMsg ->
    InterruptReason
  -- | An interrupt with a custom message.
  --
  -- @since 0.30.0
  InterruptedBy ::
    Message ->
    InterruptReason
  deriving (Show, Typeable)

-- | A sum-type with reasons for why a process should shutdown.
--
-- This includes errors that force a process to exit.
--
-- @since 1.0.0
data ShutdownReason where
  -- | A process function returned or exited without any error.
  ExitNormally ::
    ShutdownReason
  -- | A process function returned or exited without any error, and with a custom message
  --
  -- @since 0.30.0
  ExitNormallyWith ::
    LogMsg ->
    ShutdownReason
  -- | An error causes the process to exit immediately.
  -- For example an unexpected runtime exception was thrown, i.e. an exception
  -- derived from 'Control.Exception.Safe.SomeException'
  -- Or a 'Recoverable' Interrupt was not recovered.
  ExitUnhandledError ::
    LogMsg ->
    ShutdownReason
  -- | A process shall exit immediately, without any cleanup was cancelled (e.g. killed, in 'Async.cancel')
  ExitProcessCancelled ::
    Maybe ProcessId ->
    ShutdownReason
  -- | A process that is vital to the crashed process was not running
  ExitOtherProcessNotRunning ::
    ProcessId ->
    ShutdownReason
  deriving (Eq, Ord, Show, Typeable)

-- | Return either 'ExitNormally' or 'interruptToExit' from a 'Recoverable' 'Interrupt';
--
-- If the 'Interrupt' is 'NormalExitRequested' then return 'ExitNormally'
interruptToExit :: InterruptReason -> ShutdownReason
interruptToExit NormalExitRequested = ExitNormally
interruptToExit x = ExitUnhandledError (toLogMsg x)

instance ToLogMsg InterruptReason where
  toLogMsg =
    \case
      NormalExitRequested -> packLogMsg "interrupt: A normal exit was requested"
      OtherProcessNotRunning p -> packLogMsg "interrupt: Another process is not running: " <> toLogMsg p
      TimeoutInterrupt reason -> packLogMsg "interrupt: A timeout occured: " <> reason
      LinkedProcessCrashed m -> packLogMsg "interrupt: A linked process " <> toLogMsg m <> packLogMsg " crashed"
      InterruptedBy reason -> packLogMsg "interrupt: " <> toLogMsg reason
      ErrorInterrupt reason -> packLogMsg "interrupt: An error occured: " <> reason

instance ToLogMsg ShutdownReason where
  toLogMsg =
    \case
      ExitNormally -> packLogMsg "exit: Process finished successfully"
      ExitNormallyWith reason -> packLogMsg "exit: Process finished successfully: " <> toLogMsg reason
      ExitUnhandledError w -> packLogMsg "exit: Unhandled " <> toLogMsg w
      ExitProcessCancelled Nothing -> packLogMsg "exit: The process was cancelled by a runtime exception"
      ExitProcessCancelled (Just origin) -> packLogMsg "exit: The process was cancelled by: " <> toLogMsg origin
      ExitOtherProcessNotRunning p -> packLogMsg "exit: Another process is not running: " <> toLogMsg p

instance NFData InterruptReason where
  rnf NormalExitRequested = rnf ()
  rnf (OtherProcessNotRunning !l) = rnf l
  rnf (TimeoutInterrupt !l) = rnf l
  rnf (LinkedProcessCrashed !l) = rnf l
  rnf (ErrorInterrupt !l) = rnf l
  rnf (InterruptedBy !l) = rnf l

instance NFData ShutdownReason where
  rnf ExitNormally = rnf ()
  rnf (ExitNormallyWith !l) = rnf l
  rnf (ExitUnhandledError !l) = rnf l
  rnf (ExitProcessCancelled !o) = rnf o
  rnf (ExitOtherProcessNotRunning !l) = rnf l

-- | Classify the 'ExitSeverity' of interrupt or shutdown reasons.
--
-- @since 1.0.0
class IsExitReason a where
  -- | Convert a value to the corresponding 'ExitSeverity'
  toExitSeverity :: a -> ExitSeverity

  -- | Return 'True' of the given reason @a@ indicates a 'Crash'
  isCrash :: a -> Bool
  isCrash x =
    toExitSeverity x == Crash

-- | This value indicates whether a process exited in way consistent with
-- the planned behaviour or not.
data ExitSeverity = ExitSuccess | Crash
  deriving (Typeable, Ord, Eq, Generic, Show)

instance ToLogMsg ExitSeverity where
  toLogMsg =
    \case
      ExitSuccess -> packLogMsg "exit-success"
      Crash -> packLogMsg "crash"

instance NFData ExitSeverity

instance IsExitReason InterruptReason where
  toExitSeverity = \case
    NormalExitRequested -> ExitSuccess
    _ -> Crash

instance IsExitReason ShutdownReason where
  toExitSeverity = \case
    ExitNormally -> ExitSuccess
    ExitNormallyWith _ -> ExitSuccess
    _ -> Crash

-- | Print an 'Interrupt' to 'Just' a formatted 'String' when 'isCrash'
-- is 'True'.
-- This can be useful in combination with view patterns, e.g.:
--
-- > logCrash :: InterruptReason -> Eff e ()
-- > logCrash (toCrashReason -> Just reason) = logError reason
-- > logCrash _ = return ()
--
-- Though this can be improved to:
--
-- > logCrash = traverse_ logError . toCrashReason
toCrashReason :: (ToLogMsg a, IsExitReason a) => a -> Maybe LogMsg
toCrashReason e
  | isCrash e = Just (toLogMsg e)
  | otherwise = Nothing

-- | Log the 'Interrupt's
logProcessExit ::
  forall e x. (Member Logs e, ToLogMsg x, IsExitReason x) => x -> Eff e ()
logProcessExit (toCrashReason -> Just ex) = withFrozenCallStack (logWarning ex)
logProcessExit ex = withFrozenCallStack (logDebug ex)

-- | An existential wrapper around 'Interrupt'
newtype InterruptOrShutdown = InterruptOrShutdown {fromInterruptOrShutdown :: Either ShutdownReason InterruptReason}
  deriving (Show, ToLogMsg, NFData)

-- | A predicate for linked process __crashes__.
isLinkedProcessCrashed :: Maybe ProcessId -> InterruptReason -> Bool
isLinkedProcessCrashed mOtherProcess =
  \case
    NormalExitRequested -> False
    OtherProcessNotRunning _ -> False
    TimeoutInterrupt _ -> False
    LinkedProcessCrashed p -> maybe True (== p) mOtherProcess
    InterruptedBy _ -> False
    ErrorInterrupt _ -> False

-- | A newtype wrapper for 'InterruptReason' to by used for 'Exc.Exception'.
--
-- @since 1.0.0
newtype UnhandledProcessInterrupt = MkUnhandledProcessInterrupt InterruptReason
  deriving (NFData, Typeable, Show)

instance Exc.Exception UnhandledProcessInterrupt

-- | A newtype wrapper for 'ShutdownReason' to by used for 'Exc.Exception'.
--
-- @since 1.0.0
newtype UnhandledProcessExit = MkUnhandledProcessExit ShutdownReason
  deriving (Show, Eq)

instance Exc.Exception UnhandledProcessExit

-- | This adds a layer of the 'Interrupts' effect on top of 'Processes'
type Processes e = Interrupts ': SafeProcesses e

-- | A constraint for an effect set that requires the presence of 'Processes'.
--
-- This constrains the effect list to look like this:
-- @[e1 ... eN, 'Interrupts', 'Process' [e(N+1) .. e(N+k)], e(N+1) .. e(N+k)]@
--
-- It constrains @e@ beyond 'HasSafeProcesses' to encompass 'Interrupts'.
--
-- @since 0.27.1
type HasProcesses e inner = (HasSafeProcesses e inner, Member Interrupts e)

-- | /Cons/ 'Process' onto a list of effects. This is called @SafeProcesses@ because
-- the actions cannot be interrupted through an exception mechanism,
-- and the 'ResumeProcess' value has to be inspected.
type SafeProcesses r = Process r ': r

-- | A constraint for an effect set that requires the presence of 'SafeProcesses'.
--
-- This constrains the effect list to look like this:
-- @[e1 ... eN, 'Process' [e(N+1) .. e(N+k)], e(N+1) .. e(N+k)]@
--
-- It constrains @e@ to support the (only) 'Process' effect.
--
-- This is more relaxed that 'HasProcesses' since it does not require 'Interrupts'.
--
-- @since 0.27.1
type HasSafeProcesses e inner = (SetMember Process (Process inner) e)

-- | 'Exc'eptions containing 'Interrupt's.
-- See 'handleInterrupts', 'exitOnInterrupt' or 'provideInterrupts'
type Interrupts = Exc InterruptReason

-- | Handle all 'Interrupt's of an 'Processes' by
-- wrapping them up in 'interruptToExit' and then do a process 'Shutdown'.
provideInterruptsShutdown ::
  forall e a. Eff (Processes e) a -> Eff (SafeProcesses e) a
provideInterruptsShutdown e = do
  res <- provideInterrupts e
  case res of
    Left ex -> send (Shutdown @e (interruptToExit ex))
    Right a -> return a

-- | Handle 'Interrupt's arising during process operations, e.g.
-- when a linked process crashes while we wait in a 'receiveSelectedMessage'
-- via a call to 'interrupt'.
handleInterrupts ::
  (Member Interrupts r) =>
  (InterruptReason -> Eff r a) ->
  Eff r a ->
  Eff r a
handleInterrupts = flip catchError

-- | Like 'handleInterrupts', but instead of passing the 'Interrupt'
-- to a handler function, 'Either' is returned.
--
-- @since 0.13.2
tryUninterrupted ::
  (Member Interrupts r) =>
  Eff r a ->
  Eff r (Either InterruptReason a)
tryUninterrupted = handleInterrupts (pure . Left) . fmap Right

-- | Handle interrupts by logging them with `logProcessExit` and otherwise
-- ignoring them.
logInterrupts ::
  forall r.
  (Member Logs r, Member Interrupts r) =>
  Eff r () ->
  Eff r ()
logInterrupts = handleInterrupts logProcessExit

-- | Handle 'Interrupt's arising during process operations, e.g.
-- when a linked process crashes while we wait in a 'receiveSelectedMessage'
-- via a call to 'interrupt'.
exitOnInterrupt ::
  HasProcesses r q =>
  Eff r a ->
  Eff r a
exitOnInterrupt = handleInterrupts (exitBecause . interruptToExit)

-- | Handle 'Interrupt's arising during process operations, e.g.
-- when a linked process crashes while we wait in a 'receiveSelectedMessage'
-- via a call to 'interrupt'.
provideInterrupts :: Eff (Interrupts ': r) a -> Eff r (Either InterruptReason a)
provideInterrupts = runError

-- | Wrap all (left) 'Interrupt's into 'interruptToExit' and
-- return the (right) 'NoRecovery' 'Interrupt's as is.
mergeEitherInterruptAndExitReason ::
  Either InterruptReason ShutdownReason -> ShutdownReason
mergeEitherInterruptAndExitReason = either interruptToExit id

-- | Throw an 'Interrupt', can be handled by 'handleInterrupts' or
--   'exitOnInterrupt' or 'provideInterrupts'.
interrupt :: Member Interrupts r => InterruptReason -> Eff r a
interrupt = throwError

-- | Execute a and action and return the result;
-- if the process is interrupted by an error or exception, or an explicit
-- shutdown from another process, or through a crash of a linked process, i.e.
-- whenever the exit reason satisfies 'isRecoverable', return the exit reason.
executeAndResume ::
  forall q r v.
  HasSafeProcesses r q =>
  Process q (ResumeProcess v) ->
  Eff r (Either InterruptReason v)
executeAndResume processAction = do
  result <- send processAction
  case result of
    ResumeWith !value -> return (Right value)
    Interrupted r -> return (Left r)

-- | Execute a 'Process' action and resume the process, exit
-- the process when an 'Interrupts' was raised. Use 'executeAndResume' to catch
-- interrupts.
executeAndResumeOrExit ::
  forall r q v.
  HasSafeProcesses r q =>
  Process q (ResumeProcess v) ->
  Eff r v
executeAndResumeOrExit processAction = do
  result <- send processAction
  case result of
    ResumeWith !value -> return value
    Interrupted r -> send (Shutdown @q (interruptToExit r))

-- | Execute a 'Process' action and resume the process, exit
-- the process when an 'Interrupts' was raised. Use 'executeAndResume' to catch
-- interrupts.
executeAndResumeOrThrow ::
  forall q r v.
  HasProcesses r q =>
  Process q (ResumeProcess v) ->
  Eff r v
executeAndResumeOrThrow processAction = do
  result <- send processAction
  case result of
    ResumeWith !value -> return value
    Interrupted r -> interrupt r

-- * Process Effects

-- | Use 'executeAndResumeOrExit' to execute 'YieldProcess'. Refer to 'YieldProcess'
-- for more information.
yieldProcess :: forall r q. HasProcesses r q => Eff r ()
yieldProcess = executeAndResumeOrThrow YieldProcess

-- | Simply block until the time in the 'Timeout' has passed.
--
-- @since 0.30.0
delay :: forall r q. HasProcesses r q => Timeout -> Eff r ()
delay = executeAndResumeOrThrow . Delay

-- | Send a message to a process addressed by the 'ProcessId'.
-- See 'SendMessage'.
--
-- The message will be reduced to normal form ('rnf') by/in the caller process.
sendMessage ::
  forall o r q.
  ( HasProcesses r q,
    Typeable o,
    NFData o
  ) =>
  ProcessId ->
  o ->
  Eff r ()
sendMessage pid message =
  rnf pid `seq` toMessage message
    `seq` executeAndResumeOrThrow (SendMessage pid (toMessage message))

-- | Send a 'Dynamic' value to a process addressed by the 'ProcessId'.
-- See 'SendMessage'.
sendAnyMessage ::
  forall r q.
  HasProcesses r q =>
  ProcessId ->
  Message ->
  Eff r ()
sendAnyMessage pid message =
  executeAndResumeOrThrow (SendMessage pid message)

-- | Exit a process addressed by the 'ProcessId'. The process will exit,
-- it might do some cleanup, but is ultimately unrecoverable.
-- See 'SendShutdown'.
sendShutdown ::
  forall r q.
  HasProcesses r q =>
  ProcessId ->
  ShutdownReason ->
  Eff r ()
sendShutdown pid s =
  pid `deepseq` s `deepseq` executeAndResumeOrThrow (SendShutdown pid s)

-- | Interrupts a process addressed by the 'ProcessId'. The process might exit,
-- or it may continue.
-- | Like 'sendInterrupt', but also return @True@ iff the process to exit exists.
sendInterrupt ::
  forall r q.
  HasProcesses r q =>
  ProcessId ->
  InterruptReason ->
  Eff r ()
sendInterrupt pid s =
  pid `deepseq` s `deepseq` executeAndResumeOrThrow (SendInterrupt pid s)

-- | Start a new process, the new process will execute an effect, the function
-- will return immediately with a 'ProcessId'. If the new process is
-- interrupted, the process will 'Shutdown' with the 'Interrupt'
-- wrapped in 'interruptToExit'. For specific use cases it might be better to use
-- 'spawnRaw'.
spawn ::
  forall r q.
  HasProcesses r q =>
  ProcessTitle ->
  Eff (Processes q) () ->
  Eff r ProcessId
spawn t child =
  executeAndResumeOrThrow (Spawn @q t (provideInterruptsShutdown child))

-- | Like 'spawn' but return @()@.
spawn_ ::
  forall r q.
  HasProcesses r q =>
  ProcessTitle ->
  Eff (Processes q) () ->
  Eff r ()
spawn_ t child = void (spawn t child)

-- | Start a new process, and immediately link to it.
--
-- See 'Link' for a discussion on linking.
--
-- @since 0.12.0
spawnLink ::
  forall r q.
  HasProcesses r q =>
  ProcessTitle ->
  Eff (Processes q) () ->
  Eff r ProcessId
spawnLink t child =
  executeAndResumeOrThrow (SpawnLink @q t (provideInterruptsShutdown child))

-- | Start a new process, the new process will execute an effect, the function
-- will return immediately with a 'ProcessId'. The spawned process has only the
-- /raw/ 'SafeProcesses' effects. For non-library code 'spawn' might be better
-- suited.
spawnRaw ::
  forall r q.
  HasProcesses r q =>
  ProcessTitle ->
  Eff (SafeProcesses q) () ->
  Eff r ProcessId
spawnRaw t child = executeAndResumeOrThrow (Spawn @q t child)

-- | Like 'spawnRaw' but return @()@.
spawnRaw_ ::
  forall r q.
  HasProcesses r q =>
  ProcessTitle ->
  Eff (SafeProcesses q) () ->
  Eff r ()
spawnRaw_ t = void . spawnRaw t

-- | Return 'True' if the process is alive.
--
-- @since 0.12.0
isProcessAlive ::
  forall r q.
  HasProcesses r q =>
  ProcessId ->
  Eff r Bool
isProcessAlive pid = isJust <$> executeAndResumeOrThrow (GetProcessState pid)

-- | Return the 'ProcessTitle', 'ProcessDetails'  and 'ProcessState',
-- for the given process, if the process is alive.
--
-- @since 0.24.1
getProcessState ::
  forall r q.
  HasProcesses r q =>
  ProcessId ->
  Eff r (Maybe (ProcessTitle, ProcessDetails, ProcessState))
getProcessState pid = executeAndResumeOrThrow (GetProcessState pid)

-- | Replace the 'ProcessDetails' of the process.
--
-- @since 0.24.1
updateProcessDetails ::
  forall r q.
  HasProcesses r q =>
  ProcessDetails ->
  Eff r ()
updateProcessDetails pd = executeAndResumeOrThrow (UpdateProcessDetails pd)

-- | Block until a message was received.
-- See 'ReceiveSelectedMessage' for more documentation.
receiveAnyMessage ::
  forall r q.
  HasProcesses r q =>
  Eff r Message
receiveAnyMessage =
  executeAndResumeOrThrow (ReceiveSelectedMessage selectAnyMessage)

-- | Block until a message was received, that is not 'Nothing' after applying
-- a callback to it.
-- See 'ReceiveSelectedMessage' for more documentation.
receiveSelectedMessage ::
  forall r q a.
  HasProcesses r q =>
  MessageSelector a ->
  Eff r a
receiveSelectedMessage f = executeAndResumeOrThrow (ReceiveSelectedMessage f)

-- | Receive and cast the message to some 'Typeable' instance.
-- See 'ReceiveSelectedMessage' for more documentation.
-- This will wait for a message of the return type using 'receiveSelectedMessage'
receiveMessage ::
  forall a r q.
  ( Typeable a,
    HasProcesses r q
  ) =>
  Eff r a
receiveMessage = receiveSelectedMessage (MessageSelector fromMessage)

-- | Remove and return all messages currently enqueued in the process message
-- queue.
--
-- @since 0.12.0
flushMessages ::
  forall r q.
  HasProcesses r q =>
  Eff r [Message]
flushMessages = executeAndResumeOrThrow @q FlushMessages

-- | Enter a loop to receive messages and pass them to a callback, until the
-- function returns 'Just' a result.
-- Only the messages of the given type will be received.
-- If the process is interrupted by an exception or by a 'SendInterrupt' from
-- another process, then the callback will be invoked with @'Left' 'Interrupt'@,
-- otherwise the process will be exited
--
-- See also 'ReceiveSelectedMessage' for more documentation.
receiveSelectedLoop ::
  forall r q a endOfLoopResult.
  HasSafeProcesses r q =>
  MessageSelector a ->
  (Either InterruptReason a -> Eff r (Maybe endOfLoopResult)) ->
  Eff r endOfLoopResult
receiveSelectedLoop selector handlers = do
  mReq <- send (ReceiveSelectedMessage @q @a selector)
  mRes <- case mReq of
    Interrupted reason -> handlers (Left reason)
    ResumeWith message -> handlers (Right message)
  maybe (receiveSelectedLoop selector handlers) return mRes

-- | Like 'receiveSelectedLoop' but /not selective/.
-- See also 'selectAnyMessage', 'receiveSelectedLoop'.
receiveAnyLoop ::
  forall r q endOfLoopResult.
  HasSafeProcesses r q =>
  (Either InterruptReason Message -> Eff r (Maybe endOfLoopResult)) ->
  Eff r endOfLoopResult
receiveAnyLoop = receiveSelectedLoop selectAnyMessage

-- | Like 'receiveSelectedLoop' but refined to casting to a specific 'Typeable'
-- using 'selectMessage'.
receiveLoop ::
  forall r q a endOfLoopResult.
  (HasSafeProcesses r q, Typeable a) =>
  (Either InterruptReason a -> Eff r (Maybe endOfLoopResult)) ->
  Eff r endOfLoopResult
receiveLoop = receiveSelectedLoop selectMessage

-- | Returns the 'ProcessId' of the current process.
self :: HasSafeProcesses r q => Eff r ProcessId
self = executeAndResumeOrExit SelfPid

-- | Generate a unique 'Int' for the current process.
makeReference ::
  HasProcesses r q =>
  Eff r Int
makeReference = executeAndResumeOrThrow MakeReference

-- | A value that contains a unique reference of a process
-- monitoring.
--
-- @since 0.12.0
data MonitorReference = MkMonitorReference
  { _monitorIndex :: !Int,
    _monitoredProcess :: !ProcessId
  }
  deriving (Read, Eq, Ord, Generic, Typeable, Show)

instance ToLogMsg MonitorReference where
  toLogMsg (MkMonitorReference ref pid) =
    toLogMsg pid <> packLogMsg "_" <> packLogMsg "monitor_" <> toLogMsg (show ref)

instance NFData MonitorReference

-- | Monitor another process. When the monitored process exits a
--  'ProcessDown' is sent to the calling process.
-- The return value is a unique identifier for that monitor.
-- There can be multiple monitors on the same process,
-- and a message for each will be sent.
-- If the process is already dead, the 'ProcessDown' message
-- will be sent immediately, without exit reason
--
-- @since 0.12.0
monitor ::
  forall r q.
  HasProcesses r q =>
  ProcessId ->
  Eff r MonitorReference
monitor = executeAndResumeOrThrow . Monitor . force

-- | Remove a monitor created with 'monitor'.
--
-- @since 0.12.0
demonitor ::
  forall r q.
  HasProcesses r q =>
  MonitorReference ->
  Eff r ()
demonitor = executeAndResumeOrThrow . Demonitor . force

-- | 'monitor' another process before while performing an action
-- and 'demonitor' afterwards.
--
-- @since 0.12.0
withMonitor ::
  HasProcesses r q =>
  ProcessId ->
  (MonitorReference -> Eff r a) ->
  Eff r a
withMonitor pid e = monitor pid >>= \ref -> e ref <* demonitor ref

-- | A 'MessageSelector' for receiving either a monitor of the
-- given process or another message.
--
-- @since 0.12.0
receiveWithMonitor ::
  HasProcesses r q =>
  ProcessId ->
  MessageSelector a ->
  Eff r (Either ProcessDown a)
receiveWithMonitor pid sel =
  withMonitor
    pid
    ( \ref ->
        receiveSelectedMessage (Left <$> selectProcessDown ref <|> Right <$> sel)
    )

-- | A monitored process exited.
-- This message is sent to a process by the scheduler, when
-- a process that was monitored died.
--
-- @since 0.12.0
data ProcessDown = ProcessDown
  { downReference :: !MonitorReference,
    downReason :: !ShutdownReason,
    downProcess :: !ProcessId
  }
  deriving (Typeable, Generic, Eq, Ord, Show)

instance ToTypeLogMsg ProcessDown where
  toTypeLogMsg _ = "ProcessDown"

instance ToLogMsg ProcessDown where
  toLogMsg (ProcessDown ref reason pid) =
    toLogMsg ref <> packLogMsg " " <> toLogMsg reason <> packLogMsg " " <> toLogMsg pid

-- | Make an 'Interrupt' for a 'ProcessDown' message.
--
-- For example: @doSomething >>= either (interrupt . becauseOtherProcessNotRunning) return@
--
-- @since 1.0.0
becauseOtherProcessNotRunning :: ProcessDown -> InterruptReason
becauseOtherProcessNotRunning = OtherProcessNotRunning . _monitoredProcess . downReference

instance NFData ProcessDown

-- | A 'MessageSelector' for the 'ProcessDown' message of a specific
-- process.
--
-- The parameter is the value obtained by 'monitor'.
--
-- @since 0.12.0
selectProcessDown :: MonitorReference -> MessageSelector ProcessDown
selectProcessDown ref0 =
  filterMessage (\(ProcessDown ref _reason _pid) -> ref0 == ref)

-- | A 'MessageSelector' for the 'ProcessDown' message. of a specific
-- process.
--
-- In contrast to 'selectProcessDown' this function matches the 'ProcessId'.
--
-- @since 0.28.0
selectProcessDownByProcessId :: ProcessId -> MessageSelector ProcessDown
selectProcessDownByProcessId pid0 =
  filterMessage (\(ProcessDown _ref _reason pid) -> pid0 == pid)

-- | Connect the calling process to another process, such that
-- if one of the processes crashes (i.e. 'isCrash' returns 'True'), the other
-- is shutdown with the 'Interrupt' 'LinkedProcessCrashed'.
--
-- See 'Link' for a discussion on linking.
--
-- @since 0.12.0
linkProcess ::
  forall r q.
  HasProcesses r q =>
  ProcessId ->
  Eff r ()
linkProcess = executeAndResumeOrThrow . Link . force

-- | Unlink the calling process from the other process.
--
-- See 'Link' for a discussion on linking.
--
-- @since 0.12.0
unlinkProcess ::
  forall r q.
  HasProcesses r q =>
  ProcessId ->
  Eff r ()
unlinkProcess = executeAndResumeOrThrow . Unlink . force

-- | Exit the process with a 'Interrupt'.
exitBecause ::
  forall r q a.
  HasSafeProcesses r q =>
  ShutdownReason ->
  Eff r a
exitBecause = send . Shutdown @q . force

-- | Exit the process.
exitNormally ::
  forall r q a. HasSafeProcesses r q => Eff r a
exitNormally = exitBecause ExitNormally

-- | Exit the process with an error.
exitWithError ::
  forall r q a.
  HasSafeProcesses r q =>
  String ->
  Eff r a
exitWithError = exitBecause . interruptToExit . ErrorInterrupt . fromString

-- | Each process is identified by a single process id, that stays constant
-- throughout the life cycle of a process. Also, message sending relies on these
-- values to address messages to processes.
newtype ProcessId = ProcessId {_fromProcessId :: Int}
  deriving (Eq, Ord, Typeable, Bounded, Num, Enum, Integral, Real, NFData)

instance ToLogMsg ProcessId where
  toLogMsg (ProcessId !p) = packLogMsg ('!' : show p)

instance Read ProcessId where
  readsPrec _ ('!' : rest1) = case reads rest1 of
    [(c, rest2)] -> [(ProcessId c, rest2)]
    _ -> []
  readsPrec _ _ = []

instance Show ProcessId where
  showsPrec _ (ProcessId !c) = showChar '!' . shows c

makeLenses ''ProcessId

makeLenses ''MonitorReference
