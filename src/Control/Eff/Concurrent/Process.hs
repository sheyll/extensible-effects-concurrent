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
    Process(..)
    -- ** Process Effect Aliases
  , SafeProcesses
  , Processes
    -- ** Process Effect Constraints
  , HasProcesses
  , HasSafeProcesses
      -- ** Process Info
  , ProcessTitle(..)
  , fromProcessTitle
  , toProcessTitle

  , ProcessDetails(..)
  , fromProcessDetails

      -- ** Process Timer
  , Timeout(TimeoutMicros, fromTimeoutMicros)

  , -- ** Message Data
    StrictDynamic()
  , toStrictDynamic
  , fromStrictDynamic
  , unwrapStrictDynamic
  , Serializer(..)

    -- ** ProcessId Type
  , ProcessId(..)
  , fromProcessId
  -- ** Process State
  , ProcessState(..)
  -- ** Yielding
  , yieldProcess
  -- ** Waiting/Sleeping
  , delay
  -- ** Sending Messages
  , sendMessage
  , sendAnyMessage
  -- ** Utilities
  , makeReference
  -- ** Receiving Messages
  , receiveMessage
  , receiveSelectedMessage
  , flushMessages
  , receiveAnyMessage
  , receiveLoop
  , receiveSelectedLoop
  , receiveAnyLoop
  -- ** Selecting Messages to Receive
  , MessageSelector(runMessageSelector)
  , selectMessage
  , filterMessage
  , selectMessageWith
  , selectDynamicMessage
  , selectAnyMessage
  -- ** Process State Reflection
  , self
  , isProcessAlive
  , getProcessState
  , updateProcessDetails
    -- ** Spawning
  , spawn
  , spawn_
  , spawnLink
  , spawnRaw
  , spawnRaw_
  -- ** Process Operation Execution
  , ResumeProcess(..)
  , executeAndResume
  , executeAndResumeOrExit
  , executeAndResumeOrThrow
  -- ** Exits and Interrupts
  -- *** Interrupting Processes
  , interrupt
  , sendInterrupt
  -- *** Exiting Processes
  , exitBecause
  , exitNormally
  , exitWithError
  , sendShutdown -- TODO rename to 'sendExit'
  -- *** Linking Processes
  , linkProcess
  , unlinkProcess
  -- *** Monitor Processes
  , monitor
  , demonitor
  , ProcessDown(..)
  , selectProcessDown
  , selectProcessDownByProcessId
  , becauseProcessIsDown
  , MonitorReference(..)
  , monitorIndex
  , monitoredProcess
  , withMonitor
  , receiveWithMonitor
  -- *** Exit and Interrupt Reasons
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
  -- ** Control Flow
  -- *** Process Interrupt Recoverable Handling
  , provideInterruptsShutdown
  , handleInterrupts
  , tryUninterrupted
  , exitOnInterrupt
  , logInterrupts
  , provideInterrupts
  , mergeEitherInterruptAndExitReason
  -- ** Typed ProcessIds: Receiver
  , sendToReceiver
  , Receiver(..)
  , receiverPid
  )
where

import           Control.Applicative
import           Control.Eff.Concurrent.Misc
import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Extend
import           Control.Eff.Log.Handler
import           Control.Eff.Log.Message
import qualified Control.Exception             as Exc
import           Control.Lens
import           Control.Monad                  ( void
                                                , (>=>)
                                                )
import           Data.Default
import           Data.Dynamic
import           Data.Functor.Contravariant     ()
import           Data.Kind
import           Data.Function
import           Data.Maybe
import           Data.String
import           Data.Text                      ( Text, pack, unpack)
import qualified Data.Text                     as T
import           Type.Reflection                ( SomeTypeRep(..), typeRep )
import           GHC.Stack
import           GHC.Generics                    ( Generic
                                                 , Generic1
                                                 )


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
  FlushMessages :: Process r (ResumeProcess [StrictDynamic])
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
  Shutdown :: Interrupt 'NoRecovery -> Process r a
  -- | Shutdown another process immediately, the other process has no way of handling this!
  SendShutdown ::ProcessId  -> Interrupt 'NoRecovery  -> Process r (ResumeProcess ())
  -- | Request that another a process interrupts. The targeted process is interrupted
  -- and gets an 'Interrupted', the target process may decide to ignore the
  -- interrupt and continue as if nothing happened.
  SendInterrupt :: ProcessId -> Interrupt 'Recoverable -> Process r (ResumeProcess ())
  -- | Send a message to a process addressed by the 'ProcessId'. Sending a
  -- message should __always succeed__ and return __immediately__, even if the
  -- destination process does not exist, or does not accept messages of the
  -- given type.
  SendMessage :: ProcessId -> StrictDynamic -> Process r (ResumeProcess ())
  -- | Receive a message that matches a criteria.
  -- This should block until an a message was received. The message is returned
  -- as a 'ResumeProcess' value. The function should also return if an exception
  -- was caught or a shutdown was requested.
  ReceiveSelectedMessage :: forall r a . MessageSelector a -> Process r (ResumeProcess a)
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


instance Show (Process r b) where
  showsPrec d = \case
    FlushMessages -> showString "flush messages"
    YieldProcess  -> showString "yield process"
    Delay t       -> showString "delay process until: " . shows t
    SelfPid       -> showString "lookup the current process id"
    Spawn   t _   -> showString "spawn a new process: " . shows t
    SpawnLink t _ -> showString "spawn a new process and link to it" . shows t
    Shutdown sr ->
      showParen (d >= 10) (showString "shutdown " . showsPrec 10 sr)
    SendShutdown toPid sr -> showParen
      (d >= 10)
      ( showString "shutting down "
      . showsPrec 10 toPid
      . showChar ' '
      . showsPrec 10 sr
      )
    SendInterrupt toPid sr -> showParen
      (d >= 10)
      ( showString "interrupting "
      . showsPrec 10 toPid
      . showChar ' '
      . showsPrec 10 sr
      )
    SendMessage toPid sr -> showParen
      (d >= 10)
      ( showString "sending to "
      . showsPrec 10 toPid
      . showChar ' '
      . showsPrec 10 sr
      )
    ReceiveSelectedMessage _   -> showString "receive a message"
    MakeReference              -> showString "generate a unique reference"
    Monitor   pid              -> showString "monitor " . shows pid
    Demonitor i                -> showString "demonitor " . shows i
    Link      l                -> showString "link " . shows l
    Unlink    l                -> showString "unlink " . shows l
    GetProcessState pid        -> showString "get the process state of " . shows pid
    UpdateProcessDetails l     -> showString "update the process details to: " . shows l

-- | A short title for a 'Process' for logging purposes.
--
-- @since 0.24.1
newtype ProcessTitle =
  MkProcessTitle { _fromProcessTitle :: LogMsg }
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
newtype ProcessDetails =
  MkProcessDetails { _fromProcessDetails :: Text }
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
  deriving (NFData, Ord,Eq, Num, Integral, Real, Enum, Typeable)

instance ToLogMsg Timeout where
  toLogMsg (TimeoutMicros u) = packLogMsg (show u ++ "µs")

instance Show Timeout where
  showsPrec d (TimeoutMicros t) =
    showParen (d >= 10) (showString "timeout: " . shows t . showString " µs")

-- | Data flows between 'Process'es via these messages.
--
-- This is just a newtype wrapper around 'Dynamic'.
-- The reason this type exists is to force construction through the code in this
-- module, which always evaluates a message to /normal form/ __before__
-- sending it to another process.
--
-- @since 0.22.0
newtype StrictDynamic where
  MkDynamicMessage :: Dynamic -> StrictDynamic
  deriving Typeable

instance NFData StrictDynamic where
  rnf (MkDynamicMessage d) = d `seq` ()

instance Show StrictDynamic where
  show (MkDynamicMessage d) = show d

instance ToLogMsg StrictDynamic where
  toLogMsg = packLogMsg . show

-- | Serialize a @message@ into a 'StrictDynamic' value to be sent via 'sendAnyMessage'.
--
-- This indirection allows, among other things, the composition of
-- 'Control.Eff.Concurrent.Protocol.Effectful.Server's.
--
-- @since 0.24.1
newtype Serializer message =
  MkSerializer
    { runSerializer :: message -> StrictDynamic
    } deriving (Typeable)

instance NFData (Serializer message) where
  rnf (MkSerializer !s) = s `seq` ()

instance Typeable message => Show (Serializer message) where
  showsPrec d _x = showParen (d >= 10) (showSTypeable @message . showString "-serializer")

instance Contravariant Serializer where
  contramap f (MkSerializer b) = MkSerializer (b . f)

-- | Deeply evaluate the given value and wrap it into a 'StrictDynamic'.
--
-- @since 0.22.0
toStrictDynamic :: (Typeable a, NFData a) => a -> StrictDynamic
toStrictDynamic x = force x `seq` toDyn (force x) `seq` MkDynamicMessage (toDyn (force x))

-- | Convert a 'StrictDynamic' back to a value.
--
-- @since 0.22.0
fromStrictDynamic :: Typeable a => StrictDynamic -> Maybe a
fromStrictDynamic (MkDynamicMessage d) = fromDynamic d


-- | Convert a 'StrictDynamic' back to an unwrapped 'Dynamic'.
--
-- @since 0.22.0
unwrapStrictDynamic :: StrictDynamic -> Dynamic
unwrapStrictDynamic (MkDynamicMessage d) = d

-- | Every 'Process' action returns it's actual result wrapped in this type. It
-- will allow to signal errors as well as pass on normal results such as
-- incoming messages.
data ResumeProcess v where
  -- | The current operation of the process was interrupted with a
  -- 'Interrupt'. If 'isRecoverable' holds for the given reason,
  -- the process may choose to continue.
  Interrupted :: Interrupt 'Recoverable -> ResumeProcess v
  -- | The process may resume to do work, using the given result.
  ResumeWith ::a -> ResumeProcess a
  deriving ( Typeable, Generic, Generic1, Show )

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
--
newtype MessageSelector a =
  MessageSelector {runMessageSelector :: StrictDynamic -> Maybe a }
  deriving (Semigroup, Monoid, Functor)

instance Applicative MessageSelector where
  pure = MessageSelector . pure . pure
  (MessageSelector f) <*> (MessageSelector x) =
    MessageSelector (\dyn -> f dyn <*> x dyn)

instance Alternative MessageSelector where
  empty = MessageSelector (const empty)
  (MessageSelector l) <|> (MessageSelector r) =
    MessageSelector (\dyn -> l dyn <|> r dyn)

-- | Create a message selector for a value that can be obtained by 'fromStrictDynamic'.
--
-- @since 0.9.1
selectMessage :: Typeable t => MessageSelector t
selectMessage = selectDynamicMessage fromStrictDynamic

-- | Create a message selector from a predicate.
--
-- @since 0.9.1
filterMessage :: Typeable a => (a -> Bool) -> MessageSelector a
filterMessage predicate = selectDynamicMessage
  (\d -> case fromStrictDynamic d of
    Just a | predicate a -> Just a
    _                    -> Nothing
  )

-- | Select a message of type @a@ and apply the given function to it.
-- If the function returns 'Just' The 'ReceiveSelectedMessage' function will
-- return the result (sans @Maybe@).
--
-- @since 0.9.1
selectMessageWith
  :: Typeable a => (a -> Maybe b) -> MessageSelector b
selectMessageWith f = selectDynamicMessage (fromStrictDynamic >=> f)

-- | Create a message selector.
--
-- @since 0.9.1
selectDynamicMessage :: (StrictDynamic -> Maybe a) -> MessageSelector a
selectDynamicMessage = MessageSelector

-- | Create a message selector that will match every message. This is /lazy/
-- because the result is not 'force'ed.
--
-- @since 0.9.1
selectAnyMessage :: MessageSelector StrictDynamic
selectAnyMessage = MessageSelector Just

-- | The state that a 'Process' is currently in.
data ProcessState =
    ProcessBooting              -- ^ The process has just been started but not
                                --   scheduled yet.
  | ProcessIdle                 -- ^ The process yielded it's time slice
  | ProcessBusy                 -- ^ The process is busy with a non-blocking operation
  | ProcessBusySleeping         -- ^ The process is sleeping until the 'Timeout' given to 'Delay'
  | ProcessBusyUpdatingDetails  -- ^ The process is busy with 'UpdateProcessDetails'
  | ProcessBusySending          -- ^ The process is busy with sending a message
  | ProcessBusySendingShutdown  -- ^ The process is busy with killing
  | ProcessBusySendingInterrupt -- ^ The process is busy with killing
  | ProcessBusyReceiving        -- ^ The process blocked by a 'receiveAnyMessage'
  | ProcessBusyLinking          -- ^ The process blocked by a 'linkProcess'
  | ProcessBusyUnlinking        -- ^ The process blocked by a 'unlinkProcess'
  | ProcessBusyMonitoring       -- ^ The process blocked by a 'monitor'
  | ProcessBusyDemonitoring     -- ^ The process blocked by a 'demonitor'
  | ProcessInterrupted          -- ^ The process was interrupted
  | ProcessShuttingDown         -- ^ The process was shutdown or crashed
  deriving (Read, Show, Ord, Eq, Enum, Generic)

instance NFData ProcessState

instance Default ProcessState where
  def = ProcessBooting

instance ToLogMsg ProcessState where
  toLogMsg = \case
    ProcessBooting              -> packLogMsg "Booting (the process has just been started but not scheduled yet)"
    ProcessIdle                 -> packLogMsg "Idle (the process yielded it's time slice)"
    ProcessBusy                 -> packLogMsg "Busy (the process is busy with a non-blocking operation)"
    ProcessBusySleeping         -> packLogMsg "BusySleeping (the process is sleeping until the 'Timeout' given to 'Delay')"
    ProcessBusyUpdatingDetails  -> packLogMsg "BusyUpdatingDetails (the process is busy with 'UpdateProcessDetails')"
    ProcessBusySending          -> packLogMsg "BusySending (the process is busy with sending a message)"
    ProcessBusySendingShutdown  -> packLogMsg "BusySendingShutdown (the process is busy with killing)"
    ProcessBusySendingInterrupt -> packLogMsg "BusySendingInterrupt (the process is busy with killing)"
    ProcessBusyReceiving        -> packLogMsg "BusyReceiving (the process blocked by a 'receiveAnyMessage')"
    ProcessBusyLinking          -> packLogMsg "BusyLinking (the process blocked by a 'linkProcess')"
    ProcessBusyUnlinking        -> packLogMsg "BusyUnlinking (the process blocked by a 'unlinkProcess')"
    ProcessBusyMonitoring       -> packLogMsg "BusyMonitoring (the process blocked by a 'monitor')"
    ProcessBusyDemonitoring     -> packLogMsg "BusyDemonitoring (the process blocked by a 'demonitor')"
    ProcessInterrupted          -> packLogMsg "Interrupted (the process was interrupted)"
    ProcessShuttingDown         -> packLogMsg "ShuttingDown (the process was shutdown or crashed)"

-- | This kind is used to indicate if a 'Interrupt' can be treated like
-- a short interrupt which can be handled or ignored.
data ExitRecovery = Recoverable | NoRecovery
  deriving (Typeable, Ord, Eq, Generic)

instance NFData ExitRecovery

instance Show ExitRecovery where
  showsPrec d =
    showParen (d >= 10)
      . (\case
          Recoverable -> showString "recoverable"
          NoRecovery  -> showString "not recoverable"
        )

-- | Get the 'ExitRecovery'
toExitRecovery :: Interrupt r -> ExitRecovery
toExitRecovery =
  \case
    NormalExitRequested -> Recoverable
    (NormalExitRequestedWith _) -> Recoverable
    (OtherProcessNotRunning _) -> Recoverable
    (TimeoutInterrupt _) -> Recoverable
    (LinkedProcessCrashed _) -> Recoverable
    (InterruptedBy _) -> Recoverable
    (ErrorInterrupt _) -> Recoverable
    ExitNormally -> NoRecovery
    (ExitNormallyWith _) -> NoRecovery
    (ExitUnhandledError _) -> NoRecovery
    (ExitProcessCancelled _) -> NoRecovery
    (ExitOtherProcessNotRunning _) -> NoRecovery

-- | This value indicates whether a process exited in way consistent with
-- the planned behaviour or not.
data ExitSeverity = NormalExit | Crash
  deriving (Typeable, Ord, Eq, Generic)

instance Show ExitSeverity where
  showsPrec d =
    showParen (d >= 10)
      . (\case
          NormalExit -> showString "exit success"
          Crash      -> showString "crash"
        )

instance NFData ExitSeverity

-- | Get the 'ExitSeverity' of a 'Interrupt'.
toExitSeverity :: Interrupt e -> ExitSeverity
toExitSeverity = \case
  ExitNormally                     -> NormalExit
  NormalExitRequested              -> NormalExit
  _                                -> Crash

-- | A sum-type with reasons for why a process operation, such as receiving messages,
-- is interrupted in the scheduling loop.
--
-- This includes errors, that can occur when scheduling messages.
--
-- @since 0.23.0
data Interrupt (t :: ExitRecovery) where
    -- | A process has finished a unit of work and might exit or work on
    --   something else. This is primarily used for interrupting infinite
    --   server loops, allowing for additional cleanup work before
    --   exiting (e.g. with 'ExitNormally')
    --
    -- @since 0.13.2
    NormalExitRequested
      :: Interrupt 'Recoverable
    -- | Extension of 'ExitNormally' with a custom reason
    --
    -- @since 0.30.0
    NormalExitRequestedWith
      :: forall a . (Show a, NFData a) => a -> Interrupt 'Recoverable
    -- | A process that should be running was not running.
    OtherProcessNotRunning
      :: ProcessId -> Interrupt 'Recoverable
    -- | A 'Recoverable' timeout has occurred.
    TimeoutInterrupt
      :: String -> Interrupt 'Recoverable
    -- | A linked process is down, see 'Link' for a discussion on linking.
    LinkedProcessCrashed
      :: ProcessId -> Interrupt 'Recoverable
    -- | An exit reason that has an error message and is 'Recoverable'.
    ErrorInterrupt
      :: String -> Interrupt 'Recoverable
    -- | An interrupt with a custom message.
    --
    -- @since 0.30.0
    InterruptedBy
      :: forall a . (Show a, NFData a) => a -> Interrupt 'Recoverable
    -- | A process function returned or exited without any error.
    ExitNormally
      :: Interrupt 'NoRecovery
    -- | A process function returned or exited without any error, and with a custom message
    --
    -- @since 0.30.0
    ExitNormallyWith
      :: forall a . (Show a, NFData a) => a -> Interrupt 'NoRecovery
    -- | An error causes the process to exit immediately.
    -- For example an unexpected runtime exception was thrown, i.e. an exception
    -- derived from 'Control.Exception.Safe.SomeException'
    -- Or a 'Recoverable' Interrupt was not recovered.
    ExitUnhandledError
      :: Text -> Interrupt 'NoRecovery
    -- | A process shall exit immediately, without any cleanup was cancelled (e.g. killed, in 'Async.cancel')
    ExitProcessCancelled
      :: Maybe ProcessId -> Interrupt 'NoRecovery
    -- | A process that is vital to the crashed process was not running
    ExitOtherProcessNotRunning
      :: ProcessId -> Interrupt 'NoRecovery
  deriving Typeable

-- | Return either 'ExitNormally' or 'interruptToExit' from a 'Recoverable' 'Interrupt';
--
-- If the 'Interrupt' is 'NormalExitRequested' then return 'ExitNormally'
interruptToExit :: Interrupt 'Recoverable -> Interrupt 'NoRecovery
interruptToExit NormalExitRequested = ExitNormally
interruptToExit (NormalExitRequestedWith x) = (ExitNormallyWith x)
interruptToExit x = ExitUnhandledError (pack (show x))

instance Show (Interrupt x) where
  showsPrec d =
    showParen (d >= 10) .
    (\case
       NormalExitRequested -> showString "interrupt: A normal exit was requested"
       NormalExitRequestedWith p -> showString "interrupt: A normal exit was requested: " . showsPrec 10 p
       OtherProcessNotRunning p -> showString "interrupt: Another process is not running: " . showsPrec 10 p
       TimeoutInterrupt reason -> showString "interrupt: A timeout occured: " . showString reason
       LinkedProcessCrashed m -> showString "interrupt: A linked process " . showsPrec 10 m . showString " crashed"
       InterruptedBy reason -> showString "interrupt: " . showsPrec 10 reason
       ErrorInterrupt reason -> showString "interrupt: An error occured: " . showString reason
       ExitNormally -> showString "exit: Process finished successfully"
       ExitNormallyWith reason -> showString "exit: Process finished successfully: " . showsPrec 10 reason
       ExitUnhandledError w -> showString "exit: Unhandled " . showString (unpack w)
       ExitProcessCancelled Nothing -> showString "exit: The process was cancelled by a runtime exception"
       ExitProcessCancelled (Just origin) -> showString "exit: The process was cancelled by: " . shows origin
       ExitOtherProcessNotRunning p -> showString "exit: Another process is not running: " . showsPrec 10 p)

instance Exc.Exception (Interrupt 'Recoverable)
instance Exc.Exception (Interrupt 'NoRecovery )

instance ToLogMsg (Interrupt x) where
  toLogMsg =
    \case
       NormalExitRequested -> packLogMsg "interrupt: A normal exit was requested"
       NormalExitRequestedWith p -> packLogMsg "interrupt: A normal exit was requested: " <> packLogMsg (show p)
       OtherProcessNotRunning p -> packLogMsg "interrupt: Another process is not running: " <> toLogMsg p
       TimeoutInterrupt reason -> packLogMsg "interrupt: A timeout occured: " <> toLogMsg reason
       LinkedProcessCrashed m -> packLogMsg "interrupt: A linked process " <> toLogMsg m <> packLogMsg " crashed"
       InterruptedBy reason -> packLogMsg "interrupt: " <> packLogMsg (show reason)
       ErrorInterrupt reason -> packLogMsg "interrupt: An error occured: " <> toLogMsg reason
       ExitNormally -> packLogMsg "exit: Process finished successfully"
       ExitNormallyWith reason -> packLogMsg "exit: Process finished successfully: " <> packLogMsg (show reason)
       ExitUnhandledError w -> packLogMsg "exit: Unhandled " <> MkLogMsg w
       ExitProcessCancelled Nothing -> packLogMsg "exit: The process was cancelled by a runtime exception"
       ExitProcessCancelled (Just origin) -> packLogMsg "exit: The process was cancelled by: " <> toLogMsg origin
       ExitOtherProcessNotRunning p -> packLogMsg "exit: Another process is not running: " <> toLogMsg p


instance NFData (Interrupt x) where
  rnf NormalExitRequested             = rnf ()
  rnf (NormalExitRequestedWith   !l)  = rnf l
  rnf (OtherProcessNotRunning    !l)  = rnf l
  rnf (TimeoutInterrupt       !l)     = rnf l
  rnf (LinkedProcessCrashed !l)       = rnf l
  rnf (ErrorInterrupt         !l)     = rnf l
  rnf (InterruptedBy         !l)    = rnf l
  rnf ExitNormally                    = rnf ()
  rnf (ExitNormallyWith !l)           = rnf l
  rnf (ExitUnhandledError !l)         = rnf l
  rnf (ExitProcessCancelled  !o)      = rnf o
  rnf (ExitOtherProcessNotRunning !l) = rnf l

instance Ord (Interrupt x) where
  compare NormalExitRequested          NormalExitRequested               = EQ
  compare NormalExitRequested          _                                 = LT
  compare _                        NormalExitRequested                   = GT
  compare (NormalExitRequestedWith _)    (NormalExitRequestedWith _)     = EQ
  compare (NormalExitRequestedWith _)    _                               = LT
  compare _                        (NormalExitRequestedWith    _)        = GT
  compare (OtherProcessNotRunning l)    (OtherProcessNotRunning r)       = compare l r
  compare (OtherProcessNotRunning _)    _                                = LT
  compare _                        (OtherProcessNotRunning    _)         = GT
  compare (TimeoutInterrupt l)       (TimeoutInterrupt r)                = compare l r
  compare (TimeoutInterrupt _) _                                         = LT
  compare _                        (TimeoutInterrupt _)                  = GT
  compare (LinkedProcessCrashed l) (LinkedProcessCrashed r)              = compare l r
  compare (LinkedProcessCrashed _) _                                     = LT
  compare _                        (LinkedProcessCrashed _)              = GT
  compare (InterruptedBy _) (InterruptedBy _)                        = EQ
  compare (InterruptedBy _) _                                          = LT
  compare _                        (InterruptedBy _)                   = GT
  compare (ErrorInterrupt l)         (ErrorInterrupt         r)          = compare l r
  compare ExitNormally             ExitNormally                          = EQ
  compare ExitNormally             _                                     = LT
  compare _                        ExitNormally                          = GT
  compare (ExitNormallyWith _) (ExitNormallyWith _)                      = EQ
  compare (ExitNormallyWith _ ) _                                        = LT
  compare _                         (ExitNormallyWith _)                 = GT
  compare (ExitUnhandledError l) (ExitUnhandledError r)                  = compare l r
  compare (ExitUnhandledError _ ) _                                      = LT
  compare _                         (ExitUnhandledError _)               = GT
  compare (ExitOtherProcessNotRunning l)  (ExitOtherProcessNotRunning r) = compare l r
  compare (ExitOtherProcessNotRunning _ ) _                              = LT
  compare _                         (ExitOtherProcessNotRunning _)       = GT
  compare (ExitProcessCancelled l)  (ExitProcessCancelled r)             = compare l r

instance Eq (Interrupt x) where
  (==) NormalExitRequested NormalExitRequested = True
  (==) (OtherProcessNotRunning l) (OtherProcessNotRunning r) = (==) l r
  (==) ExitNormally ExitNormally = True
  (==) (TimeoutInterrupt l) (TimeoutInterrupt r) = l == r
  (==) (LinkedProcessCrashed l) (LinkedProcessCrashed r) = l == r
  (==) (ErrorInterrupt l) (ErrorInterrupt r) = (==) l r
  (==) (ExitUnhandledError l) (ExitUnhandledError r) = (==) l r
  (==) (ExitOtherProcessNotRunning l) (ExitOtherProcessNotRunning r) = (==) l r
  (==) (ExitProcessCancelled l) (ExitProcessCancelled r) = l == r
  (==) _ _ = False

-- | A predicate for linked process __crashes__.
isProcessDownInterrupt :: Maybe ProcessId -> Interrupt r -> Bool
isProcessDownInterrupt mOtherProcess =
  \case
    NormalExitRequested -> False
    NormalExitRequestedWith _ -> False
    OtherProcessNotRunning _ -> False
    TimeoutInterrupt _ -> False
    LinkedProcessCrashed p -> maybe True (== p) mOtherProcess
    InterruptedBy _ -> False
    ErrorInterrupt _ -> False
    ExitNormally -> False
    ExitNormallyWith _ -> False
    ExitUnhandledError _ -> False
    ExitProcessCancelled _ -> False
    ExitOtherProcessNotRunning _ -> False

-- | 'Interrupt's which are 'Recoverable'.
type RecoverableInterrupt = Interrupt 'Recoverable

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
-- the the actions cannot be interrupted in.
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
type Interrupts = Exc (Interrupt 'Recoverable)

-- | Handle all 'Interrupt's of an 'Processes' by
-- wrapping them up in 'interruptToExit' and then do a process 'Shutdown'.
provideInterruptsShutdown
  :: forall e a . Eff (Processes e) a -> Eff (SafeProcesses e) a
provideInterruptsShutdown e = do
  res <- provideInterrupts e
  case res of
    Left  ex -> send (Shutdown @e (interruptToExit ex))
    Right a  -> return a

-- | Handle 'Interrupt's arising during process operations, e.g.
-- when a linked process crashes while we wait in a 'receiveSelectedMessage'
-- via a call to 'interrupt'.
handleInterrupts
  :: (HasCallStack, Member Interrupts r)
  => (Interrupt 'Recoverable -> Eff r a)
  -> Eff r a
  -> Eff r a
handleInterrupts = flip catchError

-- | Like 'handleInterrupts', but instead of passing the 'Interrupt'
-- to a handler function, 'Either' is returned.
--
-- @since 0.13.2
tryUninterrupted
  :: (HasCallStack, Member Interrupts r)
  => Eff r a
  -> Eff r (Either (Interrupt 'Recoverable) a)
tryUninterrupted = handleInterrupts (pure . Left) . fmap Right

-- | Handle interrupts by logging them with `logProcessExit` and otherwise
-- ignoring them.
logInterrupts
  :: forall r
   . (Member Logs r, HasCallStack, Member Interrupts r)
  => Eff r ()
  -> Eff r ()
logInterrupts = handleInterrupts logProcessExit

-- | Handle 'Interrupt's arising during process operations, e.g.
-- when a linked process crashes while we wait in a 'receiveSelectedMessage'
-- via a call to 'interrupt'.
exitOnInterrupt
  :: (HasCallStack, HasProcesses r q)
  => Eff r a
  -> Eff r a
exitOnInterrupt = handleInterrupts (exitBecause . interruptToExit)

-- | Handle 'Interrupt's arising during process operations, e.g.
-- when a linked process crashes while we wait in a 'receiveSelectedMessage'
-- via a call to 'interrupt'.
provideInterrupts
  :: HasCallStack => Eff (Interrupts ': r) a -> Eff r (Either (Interrupt 'Recoverable) a)
provideInterrupts = runError


-- | Wrap all (left) 'Interrupt's into 'interruptToExit' and
-- return the (right) 'NoRecovery' 'Interrupt's as is.
mergeEitherInterruptAndExitReason
  :: Either (Interrupt 'Recoverable) (Interrupt 'NoRecovery) -> Interrupt 'NoRecovery
mergeEitherInterruptAndExitReason = either interruptToExit id

-- | Throw an 'Interrupt', can be handled by 'handleInterrupts' or
--   'exitOnInterrupt' or 'provideInterrupts'.
interrupt :: (HasCallStack, Member Interrupts r) => Interrupt 'Recoverable -> Eff r a
interrupt = throwError

-- | A predicate for crashes. A /crash/ happens when a process exits
-- with an 'Interrupt' other than 'ExitNormally'
isCrash :: Interrupt x -> Bool
isCrash NormalExitRequested = False
isCrash ExitNormally      = False
isCrash _                 = True

-- | A predicate for recoverable exit reasons. This predicate defines the
-- exit reasons which functions such as 'executeAndResume'
isRecoverable :: Interrupt x -> Bool
isRecoverable (toExitRecovery -> Recoverable) = True
isRecoverable _                               = False

-- | An existential wrapper around 'Interrupt'
data SomeExitReason where
  SomeExitReason ::Interrupt x -> SomeExitReason

instance Ord SomeExitReason where
  compare = compare `on` fromSomeExitReason

instance Eq SomeExitReason where
  (==) = (==) `on` fromSomeExitReason

instance Show SomeExitReason where
  show = show . fromSomeExitReason

instance NFData SomeExitReason where
  rnf = rnf . fromSomeExitReason

-- | Partition a 'SomeExitReason' back into either a 'NoRecovery'
-- or a 'Recoverable' 'Interrupt'
fromSomeExitReason :: SomeExitReason -> Either (Interrupt 'NoRecovery) (Interrupt 'Recoverable)
fromSomeExitReason (SomeExitReason e) =
  case e of
    recoverable@NormalExitRequested -> Right recoverable
    recoverable@(NormalExitRequestedWith _) -> Right recoverable
    recoverable@(OtherProcessNotRunning _) -> Right recoverable
    recoverable@(TimeoutInterrupt _) -> Right recoverable
    recoverable@(LinkedProcessCrashed _) -> Right recoverable
    recoverable@(InterruptedBy _) -> Right recoverable
    recoverable@(ErrorInterrupt _) -> Right recoverable
    noRecovery@ExitNormally -> Left noRecovery
    noRecovery@(ExitNormallyWith _) -> Left noRecovery
    noRecovery@(ExitUnhandledError _) -> Left noRecovery
    noRecovery@(ExitProcessCancelled _) -> Left noRecovery
    noRecovery@(ExitOtherProcessNotRunning _) -> Left noRecovery

-- | Print a 'Interrupt' to 'Just' a formatted 'String' when 'isCrash'
-- is 'True'.
-- This can be useful in combination with view patterns, e.g.:
--
-- > logCrash :: Interrupt -> Eff e ()
-- > logCrash (toCrashReason -> Just reason) = logError reason
-- > logCrash _ = return ()
--
-- Though this can be improved to:
--
-- > logCrash = traverse_ logError . toCrashReason
--
toCrashReason :: Interrupt x -> Maybe T.Text
toCrashReason e | isCrash e = Just (T.pack (show e))
                | otherwise = Nothing

-- | Log the 'Interrupt's
logProcessExit
  :: forall e x . (Member Logs e, HasCallStack) => Interrupt x -> Eff e ()
logProcessExit (toCrashReason -> Just ex) = withFrozenCallStack (logWarning (MkLogMsg ex))
logProcessExit ex = withFrozenCallStack (logDebug ex)


-- | Execute a and action and return the result;
-- if the process is interrupted by an error or exception, or an explicit
-- shutdown from another process, or through a crash of a linked process, i.e.
-- whenever the exit reason satisfies 'isRecoverable', return the exit reason.
executeAndResume
  :: forall q r v
   . (HasSafeProcesses r q, HasCallStack)
  => Process q (ResumeProcess v)
  -> Eff r (Either (Interrupt 'Recoverable) v)
executeAndResume processAction = do
  result <- send processAction
  case result of
    ResumeWith  !value -> return (Right value)
    Interrupted r      -> return (Left r)

-- | Execute a 'Process' action and resume the process, exit
-- the process when an 'Interrupts' was raised. Use 'executeAndResume' to catch
-- interrupts.
executeAndResumeOrExit
  :: forall r q v
   . (HasSafeProcesses r q, HasCallStack)
  => Process q (ResumeProcess v)
  -> Eff r v
executeAndResumeOrExit processAction = do
  result <- send processAction
  case result of
    ResumeWith  !value -> return value
    Interrupted r      -> send (Shutdown @q (interruptToExit r))

-- | Execute a 'Process' action and resume the process, exit
-- the process when an 'Interrupts' was raised. Use 'executeAndResume' to catch
-- interrupts.
executeAndResumeOrThrow
  :: forall q r v
   . (HasProcesses r q, HasCallStack)
  => Process q (ResumeProcess v)
  -> Eff r v
executeAndResumeOrThrow processAction = do
  result <- send processAction
  case result of
    ResumeWith  !value -> return value
    Interrupted r      -> interrupt r

-- * Process Effects

-- | Use 'executeAndResumeOrExit' to execute 'YieldProcess'. Refer to 'YieldProcess'
-- for more information.
yieldProcess
  :: forall r q
   . (HasProcesses r q, HasCallStack)
  => Eff r ()
yieldProcess = executeAndResumeOrThrow YieldProcess


-- | Simply block until the time in the 'Timeout' has passed.
--
-- @since 0.30.0
delay
  :: forall r q
   . ( HasProcesses r q
     , HasCallStack
     )
  => Timeout
  -> Eff r ()
delay = executeAndResumeOrThrow . Delay

-- | Send a message to a process addressed by the 'ProcessId'.
-- See 'SendMessage'.
--
-- The message will be reduced to normal form ('rnf') by/in the caller process.
sendMessage
  :: forall o r q
   . ( HasProcesses r q
     , HasCallStack
     , Typeable o
     , NFData o
     )
  => ProcessId
  -> o
  -> Eff r ()
sendMessage pid message =
  rnf pid `seq` toStrictDynamic message
          `seq` executeAndResumeOrThrow (SendMessage pid (toStrictDynamic message))

-- | Send a 'Dynamic' value to a process addressed by the 'ProcessId'.
-- See 'SendMessage'.
sendAnyMessage
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessId
  -> StrictDynamic
  -> Eff r ()
sendAnyMessage pid message =
  executeAndResumeOrThrow (SendMessage pid message)

-- | Exit a process addressed by the 'ProcessId'. The process will exit,
-- it might do some cleanup, but is ultimately unrecoverable.
-- See 'SendShutdown'.
sendShutdown
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessId
  -> Interrupt 'NoRecovery
  -> Eff r ()
sendShutdown pid s =
  pid `deepseq` s `deepseq` executeAndResumeOrThrow (SendShutdown pid s)

-- | Interrupts a process addressed by the 'ProcessId'. The process might exit,
-- or it may continue.
-- | Like 'sendInterrupt', but also return @True@ iff the process to exit exists.
sendInterrupt
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessId
  -> Interrupt 'Recoverable
  -> Eff r ()
sendInterrupt pid s =
  pid `deepseq` s `deepseq` executeAndResumeOrThrow (SendInterrupt pid s)

-- | Start a new process, the new process will execute an effect, the function
-- will return immediately with a 'ProcessId'. If the new process is
-- interrupted, the process will 'Shutdown' with the 'Interrupt'
-- wrapped in 'interruptToExit'. For specific use cases it might be better to use
-- 'spawnRaw'.
spawn
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessTitle
  -> Eff (Processes q) ()
  -> Eff r ProcessId
spawn t child =
  executeAndResumeOrThrow (Spawn @q t (provideInterruptsShutdown child))

-- | Like 'spawn' but return @()@.
spawn_
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessTitle
  -> Eff (Processes q) ()
  -> Eff r ()
spawn_ t child = void (spawn t child)

-- | Start a new process, and immediately link to it.
--
-- See 'Link' for a discussion on linking.
--
-- @since 0.12.0
spawnLink
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessTitle
  -> Eff (Processes q) ()
  -> Eff r ProcessId
spawnLink t child =
  executeAndResumeOrThrow (SpawnLink @q t (provideInterruptsShutdown child))

-- | Start a new process, the new process will execute an effect, the function
-- will return immediately with a 'ProcessId'. The spawned process has only the
-- /raw/ 'SafeProcesses' effects. For non-library code 'spawn' might be better
-- suited.
spawnRaw
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessTitle
  -> Eff (SafeProcesses q) ()
  -> Eff r ProcessId
spawnRaw t child = executeAndResumeOrThrow (Spawn @q t child)

-- | Like 'spawnRaw' but return @()@.
spawnRaw_
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessTitle
  -> Eff (SafeProcesses q) ()
  -> Eff r ()
spawnRaw_ t = void . spawnRaw t

-- | Return 'True' if the process is alive.
--
-- @since 0.12.0
isProcessAlive
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessId
  -> Eff r Bool
isProcessAlive pid = isJust <$> executeAndResumeOrThrow (GetProcessState pid)


-- | Return the 'ProcessTitle', 'ProcessDetails'  and 'ProcessState',
-- for the given process, if the process is alive.
--
-- @since 0.24.1
getProcessState
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessId
  -> Eff r (Maybe (ProcessTitle, ProcessDetails, ProcessState))
getProcessState pid = executeAndResumeOrThrow (GetProcessState pid)

-- | Replace the 'ProcessDetails' of the process.
--
-- @since 0.24.1
updateProcessDetails
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessDetails
  -> Eff r ()
updateProcessDetails pd = executeAndResumeOrThrow (UpdateProcessDetails pd)

-- | Block until a message was received.
-- See 'ReceiveSelectedMessage' for more documentation.
receiveAnyMessage
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => Eff r StrictDynamic
receiveAnyMessage =
  executeAndResumeOrThrow (ReceiveSelectedMessage selectAnyMessage)

-- | Block until a message was received, that is not 'Nothing' after applying
-- a callback to it.
-- See 'ReceiveSelectedMessage' for more documentation.
receiveSelectedMessage
  :: forall r q a
   . ( HasCallStack
     , Show a
     , HasProcesses r q
     )
  => MessageSelector a
  -> Eff r a
receiveSelectedMessage f = executeAndResumeOrThrow (ReceiveSelectedMessage f)

-- | Receive and cast the message to some 'Typeable' instance.
-- See 'ReceiveSelectedMessage' for more documentation.
-- This will wait for a message of the return type using 'receiveSelectedMessage'
receiveMessage
  :: forall a r q
   . ( HasCallStack
     , Typeable a
     , NFData a
     , Show a
     , HasProcesses r q
     )
  => Eff r a
receiveMessage = receiveSelectedMessage (MessageSelector fromStrictDynamic)

-- | Remove and return all messages currently enqueued in the process message
-- queue.
--
-- @since 0.12.0
flushMessages
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => Eff r [StrictDynamic]
flushMessages = executeAndResumeOrThrow @q FlushMessages

-- | Enter a loop to receive messages and pass them to a callback, until the
-- function returns 'Just' a result.
-- Only the messages of the given type will be received.
-- If the process is interrupted by an exception of by a 'SendShutdown' from
-- another process, with an exit reason that satisfies 'isRecoverable', then
-- the callback will be invoked with @'Left' 'Interrupt'@, otherwise the
-- process will be exited with the same reason using 'exitBecause'.
-- See also 'ReceiveSelectedMessage' for more documentation.
receiveSelectedLoop
  :: forall r q a endOfLoopResult
   . (HasSafeProcesses r q, HasCallStack)
  => MessageSelector a
  -> (Either (Interrupt 'Recoverable) a -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveSelectedLoop selector handlers = do
  mReq <- send (ReceiveSelectedMessage @q @a selector)
  mRes <- case mReq of
    Interrupted reason  -> handlers (Left reason)
    ResumeWith  message -> handlers (Right message)
  maybe (receiveSelectedLoop selector handlers) return mRes

-- | Like 'receiveSelectedLoop' but /not selective/.
-- See also 'selectAnyMessage', 'receiveSelectedLoop'.
receiveAnyLoop
  :: forall r q endOfLoopResult
   . (HasSafeProcesses r q, HasCallStack)
  => (Either (Interrupt 'Recoverable) StrictDynamic -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveAnyLoop = receiveSelectedLoop selectAnyMessage

-- | Like 'receiveSelectedLoop' but refined to casting to a specific 'Typeable'
-- using 'selectMessage'.
receiveLoop
  :: forall r q a endOfLoopResult
   . (HasSafeProcesses r q, HasCallStack, NFData a, Typeable a)
  => (Either (Interrupt 'Recoverable) a -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveLoop = receiveSelectedLoop selectMessage

-- | Returns the 'ProcessId' of the current process.
self :: (HasCallStack, HasSafeProcesses r q) => Eff r ProcessId
self = executeAndResumeOrExit SelfPid

-- | Generate a unique 'Int' for the current process.
makeReference
  :: (HasCallStack, HasProcesses r q)
  => Eff r Int
makeReference = executeAndResumeOrThrow MakeReference

-- | A value that contains a unique reference of a process
-- monitoring.
--
-- @since 0.12.0
data MonitorReference =
  MkMonitorReference { _monitorIndex     :: !Int
                     , _monitoredProcess :: !ProcessId
                     }
  deriving (Read, Eq, Ord, Generic, Typeable)

instance ToLogMsg MonitorReference where
  toLogMsg (MkMonitorReference ref pid) =
   toLogMsg pid <> packLogMsg "_" <>  packLogMsg "monitor_" <> toLogMsg (show ref)

instance NFData MonitorReference

instance Show MonitorReference where
  showsPrec d m = showParen
    (d >= 10)
    (showString "monitor: " . shows (_monitorIndex m) . showChar ' ' . shows
      (_monitoredProcess m)
    )

-- | Monitor another process. When the monitored process exits a
--  'ProcessDown' is sent to the calling process.
-- The return value is a unique identifier for that monitor.
-- There can be multiple monitors on the same process,
-- and a message for each will be sent.
-- If the process is already dead, the 'ProcessDown' message
-- will be sent immediately, without exit reason
--
-- @since 0.12.0
monitor
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessId
  -> Eff r MonitorReference
monitor = executeAndResumeOrThrow . Monitor . force

-- | Remove a monitor created with 'monitor'.
--
-- @since 0.12.0
demonitor
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => MonitorReference
  -> Eff r ()
demonitor = executeAndResumeOrThrow . Demonitor . force

-- | 'monitor' another process before while performing an action
-- and 'demonitor' afterwards.
--
-- @since 0.12.0
withMonitor
  :: (HasCallStack, HasProcesses r q)
  => ProcessId
  -> (MonitorReference -> Eff r a)
  -> Eff r a
withMonitor pid e = monitor pid >>= \ref -> e ref <* demonitor ref

-- | A 'MessageSelector' for receiving either a monitor of the
-- given process or another message.
--
-- @since 0.12.0
receiveWithMonitor
  :: ( HasCallStack
     , HasProcesses r q
     , Typeable a
     , Show a
     )
  => ProcessId
  -> MessageSelector a
  -> Eff r (Either ProcessDown a)
receiveWithMonitor pid sel = withMonitor
  pid
  (\ref ->
    receiveSelectedMessage (Left <$> selectProcessDown ref <|> Right <$> sel)
  )

-- | A monitored process exited.
-- This message is sent to a process by the scheduler, when
-- a process that was monitored died.
--
-- @since 0.12.0
data ProcessDown =
  ProcessDown
    { downReference :: !MonitorReference
    , downReason    :: !(Interrupt 'NoRecovery)
    , downProcess   :: !ProcessId
    }
  deriving (Typeable, Generic, Eq, Ord)

instance ToLogMsg ProcessDown where
  toLogMsg (ProcessDown ref reason pid) =
    toLogMsg ref <> packLogMsg " " <> toLogMsg reason <> packLogMsg " " <> toLogMsg pid

-- | Make an 'Interrupt' for a 'ProcessDown' message.
--
-- For example: @doSomething >>= either (interrupt . becauseProcessIsDown) return@
--
-- @since 0.12.0
becauseProcessIsDown :: ProcessDown -> Interrupt 'Recoverable
becauseProcessIsDown = OtherProcessNotRunning . _monitoredProcess . downReference

instance NFData ProcessDown

instance Show ProcessDown where
  showsPrec d =
    showParen (d >= 10)
      . (\case
          ProcessDown ref reason pid ->
            showString "down: "
              . shows pid
              . showChar ' '
              . shows ref
              . showChar ' '
              . showsPrec 11 reason
        )

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
linkProcess
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessId
  -> Eff r ()
linkProcess = executeAndResumeOrThrow . Link . force

-- | Unlink the calling process from the other process.
--
-- See 'Link' for a discussion on linking.
--
-- @since 0.12.0
unlinkProcess
  :: forall r q
   . (HasCallStack, HasProcesses r q)
  => ProcessId
  -> Eff r ()
unlinkProcess = executeAndResumeOrThrow . Unlink . force

-- | Exit the process with a 'Interrupt'.
exitBecause
  :: forall r q a
   . (HasCallStack, HasSafeProcesses r q)
  => Interrupt 'NoRecovery
  -> Eff r a
exitBecause = send . Shutdown @q . force

-- | Exit the process.
exitNormally
  :: forall r q a . (HasCallStack, HasSafeProcesses r q) => Eff r a
exitNormally = exitBecause ExitNormally

-- | Exit the process with an error.
exitWithError
  :: forall r q a
   . (HasCallStack,HasSafeProcesses r q)
  => String
  -> Eff r a
exitWithError = exitBecause . interruptToExit . ErrorInterrupt

-- | Each process is identified by a single process id, that stays constant
-- throughout the life cycle of a process. Also, message sending relies on these
-- values to address messages to processes.
newtype ProcessId = ProcessId { _fromProcessId :: Int }
  deriving (Eq,Ord,Typeable,Bounded,Num, Enum, Integral, Real, NFData)

instance ToLogMsg ProcessId where
  toLogMsg (ProcessId !p) = packLogMsg ('!' : show p)

instance Read ProcessId where
  readsPrec _ ('!' : rest1) = case reads rest1 of
    [(c, rest2)] -> [(ProcessId c, rest2)]
    _            -> []
  readsPrec _ _ = []

instance Show ProcessId where
  showsPrec _ (ProcessId !c) = showChar '!' . shows c

makeLenses ''ProcessId
makeLenses ''MonitorReference

-- | Serialize and send a message to the process in a 'Receiver'.
--
-- EXPERIMENTAL
--
-- @since 0.29.0
sendToReceiver :: (NFData o, HasProcesses r q) => Receiver o -> o -> Eff r ()
sendToReceiver (Receiver pid serializer) message =
  rnf message `seq` sendMessage pid (toStrictDynamic (serializer message))

-- | A 'ProcessId' and a 'Serializer'. EXPERIMENTAL
--
-- See 'sendToReceiver'.
--
-- @since 0.29.0
data Receiver a =
  forall out . (NFData out, Typeable out, Show out) =>
    Receiver { _receiverPid :: ProcessId
             , _receiverSerializer :: a -> out
             }
  deriving (Typeable)

instance NFData (Receiver o) where
  rnf (Receiver e f) = f `seq` rnf e

instance Eq (Receiver o) where
  (==) = (==) `on` _receiverPid

instance Ord (Receiver o) where
  compare = compare `on` _receiverPid

instance Contravariant Receiver where
  contramap f (Receiver p s) = Receiver p (s . f)

instance Typeable protocol => Show (Receiver protocol) where
  showsPrec d (Receiver c _) =
    showParen (d>=10)
    (showSTypeRep (SomeTypeRep (Type.Reflection.typeRep @protocol)) . showsPrec 10 c)

makeLenses ''Receiver
