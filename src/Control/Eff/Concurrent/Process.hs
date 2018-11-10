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
  ( -- * ProcessId Type
    ProcessId(..)
  , fromProcessId
   -- * Process Effects
  , Process(..)
  , ConsProcess
  , ResumeProcess(..)
  , SchedulerProxy(..)
  , MessageSelector(runMessageSelector)
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
  , ProcessState(..)
  , ExitRecovery(..)
  , toExitRecovery
  , isRecoverable
  , ExitSeverity(..)
  , toExitSeverity
  , ProcessExitReason(..)
  , isCrash
  , toCrashReason
  , SomeProcessExitReason(SomeProcessExitReason)
  , fromSomeProcessExitReason
  , logProcessExit
  , thisSchedulerProxy
  , executeAndCatch
  , executeAndResume
  , yieldProcess
  , sendMessage
  , sendMessageAs
  , sendMessageChecked
  , spawn
  , spawn_
  , linkProcess
  , unlinkProcess
  , monitor
  , demonitor
  , receiveAnyMessage
  , receiveMessageAs
  , receiveSelectedMessage
  , receiveAnyLoop
  , receiveLoop
  , receiveSelectedLoop
  , self
  , sendShutdown
  , sendShutdownChecked
  , sendInterrupt
  , sendInterruptChecked
  , exitBecause
  , exitNormally
  , exitWithError
  , makeReference
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.Log.Handler
import           Control.Eff.Log.Message
import           Control.Lens
import           Control.Monad                  ( void
                                                , (>=>)
                                                )
import           Data.Default
import           Data.Dynamic
import           Data.Kind
import           GHC.Stack
import           Data.Function
import qualified Control.Exception             as Exc

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
-- * the spawn
--
-- * when the first process exists, all process should be killed immediately
data Process (r :: [Type -> Type]) b where
  -- | In cooperative schedulers, this will give processing time to the
  -- scheduler. Every other operation implicitly serves the same purpose.
  YieldProcess :: Process r (ResumeProcess ())
  -- | Return the current 'ProcessId'
  SelfPid :: Process r (ResumeProcess ProcessId)
  -- | Start a new process, the new process will execute an effect, the function
  -- will return immediately with a 'ProcessId'.
  Spawn :: Eff (Process r ': r) () -> Process r (ResumeProcess ProcessId)
  -- | Shutdown the process; irregardles of the exit reason, this function never
  -- returns,
  Shutdown :: ProcessExitReason 'NoRecovery   -> Process r a
  -- | Raise an error, that can be handled.
  SendShutdown :: ProcessId  -> ProcessExitReason 'NoRecovery  -> Process r (ResumeProcess Bool)
  -- | Request that another a process interrupts. The targeted process is interrupted
  -- and gets an 'Interrupted', the target process may decide to ignore the
  -- interrupt and continue as if nothing happened.
  SendInterrupt :: ProcessId -> ProcessExitReason 'Recoverable -> Process r (ResumeProcess Bool)
  -- | Send a message to a process addressed by the 'ProcessId'. Sending a
  -- message should **always succeed** and return **immediately**, even if the
  -- destination process does not exist, or does not accept messages of the
  -- given type.
  SendMessage :: ProcessId -> Dynamic -> Process r (ResumeProcess Bool)
  -- | Receive a message that matches a criterium.
  -- This should block until an a message was received. The message is returned
  -- as a 'ProcessMessage' value. The function should also return if an exception
  -- was caught or a shutdown was requested.
  ReceiveSelectedMessage :: forall r a . MessageSelector a -> Process r (ResumeProcess a)
  -- | Generate a unique 'Int' for the current process.
  MakeReference :: Process r (ResumeProcess Int)
  -- | Monitor another process. When the monitored process exits a
  --  'MonitoredProcessDown' is sent to the calling process.
  -- The return value is a unique identifier for that monitor.
  -- There can be multiple monitors on the same process,
  -- and a message for each will be sent.
  -- If the process is already dead, the 'MonitoredProcessDown' message
  -- will be sent immediately, w.thout exit reason
  Monitor :: ProcessId -> Process r (ResumeProcess Int)
  -- | Remove a monitor.
  Demonitor :: Int -> Process r (ResumeProcess ())
  -- | Connect the calling process to another process, such that
  -- if one of the processes crashes (i.e. 'isCrash' returns 'True'), the other
  -- is shutdown with the 'ProcessExitReaon' 'LinkedProcessCrashed'.
  Link :: ProcessId -> Process r (ResumeProcess ())
  -- | Unlink the calling proccess from the other process.
  Unlink :: ProcessId -> Process r (ResumeProcess ())

instance Show (Process r b) where
  showsPrec d = \case
    YieldProcess -> showString "yield process"
    SelfPid      -> showString "lookup the current process id"
    Spawn    _   -> showString "spawn a new process"
    Shutdown sr  -> showParen (d >= 10) (showString "shutdown " . showsPrec 10 sr)
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
    ReceiveSelectedMessage _ -> showString "receive a message"
    MakeReference            -> showString "generate a unique reference"
    Monitor   pid            -> showString "monitor " . shows pid
    Demonitor i              -> showString "demonitor " . shows i
    Link      l              -> showString "link " . shows l
    Unlink    l              -> showString "unlink " . shows l

-- | Every 'Process' action returns it's actual result wrapped in this type. It
-- will allow to signal errors as well as pass on normal results such as
-- incoming messages.
data ResumeProcess v where
  -- | The current operation of the process was interrupted with a
  -- 'ProcessExitReason'. If 'isRecoverable' holds for the given reason,
  -- the process may choose to continue.
  Interrupted :: ProcessExitReason 'Recoverable -> ResumeProcess v
  -- | The process may resume to do work, using the given result.
  ResumeWith :: a -> ResumeProcess a
  deriving ( Typeable, Generic, Generic1, Show )

instance NFData a => NFData (ResumeProcess a)

instance NFData1 ResumeProcess

-- | A function that deciced if the next message will be received by
-- 'ReceiveSelectedMessage'. It conveniently is an instance of 'Monoid'
-- with first come, first serve bais.
newtype MessageSelector a =
  MessageSelector {runMessageSelector :: Dynamic -> Maybe a }
  deriving (Semigroup, Monoid, Functor)

-- | Create a message selector for a value that can be obtained by 'fromDynamic'.
-- It will also 'force' the result.
--
-- @since 0.9.1
selectMessage :: (NFData t, Typeable t) => MessageSelector t
selectMessage = selectDynamicMessage fromDynamic

-- | Create a message selector for a value that can be obtained by 'fromDynamic'.
-- It will also 'force' the result.
--
-- @since 0.9.1
selectMessageLazy :: Typeable t => MessageSelector t
selectMessageLazy = selectDynamicMessageLazy fromDynamic

-- | Create a message selector from a predicate. It will 'force' the result.
--
-- @since 0.9.1
filterMessage :: (Typeable a, NFData a) => (a -> Bool) -> MessageSelector a
filterMessage predicate = selectDynamicMessage
  (\d -> case fromDynamic d of
    Just a | predicate a -> Just a
    _                    -> Nothing
  )

-- | Create a message selector from a predicate. It will 'force' the result.
--
-- @since 0.9.1
filterMessageLazy :: Typeable a => (a -> Bool) -> MessageSelector a
filterMessageLazy predicate = selectDynamicMessageLazy
  (\d -> case fromDynamic d of
    Just a | predicate a -> Just a
    _                    -> Nothing
  )

-- | Select a message of type @a@ and apply the given function to it.
-- If the function returns 'Just' The 'ReceiveSelectedMessage' function will
-- return the result (sans @Maybe@). It will 'force' the result.
--
-- @since 0.9.1
selectMessageWith
  :: (Typeable a, NFData b) => (a -> Maybe b) -> MessageSelector b
selectMessageWith f = selectDynamicMessage (fromDynamic >=> f)

-- | Select a message of type @a@ and apply the given function to it.
-- If the function returns 'Just' The 'ReceiveSelectedMessage' function will
-- return the result (sans @Maybe@). It will 'force' the result.
--
-- @since 0.9.1
selectMessageWithLazy :: Typeable a => (a -> Maybe b) -> MessageSelector b
selectMessageWithLazy f = selectDynamicMessageLazy (fromDynamic >=> f)

-- | Create a message selector. It will 'force' the result.
--
-- @since 0.9.1
selectDynamicMessage :: NFData a => (Dynamic -> Maybe a) -> MessageSelector a
selectDynamicMessage = MessageSelector . (force .)

-- | Create a message selector.
--
-- @since 0.9.1
selectDynamicMessageLazy :: (Dynamic -> Maybe a) -> MessageSelector a
selectDynamicMessageLazy = MessageSelector

-- | Create a message selector that will match every message. This is /lazy/
-- because the result is not 'force'ed.
--
-- @since 0.9.1
selectAnyMessageLazy :: MessageSelector Dynamic
selectAnyMessageLazy = MessageSelector Just

-- | Create a message selector for a value that can be obtained by 'fromDynamic'
-- with a proxy argument. It will also 'force' the result.
--
-- @since 0.9.1
selectMessageProxy
  :: forall proxy t . (NFData t, Typeable t) => proxy t -> MessageSelector t
selectMessageProxy _ = selectDynamicMessage fromDynamic

-- | Create a message selector for a value that can be obtained by 'fromDynamic'
-- with a proxy argument. It will also 'force' the result.
--
-- @since 0.9.1
selectMessageProxyLazy
  :: forall proxy t . (Typeable t) => proxy t -> MessageSelector t
selectMessageProxyLazy _ = selectDynamicMessageLazy fromDynamic

-- | Every function for 'Process' things needs such a proxy value
-- for the low-level effect list, i.e. the effects identified by
-- @__r__@ in @'Process' r : r@, this might be dependent on the
-- scheduler implementation.
data SchedulerProxy :: [Type -> Type] -> Type where
  -- | Tell the typechecker what effects we have below 'Process'
  SchedulerProxy :: SchedulerProxy q
  -- | Like 'SchedulerProxy' but shorter
  SP :: SchedulerProxy q
  -- | Like 'SP' but different
  Scheduler :: SchedulerProxy q

-- | /Cons/ 'Process' onto a list of effects.
type ConsProcess r = Process r ': r

-- | Return a 'SchedulerProxy' for a 'Process' effect.
thisSchedulerProxy :: Eff (Process r ': r) (SchedulerProxy r)
thisSchedulerProxy = return SchedulerProxy

-- | The state that a 'Process' is currently in.
data ProcessState =
    ProcessBooting              -- ^ The process has just been started but not
                                --   called 'handleProcess' yet.
  | ProcessIdle                 -- ^ The process yielded it's timeslice
  | ProcessBusy                 -- ^ The process is busy with non-blocking
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

instance Default ProcessState where def = ProcessBooting

-- | This kind is used to indicate if a 'ProcessExitReason' can be treated like
-- a short interrupt which can be handled or ignored.
data ExitRecovery = Recoverable | NoRecovery
  deriving (Typeable, Ord, Eq, Generic)

instance NFData ExitRecovery

instance Show ExitRecovery where
  showsPrec d =
    showParen (d>=10) .
    (\case
        Recoverable -> showString "recoverable"
        NoRecovery  -> showString "not recoverable")

-- | Get the 'ExitRecover'y
toExitRecovery :: ProcessExitReason r -> ExitRecovery
toExitRecovery = \case
  ExitNormally                 -> Recoverable
  (LinkedProcessCrashed _ _ _) -> Recoverable
  (ProcessError _            ) -> Recoverable
  (NotRecovered _            ) -> NoRecovery
  (UnexpectedException _ _   ) -> NoRecovery
  Killed                       -> NoRecovery

-- | This value indicates wether a process exitted in way consistent with
-- the planned behaviour or not.
data ExitSeverity = NormalExit | Crash
  deriving (Typeable, Ord, Eq, Generic)

instance Show ExitSeverity where
  showsPrec d =
    showParen (d>=10) .
    (\case
        NormalExit -> showString "exit success"
        Crash      -> showString "crash")

instance NFData ExitSeverity

-- | Get the 'ExitSeverity' of a 'ProcessExitReason'.
toExitSeverity :: ProcessExitReason e -> ExitSeverity
toExitSeverity = \case
  ExitNormally -> NormalExit
  _            -> Crash

-- | A sum-type with reasons for why a process exists the scheduling loop,
-- this includes errors, that can occur when scheduleing messages.
data ProcessExitReason (t :: ExitRecovery) where
    ExitNormally
      :: ProcessExitReason 'Recoverable
    -- ^ A process function returned or exitted without any error.
    LinkedProcessCrashed
      :: ProcessId -> ExitSeverity -> ExitRecovery -> ProcessExitReason 'Recoverable
    -- ^ A linked process is down
    ProcessError
      :: String -> ProcessExitReason 'Recoverable
    -- ^ An exit reason that has an error message but isn't 'Recoverable'.
    NotRecovered
      :: (ProcessExitReason 'Recoverable) -> ProcessExitReason 'NoRecovery
    -- ^ An unhandled 'Recoverable' allows 'NoRecovery'.
    UnexpectedException
      :: String -> String -> ProcessExitReason 'NoRecovery
    -- ^ An unexpected runtime exception was thrown, i.e. an exception
    -- derived from 'Control.Exception.Safe.SomeException'
    Killed
      :: ProcessExitReason 'NoRecovery
    -- ^ A process was cancelled (e.g. killed, in 'Async.cancel')
  deriving Typeable

instance Show (ProcessExitReason x) where
  showsPrec d =
    showParen (d>=10) .
    (\case
        ExitNormally                -> showString "exit normally"
        LinkedProcessCrashed m n o  ->  showString "linked process crashed: "
                                        . shows m . showChar ' '
                                        . shows n . showChar ' '
                                        . shows o
        ProcessError reason         -> showString "error: " . showString reason
        NotRecovered         e      -> showString "not recovered from: " . shows e
        UnexpectedException w m     ->   showString "unhandled runtime exception: "
                                       . showString m
                                       . showString " caught here: "
                                       . showString w
        Killed                      -> showString "killed"
    )

instance Exc.Exception (ProcessExitReason 'Recoverable)
instance Exc.Exception (ProcessExitReason 'NoRecovery )

instance NFData (ProcessExitReason x) where
  rnf ExitNormally = rnf ()
  rnf (LinkedProcessCrashed !l !m !n) = rnf l `seq` rnf m `seq` rnf n `seq` ()
  rnf (ProcessError !l) = rnf l
  rnf (NotRecovered !l) = rnf l
  rnf (UnexpectedException !l1 !l2) = rnf l1 `seq` rnf l2 `seq` ()
  rnf Killed = rnf ()

instance Ord (ProcessExitReason x) where
  compare ExitNormally ExitNormally = EQ
  compare ExitNormally _ = LT
  compare _ ExitNormally = GT
  compare (LinkedProcessCrashed l m n) (LinkedProcessCrashed r s t) = compare l r <> compare m s <> compare n t
  compare (LinkedProcessCrashed _ _ _) _ = LT
  compare _ (LinkedProcessCrashed _ _ _) = GT
  compare (ProcessError l) (ProcessError r) = compare l r
  compare (NotRecovered l) (NotRecovered r) = compare l r
  compare (NotRecovered _) _ = LT
  compare _ (NotRecovered _) = GT
  compare (UnexpectedException l1 l2) (UnexpectedException r1 r2) =
    compare l1 r1 <> compare l2 r2
  compare (UnexpectedException _ _) _ = LT
  compare _ (UnexpectedException _ _) = GT
  compare Killed Killed = EQ

instance Eq (ProcessExitReason x) where
  (==) ExitNormally ExitNormally = True
  (==) (LinkedProcessCrashed l m n) (LinkedProcessCrashed r s t) = l == r && m == s && n == t
  (==) (ProcessError l) (ProcessError r) = (==) l r
  (==) (NotRecovered l) (NotRecovered r) = (==) l r
  (==) (UnexpectedException l1 l2) (UnexpectedException r1 r2) =
    (==) l1 r1 && (==) l2 r2
  (==) Killed Killed = True
  (==) _ _ = False

-- | A predicate for crashes. A /crash/ happens when a process exits
-- with an 'ExitReason' other than 'ExitNormally'
isCrash :: ProcessExitReason x -> Bool
isCrash (NotRecovered !x) = isCrash x
isCrash ExitNormally      = False
isCrash _                 = True

-- | A predicate for recoverable exit reasons. This predicate defines the
-- exit reasonson which functions such as 'executeAndCatch'
isRecoverable :: ProcessExitReason x -> Bool
isRecoverable (toExitRecovery -> Recoverable) = True
isRecoverable _                               = False

-- | An existential wrapper around 'ProcessExitReason'
data SomeProcessExitReason where
  SomeProcessExitReason :: ProcessExitReason x -> SomeProcessExitReason

instance Eq SomeProcessExitReason where
  (==) = (==) `on` fromSomeProcessExitReason

-- | Partition a 'SomeProcessExitReason' back into either a 'NoRecovery'
-- or a 'Recoverable' 'ProcessExitReason'
fromSomeProcessExitReason
  :: SomeProcessExitReason
  -> Either
       (ProcessExitReason 'NoRecovery)
       (ProcessExitReason 'Recoverable)
fromSomeProcessExitReason (SomeProcessExitReason e) = case e of
  recoverable@ExitNormally                 -> Right recoverable
  recoverable@(LinkedProcessCrashed _ _ _) -> Right recoverable
  recoverable@(ProcessError _            ) -> Right recoverable
  noRecovery@( NotRecovered _            ) -> Left noRecovery
  noRecovery@( UnexpectedException _ _   ) -> Left noRecovery
  noRecovery@Killed                        -> Left noRecovery

-- | Print a 'ProcessExitReason' to 'Just' a formatted 'String' when 'isCrash'
-- is 'True'.
-- This can be useful in combination with view patterns, e.g.:
--
-- > logCrash :: ProcessExitReason -> Eff e ()
-- > logCrash (toCrashReason -> Just reason) = logError reason
-- > logCrash _ = return ()
--
-- Though this can be improved to:
--
-- > logCrash = traverse_ logError . toCrashReason
--
toCrashReason :: ProcessExitReason x -> Maybe String
toCrashReason e | isCrash e = Just (show e)
                | otherwise = Nothing

-- | Log the 'ProcessExitReaons'
logProcessExit
  :: (HasCallStack, HasLogWriter LogMessage h e)
  => ProcessExitReason x
  -> Eff e ()
logProcessExit (toCrashReason -> Just ex) = withFrozenCallStack (logError ex)
logProcessExit ex = withFrozenCallStack (logInfo (show ex))


-- | Execute a and action and return the result;
-- if the process is interrupted by an error or exception, or an explicit
-- shutdown from another process, or through a crash of a linked process, i.e.
-- whenever the exit reason satisfies 'isRecoverable', return the exit reason.
executeAndCatch
  :: forall q r v
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> Eff r (ResumeProcess v)
  -> Eff r (Either (ProcessExitReason 'Recoverable) v)
executeAndCatch _px processAction = do
  result <- processAction
  case result of
    ResumeWith  !value -> return (Right value)
    Interrupted r      -> return (Left r)

-- | Execute a 'Process' action and resume the process, exit
-- the process when an 'Interrupt' was raised. Use 'executeAndCatch' to catch
-- interrupts.
executeAndResume
  :: forall r q v
   . (SetMember Process (Process q) r, HasCallStack)
  => Process q (ResumeProcess v)
  -> Eff r v
executeAndResume processAction = do
  result <- send processAction
  case result of
    ResumeWith  !value -> return value
    Interrupted r      -> send (Shutdown @q (NotRecovered r))

-- * Process Effects

-- | Use 'executeAndResume' to execute 'YieldProcess'. Refer to 'YieldProcess'
-- for more information.
yieldProcess
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r ()
yieldProcess _ = executeAndResume YieldProcess

-- | Send a message to a process addressed by the 'ProcessId'.
-- See 'SendMessage'.
sendMessage
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> Dynamic
  -> Eff r ()
sendMessage px pid message = void (sendMessageChecked px pid message)

-- | Send a message to a process addressed by the 'ProcessId'.
-- See 'SendMessage'. Return @True@ if the process existed.
-- I you don't care, just 'sendMessage' instead.
sendMessageChecked
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> Dynamic
  -> Eff r Bool
sendMessageChecked _ pid message =
  executeAndResume (SendMessage pid $! message)

-- | Send a message to a process addressed by the 'ProcessId'.
-- See 'SendMessage'.
sendMessageAs
  :: forall o r q
   . (HasCallStack, SetMember Process (Process q) r, Typeable o)
  => SchedulerProxy q
  -> ProcessId
  -> o
  -> Eff r ()
sendMessageAs px pid = sendMessage px pid . toDyn

-- | Exit a process addressed by the 'ProcessId'. The process will exit,
-- it might do some cleanup, but is ultimately unrecoverable.
-- See 'SendShutdown'.
sendShutdown
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> ProcessExitReason 'NoRecovery
  -> Eff r ()
sendShutdown px pid s = void (sendShutdownChecked px pid s)

-- | Like 'sendShutdown', but also return @True@ iff the process to exit exists.
sendShutdownChecked
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> ProcessExitReason 'NoRecovery
  -> Eff r Bool
sendShutdownChecked _ pid s =
  pid `deepseq` s `deepseq` executeAndResume (SendShutdown pid s)

-- | Interrupt a process addressed by the 'ProcessId'. The process might exit,
-- or it may continue.
sendInterrupt
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> ProcessExitReason 'Recoverable
  -> Eff r ()
sendInterrupt px pid s = void (sendInterruptChecked px pid s)

-- | Like 'sendInterrupt', but also return @True@ iff the process to exit exists.
sendInterruptChecked
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> ProcessExitReason 'Recoverable
  -> Eff r Bool
sendInterruptChecked _ pid s =
  pid `deepseq` s `deepseq` executeAndResume (SendInterrupt pid s)

-- | Start a new process, the new process will execute an effect, the function
-- will return immediately with a 'ProcessId'.
spawn
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => Eff (Process q ': q) ()
  -> Eff r ProcessId
spawn child = executeAndResume (Spawn @q child)

-- | Like 'spawn' but return @()@.
spawn_
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => Eff (Process q ': q) ()
  -> Eff r ()
spawn_ = void . spawn

-- | Block until a message was received.
-- See 'ReceiveMessage' for more documentation.
receiveAnyMessage
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r Dynamic
receiveAnyMessage _ =
  executeAndResume (ReceiveSelectedMessage selectAnyMessageLazy)

-- | Block until a message was received, that is not 'Nothing' after applying
-- a callback to it.
-- See 'ReceiveSelectedMessage' for more documentation.
receiveSelectedMessage
  :: forall r q a
   . (HasCallStack, Typeable a, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> MessageSelector a
  -> Eff r a
receiveSelectedMessage _ f = executeAndResume (ReceiveSelectedMessage f)

-- | Receive and cast the message to some 'Typeable' instance.
-- See 'ReceiveSelectedMessage' for more documentation.
-- This will wait for a message of the return type using 'receiveSelectedMessage'
receiveMessageAs
  :: forall a r q
   . (HasCallStack, Typeable a, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r a
receiveMessageAs px = receiveSelectedMessage px (MessageSelector fromDynamic)

-- | Enter a loop to receive messages and pass them to a callback, until the
-- function returns 'Just' a result.
-- Only the messages of the given type will be received.
-- If the process is interrupted by an exception of by a 'SendShutdown' from
-- another process, with an exit reason that satisfies 'isRecoverable', then
-- the callback will be invoked with @'Left' 'ProcessExitReaon'@, otherwise the
-- process will be exited with the same reason using 'exitBecause'.
-- See also 'ReceiveSelectedMessage' for more documentation.
receiveSelectedLoop
  :: forall r q a endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> MessageSelector a
  -> (  Either (ProcessExitReason 'Recoverable) a
     -> Eff r (Maybe endOfLoopResult)
     )
  -> Eff r endOfLoopResult
receiveSelectedLoop px selectMesage handlers = do
  mReq <- send (ReceiveSelectedMessage @q @a selectMesage)
  mRes <- case mReq of
    Interrupted reason  -> handlers (Left reason)
    ResumeWith  message -> handlers (Right message)
  maybe (receiveSelectedLoop px selectMesage handlers) return mRes

-- | Like 'receiveSelectedLoop' but /not selective/.
-- See also 'selectAnyMessageLazy', 'receiveSelectedLoop'.
receiveAnyLoop
  :: forall r q endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> (  Either (ProcessExitReason 'Recoverable) Dynamic
     -> Eff r (Maybe endOfLoopResult)
     )
  -> Eff r endOfLoopResult
receiveAnyLoop px = receiveSelectedLoop px selectAnyMessageLazy

-- | Like 'receiveSelectedLoop' but refined to casting to a specific 'Typeable'
-- using 'selectMessageLazy'.
receiveLoop
  :: forall r q a endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack, Typeable a)
  => SchedulerProxy q
  -> (  Either (ProcessExitReason 'Recoverable) a
     -> Eff r (Maybe endOfLoopResult)
     )
  -> Eff r endOfLoopResult
receiveLoop px = receiveSelectedLoop px selectMessageLazy

-- | Returns the 'ProcessId' of the current process.
self
  :: (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r ProcessId
self _px = executeAndResume SelfPid

-- | Generate a unique 'Int' for the current process.
makeReference
  :: (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r Int
makeReference _px = executeAndResume MakeReference

-- | Monitor another process. When the monitored process exits a
--  'MonitoredProcessDown' is sent to the calling process.
-- The return value is a unique identifier for that monitor.
-- There can be multiple monitors on the same process,
-- and a message for each will be sent.
-- If the process is already dead, the 'MonitoredProcessDown' message
-- will be sent immediately, w.thout exit reason
monitor
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> Eff r Int
monitor _px = executeAndResume . Monitor . force

-- | Remove a monitor created with 'monitor'.
demonitor
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Int
  -> Eff r ()
demonitor _px = executeAndResume . Demonitor . force

-- | Connect the calling process to another process, such that
-- if one of the processes crashes (i.e. 'isCrash' returns 'True'), the other
-- is shutdown with the 'ProcessExitReaon' 'LinkedProcessCrashed'.
linkProcess
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> Eff r ()
linkProcess _px = executeAndResume . Link . force

-- | Unlink the calling proccess from the other process.
unlinkProcess
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> Eff r ()
unlinkProcess _px = executeAndResume . Unlink . force

-- | Exit the process with a 'ProcessExitReaon'.
exitBecause
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessExitReason 'NoRecovery
  -> Eff r a
exitBecause _ = send . Shutdown @q . force

-- | Exit the process.
exitNormally
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r a
exitNormally px = exitBecause px (NotRecovered ExitNormally)

-- | Exit the process with an error.
exitWithError
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> String
  -> Eff r a
exitWithError px = exitBecause px . NotRecovered . ProcessError

-- | Each process is identified by a single process id, that stays constant
-- throughout the life cycle of a process. Also, message sending relies on these
-- values to address messages to processes.
newtype ProcessId = ProcessId { _fromProcessId :: Int }
  deriving (Eq,Ord,Typeable,Bounded,Num, Enum, Integral, Real, NFData)

instance Read ProcessId where
  readsPrec _ ('!':rest1) =
    case reads rest1 of
      [(c, rest2)] -> [(ProcessId c, rest2)]
      _ -> []
  readsPrec _ _ = []

instance Show ProcessId where
  showsPrec _ (ProcessId !c) = showChar '!' . shows c

makeLenses ''ProcessId

-- * Scheduler Event Messages

-- | A monitored process exitted.
-- This message is sent to a process by the scheduler, when
-- a process that was monitored via a 'SchedulerCommand' died.
data MonitoredProcessDown x =
  MonitoredProcessDown ProcessId Int (ProcessExitReason x)
  deriving (Typeable, Generic, Eq, Ord)

instance NFData (MonitoredProcessDown x)

instance Show (MonitoredProcessDown x) where
  showsPrec d =
    showParen
      (d>=10)
    . (\case
          MonitoredProcessDown pid ref reason ->
            showString "monitored process down "
             . showsPrec 11 pid . showChar ' '
             . showsPrec 11 ref . showChar ' '
             . showsPrec 11 reason
      )

-- | A process linked to the current process via a 'SchedulerCommand' crashed,
-- and the current process
data CaughtLinkedProcessCrashed x =
  CaughtLinkedProcessCrashed ProcessId (ProcessExitReason x)
  deriving (Typeable, Generic, Eq, Ord)

instance NFData (CaughtLinkedProcessCrashed x)

instance Show (CaughtLinkedProcessCrashed x) where
  showsPrec d =
    showParen
      (d>=10)
    . (\case
          CaughtLinkedProcessCrashed pid reason ->
            showString "caught unlink from "
             . showsPrec 11 pid . showChar ' '
             . showsPrec 11 reason
      )
