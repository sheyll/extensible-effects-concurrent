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
    -- ** Effect Type Handling
    Process(..)
    -- ** ProcessId Type
  , ProcessId(..)
  , fromProcessId
  , ConsProcess
  , ResumeProcess(..)
  -- ** Scheduler Effect Identification
  , SchedulerProxy(..)
  , HasScheduler
  , getSchedulerProxy
  , withSchedulerProxy
  , thisSchedulerProxy
  -- ** Process State
  , ProcessState(..)
  -- ** Yielding
  , yieldProcess
  -- ** Sending Messages
  , sendMessage
  , sendAnyMessage
  , sendShutdown
  , sendInterrupt
  -- ** Utilities
  , makeReference
  -- ** Receiving Messages
  , receiveAnyMessage
  , receiveMessage
  , receiveSelectedMessage
  , flushMessages
  , receiveAnyLoop
  , receiveLoop
  , receiveSelectedLoop
  -- *** Selecting Messages to Receive
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
  -- ** Process Life Cycle Management
  , self
  , isProcessAlive
  -- *** Spawning
  , spawn
  , spawn_
  , spawnLink
  , spawnRaw
  , spawnRaw_
  -- *** Process Exit or Interrupt
  , exitBecause
  , exitNormally
  , exitWithError
  -- *** Links
  , linkProcess
  , unlinkProcess
  -- *** Monitors
  , monitor
  , demonitor
  , ProcessDown(..)
  , selectProcessDown
  , becauseProcessIsDown
  , MonitorReference(..)
  , withMonitor
  , receiveWithMonitor
  -- *** Process Interrupt Handling
  , provideInterruptsShutdown
  , handleInterrupts
  , exitOnInterrupt
  , logInterrupts
  , provideInterrupts
  , mergeEitherInterruptAndExitReason
  , interrupt
  -- *** Process Operation Execution
  , executeAndResume
  , executeAndResumeOrExit
  , executeAndResumeOrThrow
  -- *** Exit Or Interrupt Reasons
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
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.Exception
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
import           Control.Applicative
import           Data.Maybe
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
-- * the spawnRaw
--
-- * when the first process exists, all process should be killed immediately
data Process (r :: [Type -> Type]) b where
  -- | Remove all messages from the process' message queue
  FlushMessages :: Process r (ResumeProcess [Dynamic])
  -- | In cooperative schedulers, this will give processing time to the
  -- scheduler. Every other operation implicitly serves the same purpose.
  YieldProcess :: Process r (ResumeProcess ())
  -- | Return the current 'ProcessId'
  SelfPid :: Process r (ResumeProcess ProcessId)
  -- | Start a new process, the new process will execute an effect, the function
  -- will return immediately with a 'ProcessId'.
  Spawn :: Eff (Process r ': r) () -> Process r (ResumeProcess ProcessId)
  -- | Start a new process, and 'Link' to it .
  SpawnLink :: Eff (Process r ': r) () -> Process r (ResumeProcess ProcessId)
  -- | Get the process state (or 'Nothing' if the process is dead)
  GetProcessState :: ProcessId -> Process r (ResumeProcess (Maybe ProcessState))
  -- | Shutdown the process; irregardles of the exit reason, this function never
  -- returns,
  Shutdown :: ExitReason 'NoRecovery   -> Process r a
  -- | Raise an error, that can be handled.
  SendShutdown :: ProcessId  -> ExitReason 'NoRecovery  -> Process r (ResumeProcess ())
  -- | Request that another a process interrupts. The targeted process is interrupted
  -- and gets an 'Interrupted', the target process may decide to ignore the
  -- interrupt and continue as if nothing happened.
  SendInterrupt :: ProcessId -> InterruptReason -> Process r (ResumeProcess ())
  -- | Send a message to a process addressed by the 'ProcessId'. Sending a
  -- message should **always succeed** and return **immediately**, even if the
  -- destination process does not exist, or does not accept messages of the
  -- given type.
  SendMessage :: ProcessId -> Dynamic -> Process r (ResumeProcess ())
  -- | Receive a message that matches a criterium.
  -- This should block until an a message was received. The message is returned
  -- as a 'ProcessMessage' value. The function should also return if an exception
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
  -- will be sent immediately, w.thout exit reason
  Monitor :: ProcessId -> Process r (ResumeProcess MonitorReference)
  -- | Remove a monitor.
  Demonitor :: MonitorReference -> Process r (ResumeProcess ())
  -- | Connect the calling process to another process, such that
  -- if one of the processes crashes (i.e. 'isCrash' returns 'True'), the other
  -- is shutdown with the 'ProcessExitReaon' 'LinkedProcessCrashed'.
  Link :: ProcessId -> Process r (ResumeProcess ())
  -- | Unlink the calling proccess from the other process.
  Unlink :: ProcessId -> Process r (ResumeProcess ())

instance Show (Process r b) where
  showsPrec d = \case
    FlushMessages -> showString "flush messages"
    YieldProcess  -> showString "yield process"
    SelfPid       -> showString "lookup the current process id"
    Spawn    _    -> showString "spawn a new process"
    SpawnLink _   -> showString "spawn a new process and link to it"
    Shutdown sr   -> showParen (d >= 10) (showString "shutdown " . showsPrec 10 sr)
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
    GetProcessState pid      -> showString "get the process state of " . shows pid
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
  -- 'ExitReason'. If 'isRecoverable' holds for the given reason,
  -- the process may choose to continue.
  Interrupted :: InterruptReason -> ResumeProcess v
  -- | The process may resume to do work, using the given result.
  ResumeWith :: a -> ResumeProcess a
  deriving ( Typeable, Generic, Generic1, Show )

instance NFData a => NFData (ResumeProcess a)

instance NFData1 ResumeProcess

-- | A function that deciced if the next message will be received by
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
  MessageSelector {runMessageSelector :: Dynamic -> Maybe a }
  deriving (Semigroup, Monoid, Functor)

instance Applicative MessageSelector where
  pure = MessageSelector . pure . pure
  (MessageSelector f) <*> (MessageSelector x) =
    MessageSelector (\dyn -> f dyn <*> x dyn)

instance Alternative MessageSelector where
  empty = MessageSelector (const empty)
  (MessageSelector l) <|> (MessageSelector r) =
    MessageSelector (\dyn -> l dyn <|> r dyn)

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

-- | A constraint for the implicit 'SchedulerProxy' parameter.
-- Use 'getSchedulerProxy' to query it.
--
-- @since 0.12.0
type HasScheduler q = (?_schedulerProxy :: SchedulerProxy q)

-- | Get access to the 'SchedulerProxy' for the current scheduler effects.
--
-- @since 0.12.0
getSchedulerProxy :: HasScheduler q => SchedulerProxy q
getSchedulerProxy = ?_schedulerProxy

-- | Set the 'SchedulerProxy' to use, this satisfies 'HasScheduler' .
--
-- @since 0.12.0
withSchedulerProxy :: SchedulerProxy q -> (HasScheduler q => a) -> a
withSchedulerProxy px x = let ?_schedulerProxy = px in x


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

-- | This kind is used to indicate if a 'ExitReason' can be treated like
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
toExitRecovery :: ExitReason r -> ExitRecovery
toExitRecovery = \case
  (ProcessNotRunning    _)  -> Recoverable
  (LinkedProcessCrashed _)  -> Recoverable
  (ProcessError         _)  -> Recoverable
  ExitNormally              -> NoRecovery
  (NotRecovered _         ) -> NoRecovery
  (UnexpectedException _ _) -> NoRecovery
  Killed                    -> NoRecovery

-- | This value indicates wether a process exited in way consistent with
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

-- | Get the 'ExitSeverity' of a 'ExitReason'.
toExitSeverity :: ExitReason e -> ExitSeverity
toExitSeverity = \case
  ExitNormally -> NormalExit
  _            -> Crash

-- | A sum-type with reasons for why a process exists the scheduling loop,
-- this includes errors, that can occur when scheduleing messages.
data ExitReason (t :: ExitRecovery) where
    -- | A process that should be running was not running.
    ProcessNotRunning
      :: ProcessId -> ExitReason 'Recoverable
    -- | A linked process is down
    LinkedProcessCrashed
      :: ProcessId -> ExitReason 'Recoverable
    -- | An exit reason that has an error message but isn't 'Recoverable'.
    ProcessError
      :: String -> ExitReason 'Recoverable
    -- | A process function returned or exited without any error.
    ExitNormally
      :: ExitReason 'NoRecovery
    -- | An unhandled 'Recoverable' allows 'NoRecovery'.
    NotRecovered
      :: (ExitReason 'Recoverable) -> ExitReason 'NoRecovery
    -- | An unexpected runtime exception was thrown, i.e. an exception
    --    derived from 'Control.Exception.Safe.SomeException'
    UnexpectedException
      :: String -> String -> ExitReason 'NoRecovery
    -- | A process was cancelled (e.g. killed, in 'Async.cancel')
    Killed
      :: ExitReason 'NoRecovery
  deriving Typeable

instance Show (ExitReason x) where
  showsPrec d =
    showParen (d>=10) .
    (\case
        ProcessNotRunning p         -> showString "process not running: " . shows p
        LinkedProcessCrashed m      -> showString "linked process "
                                        . shows m . showString " crashed"
        ProcessError reason         -> showString "error: " . showString reason
        ExitNormally                -> showString "exit normally"
        NotRecovered         e      -> showString "not recovered from: " . shows e
        UnexpectedException w m     -> showString "unhandled runtime exception: "
                                       . showString m
                                       . showString " caught here: "
                                       . showString w
        Killed                      -> showString "killed"
    )

instance Exc.Exception (ExitReason 'Recoverable)
instance Exc.Exception (ExitReason 'NoRecovery )

instance NFData (ExitReason x) where
  rnf (ProcessNotRunning !l) = rnf l
  rnf (LinkedProcessCrashed !l) = rnf l
  rnf (ProcessError !l) = rnf l
  rnf ExitNormally = rnf ()
  rnf (NotRecovered !l) = rnf l
  rnf (UnexpectedException !l1 !l2) = rnf l1 `seq` rnf l2 `seq` ()
  rnf Killed = rnf ()

instance Ord (ExitReason x) where
  compare (ProcessNotRunning l) (ProcessNotRunning r) = compare l r
  compare (ProcessNotRunning _) _ = LT
  compare _ (ProcessNotRunning _) = GT
  compare (LinkedProcessCrashed l) (LinkedProcessCrashed r) = compare l r
  compare (LinkedProcessCrashed _) _ = LT
  compare _ (LinkedProcessCrashed _) = GT
  compare (ProcessError l) (ProcessError r) = compare l r
  compare ExitNormally ExitNormally = EQ
  compare ExitNormally _ = LT
  compare _ ExitNormally = GT
  compare (NotRecovered l) (NotRecovered r) = compare l r
  compare (NotRecovered _) _ = LT
  compare _ (NotRecovered _) = GT
  compare (UnexpectedException l1 l2) (UnexpectedException r1 r2) =
    compare l1 r1 <> compare l2 r2
  compare (UnexpectedException _ _) _ = LT
  compare _ (UnexpectedException _ _) = GT
  compare Killed Killed = EQ

instance Eq (ExitReason x) where
  (==) (ProcessNotRunning l) (ProcessNotRunning r) = (==) l r
  (==) ExitNormally ExitNormally = True
  (==) (LinkedProcessCrashed l) (LinkedProcessCrashed r) = l == r
  (==) (ProcessError l) (ProcessError r) = (==) l r
  (==) (NotRecovered l) (NotRecovered r) = (==) l r
  (==) (UnexpectedException l1 l2) (UnexpectedException r1 r2) =
    (==) l1 r1 && (==) l2 r2
  (==) Killed Killed = True
  (==) _ _ = False

-- | A predicate for linked process __crashes__.
isBecauseDown :: Maybe ProcessId -> ExitReason r -> Bool
isBecauseDown mp = \case
  ProcessNotRunning    _  -> False
  LinkedProcessCrashed p  -> maybe True (== p) mp
  ProcessError         _  -> False
  ExitNormally            -> False
  NotRecovered e          -> isBecauseDown mp e
  UnexpectedException _ _ -> False
  Killed                  -> False

-- | 'ExitReason's which are recoverable are interrupts.
type InterruptReason = ExitReason 'Recoverable

-- | 'Exc'eptions containing 'InterruptReason's.
-- See 'handleInterrupts', 'exitOnInterrupt' or 'provideInterrupts'
type Interrupts = Exc InterruptReason

-- | This adds a layer of the 'Interrupts' effect ontop of 'ConsProcess'
type InterruptableProcess e = Interrupts ': ConsProcess e

-- | Handle all 'InterruptReason's of an 'InterruptableProcess' by
-- wrapping them up in 'NotRecovered' and then do a process 'Shutdown'.
provideInterruptsShutdown
  :: forall e a . Eff (InterruptableProcess e) a -> Eff (ConsProcess e) a
provideInterruptsShutdown e = do
  res <- provideInterrupts e
  case res of
    Left  ex -> send (Shutdown @e (NotRecovered ex))
    Right a  -> return a

-- | Handle 'InterruptReason's arising during process operations, e.g.
-- when a linked process crashes while we wait in a 'receiveSelectedMessage'
-- via a call to 'interrupt'.
handleInterrupts
  :: (HasCallStack, Member Interrupts r)
  => (InterruptReason -> Eff r a)
  -> Eff r a
  -> Eff r a
handleInterrupts = flip catchError

-- | Handle interrupts by logging them with `logProcessExit` and otherwise
-- ignoring them.
logInterrupts
  :: (HasCallStack, '[Interrupts, Logs LogMessage] <:: r)
  => Eff r ()
  -> Eff r ()
logInterrupts = handleInterrupts logProcessExit

-- | Handle 'InterruptReason's arising during process operations, e.g.
-- when a linked process crashes while we wait in a 'receiveSelectedMessage'
-- via a call to 'interrupt'.
exitOnInterrupt
  :: (HasCallStack, Member Interrupts r, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r a
  -> Eff r a
exitOnInterrupt px = handleInterrupts (exitBecause px . NotRecovered)

-- | Handle 'InterruptReason's arising during process operations, e.g.
-- when a linked process crashes while we wait in a 'receiveSelectedMessage'
-- via a call to 'interrupt'.
provideInterrupts
  :: HasCallStack => Eff (Interrupts ': r) a -> Eff r (Either InterruptReason a)
provideInterrupts = runError


-- | Wrap all (left) 'InterruptReason's into 'NotRecovered' and
-- return the (right) 'NoRecovery' 'ExitReason's as is.
mergeEitherInterruptAndExitReason
  :: Either InterruptReason (ExitReason 'NoRecovery) -> ExitReason 'NoRecovery
mergeEitherInterruptAndExitReason = either NotRecovered id

-- | Throw an 'InterruptReason', can be handled by 'recoverFromInterrupt' or
--   'exitOnInterrupt' or 'provideInterrupts'.
interrupt :: (HasCallStack, Member Interrupts r) => InterruptReason -> Eff r a
interrupt = throwError

-- | A predicate for crashes. A /crash/ happens when a process exits
-- with an 'ExitReason' other than 'ExitNormally'
isCrash :: ExitReason x -> Bool
isCrash (NotRecovered !x) = isCrash x
isCrash ExitNormally      = False
isCrash _                 = True

-- | A predicate for recoverable exit reasons. This predicate defines the
-- exit reasonson which functions such as 'executeAndResume'
isRecoverable :: ExitReason x -> Bool
isRecoverable (toExitRecovery -> Recoverable) = True
isRecoverable _                               = False

-- | An existential wrapper around 'ExitReason'
data SomeExitReason where
  SomeExitReason :: ExitReason x -> SomeExitReason

instance Ord SomeExitReason where
  compare = compare `on` fromSomeExitReason

instance Eq SomeExitReason where
  (==) = (==) `on` fromSomeExitReason

instance Show SomeExitReason where
  show = show . fromSomeExitReason

instance NFData SomeExitReason where
  rnf = rnf . fromSomeExitReason

-- | Partition a 'SomeExitReason' back into either a 'NoRecovery'
-- or a 'Recoverable' 'ExitReason'
fromSomeExitReason
  :: SomeExitReason -> Either (ExitReason 'NoRecovery) InterruptReason
fromSomeExitReason (SomeExitReason e) = case e of
  recoverable@(ProcessNotRunning    _) -> Right recoverable
  recoverable@(LinkedProcessCrashed _) -> Right recoverable
  recoverable@(ProcessError         _) -> Right recoverable
  noRecovery@ExitNormally              -> Left noRecovery
  noRecovery@(NotRecovered _         ) -> Left noRecovery
  noRecovery@(UnexpectedException _ _) -> Left noRecovery
  noRecovery@Killed                    -> Left noRecovery

-- | Print a 'ExitReason' to 'Just' a formatted 'String' when 'isCrash'
-- is 'True'.
-- This can be useful in combination with view patterns, e.g.:
--
-- > logCrash :: ExitReason -> Eff e ()
-- > logCrash (toCrashReason -> Just reason) = logError reason
-- > logCrash _ = return ()
--
-- Though this can be improved to:
--
-- > logCrash = traverse_ logError . toCrashReason
--
toCrashReason :: ExitReason x -> Maybe String
toCrashReason e | isCrash e = Just (show e)
                | otherwise = Nothing

-- | Log the 'ProcessExitReaons'
logProcessExit
  :: (HasCallStack, Member (Logs LogMessage) e) => ExitReason x -> Eff e ()
logProcessExit (toCrashReason -> Just ex) = withFrozenCallStack (logError ex)
logProcessExit ex = withFrozenCallStack (logDebug (show ex))


-- | Execute a and action and return the result;
-- if the process is interrupted by an error or exception, or an explicit
-- shutdown from another process, or through a crash of a linked process, i.e.
-- whenever the exit reason satisfies 'isRecoverable', return the exit reason.
executeAndResume
  :: forall q r v
   . (SetMember Process (Process q) r, HasCallStack)
  => Process q (ResumeProcess v)
  -> Eff r (Either (ExitReason 'Recoverable) v)
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
   . (SetMember Process (Process q) r, HasCallStack)
  => Process q (ResumeProcess v)
  -> Eff r v
executeAndResumeOrExit processAction = do
  result <- send processAction
  case result of
    ResumeWith  !value -> return value
    Interrupted r      -> send (Shutdown @q (NotRecovered r))

-- | Execute a 'Process' action and resume the process, exit
-- the process when an 'Interrupts' was raised. Use 'executeAndResume' to catch
-- interrupts.
executeAndResumeOrThrow
  :: forall q r v
   . (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
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
   . (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
  => SchedulerProxy q
  -> Eff r ()
yieldProcess _ = executeAndResumeOrThrow YieldProcess

-- | Send a message to a process addressed by the 'ProcessId'.
-- See 'SendMessage'.
--
sendMessage
  :: forall r q o
   . ( SetMember Process (Process q) r
     , HasCallStack
     , Member Interrupts r
     , Typeable o
     )
  => SchedulerProxy q
  -> ProcessId
  -> o
  -> Eff r ()
sendMessage _ pid message =
  executeAndResumeOrThrow (SendMessage pid $! toDyn $! message)

-- | Send a 'Dynamic' value to a process addressed by the 'ProcessId'.
-- See 'SendMessage'.
sendAnyMessage
  :: forall r q
   . (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
  => SchedulerProxy q
  -> ProcessId
  -> Dynamic
  -> Eff r ()
sendAnyMessage _ pid message =
  rnf pid `seq` executeAndResumeOrThrow (SendMessage pid $! message)

-- | Exit a process addressed by the 'ProcessId'. The process will exit,
-- it might do some cleanup, but is ultimately unrecoverable.
-- See 'SendShutdown'.
sendShutdown
  :: forall r q
   . (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
  => SchedulerProxy q
  -> ProcessId
  -> ExitReason 'NoRecovery
  -> Eff r ()
sendShutdown _ pid s =
  pid `deepseq` s `deepseq` executeAndResumeOrThrow (SendShutdown pid s)

-- | Interrupts a process addressed by the 'ProcessId'. The process might exit,
-- or it may continue.
-- | Like 'sendInterrupt', but also return @True@ iff the process to exit exists.
sendInterrupt
  :: forall r q
   . (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
  => SchedulerProxy q
  -> ProcessId
  -> InterruptReason
  -> Eff r ()
sendInterrupt _ pid s =
  pid `deepseq` s `deepseq` executeAndResumeOrThrow (SendInterrupt pid s)

-- | Start a new process, the new process will execute an effect, the function
-- will return immediately with a 'ProcessId'. If the new process is
-- interrupted, the process will 'Shutdown' with the 'InterruptReason'
-- wrapped in 'NotCovered'. For specific use cases it might be better to use
-- 'spawnRaw'.
spawn
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => Eff (InterruptableProcess q) ()
  -> Eff r ProcessId
spawn child =
  executeAndResumeOrThrow (Spawn @q (provideInterruptsShutdown child))

-- | Like 'spawn' but return @()@.
spawn_
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => Eff (InterruptableProcess q) ()
  -> Eff r ()
spawn_ child = void (spawn child)

-- | Start a new process, and immediately link to it.
spawnLink
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => Eff (InterruptableProcess q) ()
  -> Eff r ProcessId
spawnLink child =
  executeAndResumeOrThrow (SpawnLink @q (provideInterruptsShutdown child))

-- | Start a new process, the new process will execute an effect, the function
-- will return immediately with a 'ProcessId'. The spawned process has only the
-- /raw/ 'ConsProcess' effects. For non-library code 'spawn' might be better
-- suited.
spawnRaw
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => Eff (ConsProcess q) ()
  -> Eff r ProcessId
spawnRaw child = executeAndResumeOrThrow (Spawn @q child)

-- | Like 'spawnRaw' but return @()@.
spawnRaw_
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => Eff (ConsProcess q) ()
  -> Eff r ()
spawnRaw_ = void . spawnRaw

-- | Return 'True' if the process is alive.
isProcessAlive
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => SchedulerProxy q
  -> ProcessId
  -> Eff r Bool
isProcessAlive _px pid =
  isJust <$> executeAndResumeOrThrow (GetProcessState pid)

-- | Block until a message was received.
-- See 'ReceiveMessage' for more documentation.
receiveAnyMessage
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => SchedulerProxy q
  -> Eff r Dynamic
receiveAnyMessage _ =
  executeAndResumeOrThrow (ReceiveSelectedMessage selectAnyMessageLazy)

-- | Block until a message was received, that is not 'Nothing' after applying
-- a callback to it.
-- See 'ReceiveSelectedMessage' for more documentation.
receiveSelectedMessage
  :: forall r q a
   . ( HasCallStack
     , Show a
     , SetMember Process (Process q) r
     , Member Interrupts r
     )
  => SchedulerProxy q
  -> MessageSelector a
  -> Eff r a
receiveSelectedMessage _ f = executeAndResumeOrThrow (ReceiveSelectedMessage f)

-- | Receive and cast the message to some 'Typeable' instance.
-- See 'ReceiveSelectedMessage' for more documentation.
-- This will wait for a message of the return type using 'receiveSelectedMessage'
receiveMessage
  :: forall a r q
   . ( HasCallStack
     , Typeable a
     , Show a
     , SetMember Process (Process q) r
     , Member Interrupts r
     )
  => SchedulerProxy q
  -> Eff r a
receiveMessage px = receiveSelectedMessage px (MessageSelector fromDynamic)

-- | Remove and return all messages currently enqueued in the process message
-- queue.
--
-- @since 0.12.0
flushMessages
  :: forall  r q
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Member Interrupts r
     , HasScheduler q
     )
  => Eff r [Dynamic]
flushMessages =
  executeAndResumeOrThrow @q FlushMessages

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
  -> (Either InterruptReason a -> Eff r (Maybe endOfLoopResult))
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
  -> (Either InterruptReason Dynamic -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveAnyLoop px = receiveSelectedLoop px selectAnyMessageLazy

-- | Like 'receiveSelectedLoop' but refined to casting to a specific 'Typeable'
-- using 'selectMessageLazy'.
receiveLoop
  :: forall r q a endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack, Typeable a)
  => SchedulerProxy q
  -> (Either InterruptReason a -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveLoop px = receiveSelectedLoop px selectMessageLazy

-- | Returns the 'ProcessId' of the current process.
self
  :: (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r ProcessId
self _px = executeAndResumeOrExit SelfPid

-- | Generate a unique 'Int' for the current process.
makeReference
  :: (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => SchedulerProxy q
  -> Eff r Int
makeReference _px = executeAndResumeOrThrow MakeReference

-- | A value that contains a unique reference of a process
-- monitoring.
data MonitorReference =
  MonitorReference { monitorIndex :: Int
                   , monitoredProcess :: ProcessId
                   }
  deriving (Read, Eq, Ord, Generic, Typeable)

instance NFData MonitorReference

instance Show MonitorReference where
  showsPrec d m =
    showParen (d>=10)
      ( showString "monitor: "
      . shows (monitorIndex m)
      . showChar ' '
      . shows (monitoredProcess m))

-- | Monitor another process. When the monitored process exits a
--  'ProcessDown' is sent to the calling process.
-- The return value is a unique identifier for that monitor.
-- There can be multiple monitors on the same process,
-- and a message for each will be sent.
-- If the process is already dead, the 'ProcessDown' message
-- will be sent immediately, w.thout exit reason
monitor
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => SchedulerProxy q
  -> ProcessId
  -> Eff r MonitorReference
monitor _px = executeAndResumeOrThrow . Monitor . force

-- | Remove a monitor created with 'monitor'.
demonitor
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => SchedulerProxy q
  -> MonitorReference
  -> Eff r ()
demonitor _px = executeAndResumeOrThrow . Demonitor . force

-- | 'monitor' another process before while performing an action
-- and 'demonitor' afterwards.
withMonitor
  :: ( HasCallStack
     , Member Interrupts r
     , SetMember Process (Process q) r
     , Member Interrupts r
     )
  => SchedulerProxy q
  -> ProcessId
  -> (MonitorReference -> Eff r a)
  -> Eff r a
withMonitor px pid eff = monitor px pid >>= \ref -> eff ref <* demonitor px ref

-- | A 'MessageSelector' for receiving either a monitor of the
-- given process or another message.
receiveWithMonitor
  :: ( HasCallStack
     , Member Interrupts r
     , SetMember Process (Process q) r
     , Member Interrupts r
     , Typeable a
     , Show a
     )
  => SchedulerProxy q
  -> ProcessId
  -> MessageSelector a
  -> Eff r (Either ProcessDown a)
receiveWithMonitor px pid sel = withMonitor
  px
  pid
  (\ref -> receiveSelectedMessage
    px
    (Left <$> selectProcessDown ref <|> Right <$> sel)
  )

-- | A monitored process exited.
-- This message is sent to a process by the scheduler, when
-- a process that was monitored via a 'SchedulerCommand' died.
data ProcessDown =
  ProcessDown
    { downReference :: !MonitorReference
    , downReason    :: !SomeExitReason
    }
  deriving (Typeable, Generic, Eq, Ord)

-- | Trigger an 'Interrupt' for a 'ProcessDown' message.
-- The reason will be 'ProcessNotRunning'
becauseProcessIsDown :: ProcessDown -> InterruptReason
becauseProcessIsDown = ProcessNotRunning . monitoredProcess . downReference

instance NFData ProcessDown

instance Show ProcessDown where
  showsPrec d =
    showParen
      (d>=10)
    . (\case
          ProcessDown ref reason ->
            showString "monitored process down "
             . showsPrec 11 ref . showChar ' '
             . showsPrec 11 reason
      )

-- | A 'MesssageSelector' for the 'ProcessDown' message of a specific
-- process.
selectProcessDown :: MonitorReference -> MessageSelector ProcessDown
selectProcessDown ref0 =
  filterMessageLazy (\(ProcessDown ref _reason) -> ref0 == ref)

-- | Connect the calling process to another process, such that
-- if one of the processes crashes (i.e. 'isCrash' returns 'True'), the other
-- is shutdown with the 'ProcessExitReaon' 'LinkedProcessCrashed'.
linkProcess
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => SchedulerProxy q
  -> ProcessId
  -> Eff r ()
linkProcess _px = executeAndResumeOrThrow . Link . force

-- | Unlink the calling proccess from the other process.
unlinkProcess
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => SchedulerProxy q
  -> ProcessId
  -> Eff r ()
unlinkProcess _px = executeAndResumeOrThrow . Unlink . force

-- | Exit the process with a 'ProcessExitReaon'.
exitBecause
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ExitReason 'NoRecovery
  -> Eff r a
exitBecause _ = send . Shutdown @q . force

-- | Exit the process.
exitNormally
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r a
exitNormally px = exitBecause px ExitNormally

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
