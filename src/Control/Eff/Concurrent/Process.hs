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
  , -- ** Message Data
    StrictDynamic()
  , toStrictDynamic
  , fromStrictDynamic
  , unwrapStrictDynamic
    -- ** ProcessId Type
  , ProcessId(..)
  , fromProcessId
  , ConsProcess
  , ResumeProcess(..)
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
  -- ** Process Life Cycle Management
  , self
  , isProcessAlive
  -- ** Spawning
  , spawn
  , spawn_
  , spawnLink
  , spawnRaw
  , spawnRaw_
  -- ** Process Exit or Interrupt Recoverable
  , exitBecause
  , exitNormally
  , exitWithError
  -- ** Links
  , linkProcess
  , unlinkProcess
  -- ** Monitors
  , monitor
  , demonitor
  , ProcessDown(..)
  , selectProcessDown
  , becauseProcessIsDown
  , MonitorReference(..)
  , withMonitor
  , receiveWithMonitor
  -- ** Process Interrupt Recoverable Handling
  , provideInterruptsShutdown
  , handleInterrupts
  , tryUninterrupted
  , exitOnInterrupt
  , logInterrupts
  , provideInterrupts
  , mergeEitherInterruptAndExitReason
  , interrupt
  -- ** Process Operation Execution
  , executeAndResume
  , executeAndResumeOrExit
  , executeAndResumeOrThrow
  -- ** Exit Or Interrupt Recoverable Reasons
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
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Extend
import           Control.Eff.Log.Handler
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
import           Data.String                    (fromString)
import           Data.Text                     (Text, pack, unpack)
import qualified Data.Text                     as T
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
  FlushMessages :: Process r (ResumeProcess [StrictDynamic])
  -- | In cooperative schedulers, this will give processing time to the
  -- scheduler. Every other operation implicitly serves the same purpose.
  --
  -- @since 0.12.0
  YieldProcess :: Process r (ResumeProcess ())
  -- | Return the current 'ProcessId'
  SelfPid :: Process r (ResumeProcess ProcessId)
  -- | Start a new process, the new process will execute an effect, the function
  -- will return immediately with a 'ProcessId'.
  Spawn :: Eff (Process r ': r) () -> Process r (ResumeProcess ProcessId)
  -- | Start a new process, and 'Link' to it .
  --
  -- @since 0.12.0
  SpawnLink :: Eff (Process r ': r) () -> Process r (ResumeProcess ProcessId)
  -- | Get the process state (or 'Nothing' if the process is dead)
  GetProcessState :: ProcessId -> Process r (ResumeProcess (Maybe ProcessState))
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
  -- @since 0.12.0
  Link :: ProcessId -> Process r (ResumeProcess ())
  -- | Unlink the calling process from the other process.
  --
  -- @since 0.12.0
  Unlink :: ProcessId -> Process r (ResumeProcess ())

instance Show (Process r b) where
  showsPrec d = \case
    FlushMessages -> showString "flush messages"
    YieldProcess  -> showString "yield process"
    SelfPid       -> showString "lookup the current process id"
    Spawn     _   -> showString "spawn a new process"
    SpawnLink _   -> showString "spawn a new process and link to it"
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
    GetProcessState pid -> showString "get the process state of " . shows pid
    MakeReference              -> showString "generate a unique reference"
    Monitor   pid              -> showString "monitor " . shows pid
    Demonitor i                -> showString "demonitor " . shows i
    Link      l                -> showString "link " . shows l
    Unlink    l                -> showString "unlink " . shows l

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

instance Show StrictDynamic where
  show (MkDynamicMessage d) = show d

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

instance Default ProcessState where
  def = ProcessBooting

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
toExitRecovery = \case
  NormalExitRequested           -> Recoverable
  (OtherProcessNotRunning    _)  -> Recoverable
  (TimeoutInterrupt       _)  -> Recoverable
  (LinkedProcessCrashed _)  -> Recoverable
  (ErrorInterrupt         _)  -> Recoverable
  ExitNormally              -> NoRecovery
  (ExitUnhandledError _) -> NoRecovery
  ExitProcessCancelled                    -> NoRecovery

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
    -- | A process that should be running was not running.
    OtherProcessNotRunning
      :: ProcessId -> Interrupt 'Recoverable
    -- | A 'Recoverable' timeout has occurred.
    TimeoutInterrupt
      :: String -> Interrupt 'Recoverable
    -- | A linked process is down
    LinkedProcessCrashed
      :: ProcessId -> Interrupt 'Recoverable
    -- | An exit reason that has an error message and is 'Recoverable'.
    ErrorInterrupt
      :: String -> Interrupt 'Recoverable
    -- | A process function returned or exited without any error.
    ExitNormally
      :: Interrupt 'NoRecovery
    -- | An error causes the process to exit immediately.
    -- For example an unexpected runtime exception was thrown, i.e. an exception
    -- derived from 'Control.Exception.Safe.SomeException'
    -- Or a 'Recoverable' Interrupt was not recoverd.
    ExitUnhandledError
      :: Text -> Interrupt 'NoRecovery
    -- | A process shall exit immediately, without any cleanup was cancelled (e.g. killed, in 'Async.cancel')
    ExitProcessCancelled
      :: Interrupt 'NoRecovery
  deriving Typeable

-- | Return either 'ExitNormally' or 'interruptToExit' from a 'Recoverable' 'Interrupt';
--
-- If the 'Interrupt' is 'NormalExitRequested' then return 'ExitNormally'
interruptToExit :: Interrupt 'Recoverable -> Interrupt 'NoRecovery
interruptToExit NormalExitRequested = ExitNormally
interruptToExit x = ExitUnhandledError (pack (show x))

instance Show (Interrupt x) where
  showsPrec d =
    showParen (d >= 9)
      . (\case
          NormalExitRequested        -> showString "interrupt: A normal exit was requested"
          OtherProcessNotRunning p    -> showString "interrupt: Another process is not running: " . showsPrec 10 p
          TimeoutInterrupt reason -> showString "interrupt: A timeout occured: " . showString reason
          LinkedProcessCrashed m ->
            showString "interrupt: A linked process " . showsPrec 10 m . showString " crashed"
          ErrorInterrupt reason   -> showString "interrupt: An error occured: " . showString reason
          ExitNormally          -> showString "exit: Process finished successfully"
          ExitUnhandledError w ->
            showString "exit: Unhandled " . showString (unpack w)
          ExitProcessCancelled -> showString "exit: The process was cancelled"
        )

instance Exc.Exception (Interrupt 'Recoverable)
instance Exc.Exception (Interrupt 'NoRecovery )

instance NFData (Interrupt x) where
  rnf NormalExitRequested               = rnf ()
  rnf (OtherProcessNotRunning    !l)     = rnf l
  rnf (TimeoutInterrupt       !l)     = rnf l
  rnf (LinkedProcessCrashed !l)     = rnf l
  rnf (ErrorInterrupt         !l)     = rnf l
  rnf ExitNormally                  = rnf ()
  rnf (ExitUnhandledError !l) = rnf l
  rnf ExitProcessCancelled                        = rnf ()

instance Ord (Interrupt x) where
  compare NormalExitRequested          NormalExitRequested         = EQ
  compare NormalExitRequested          _                           = LT
  compare _                        NormalExitRequested             = GT
  compare (OtherProcessNotRunning l)    (OtherProcessNotRunning r) = compare l r
  compare (OtherProcessNotRunning _)    _                          = LT
  compare _                        (OtherProcessNotRunning    _)   = GT
  compare (TimeoutInterrupt l)       (TimeoutInterrupt r)          = compare l r
  compare (TimeoutInterrupt _) _                                   = LT
  compare _                        (TimeoutInterrupt _)            = GT
  compare (LinkedProcessCrashed l) (LinkedProcessCrashed r)        = compare l r
  compare (LinkedProcessCrashed _) _                               = LT
  compare _                        (LinkedProcessCrashed _)        = GT
  compare (ErrorInterrupt l)         (ErrorInterrupt         r)    = compare l r
  compare ExitNormally             ExitNormally                    = EQ
  compare ExitNormally             _                               = LT
  compare _                        ExitNormally                    = GT
  compare (ExitUnhandledError l) (ExitUnhandledError r)            = compare l r
  compare (ExitUnhandledError _ ) _                                = LT
  compare _                         (ExitUnhandledError _)         = GT
  compare ExitProcessCancelled  ExitProcessCancelled               = EQ

instance Eq (Interrupt x) where
  (==) NormalExitRequested          NormalExitRequested          = True
  (==) (OtherProcessNotRunning l)    (OtherProcessNotRunning r)    = (==) l r
  (==) ExitNormally             ExitNormally             = True
  (==) (TimeoutInterrupt l)       (TimeoutInterrupt r)       = l == r
  (==) (LinkedProcessCrashed l) (LinkedProcessCrashed r) = l == r
  (==) (ErrorInterrupt         l) (ErrorInterrupt         r) = (==) l r
  (==) (ExitUnhandledError l) (ExitUnhandledError r) = (==) l r
  (==) ExitProcessCancelled ExitProcessCancelled = True
  (==) _      _      = False

-- | A predicate for linked process __crashes__.
isProcessDownInterrupt :: Maybe ProcessId -> Interrupt r -> Bool
isProcessDownInterrupt mOtherProcess = \case
  NormalExitRequested         -> False
  OtherProcessNotRunning    _  -> False
  TimeoutInterrupt       _  -> False
  LinkedProcessCrashed p  -> maybe True (== p) mOtherProcess
  ErrorInterrupt         _  -> False
  ExitNormally            -> False
  ExitUnhandledError _ -> False
  ExitProcessCancelled -> False

-- | 'Interrupt's which are 'Recoverable'.
type RecoverableInterrupt = Interrupt 'Recoverable

-- | /Cons/ 'Process' onto a list of effects.
type ConsProcess r = Process r ': r

-- | 'Exc'eptions containing 'Interrupt's.
-- See 'handleInterrupts', 'exitOnInterrupt' or 'provideInterrupts'
type Interrupts = Exc (Interrupt 'Recoverable)

-- | This adds a layer of the 'Interrupts' effect on top of 'Process'
type InterruptableProcess e = Interrupts ': Process e ': e

-- | Handle all 'Interrupt's of an 'InterruptableProcess' by
-- wrapping them up in 'interruptToExit' and then do a process 'Shutdown'.
provideInterruptsShutdown
  :: forall e a . Eff (InterruptableProcess e) a -> Eff (ConsProcess e) a
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
  :: (HasCallStack, Member Interrupts r, SetMember Process (Process q) r)
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
fromSomeExitReason
  :: SomeExitReason -> Either (Interrupt 'NoRecovery) (Interrupt 'Recoverable)
fromSomeExitReason (SomeExitReason e) = case e of
  recoverable@NormalExitRequested          -> Right recoverable
  recoverable@(OtherProcessNotRunning    _) -> Right recoverable
  recoverable@(TimeoutInterrupt       _) -> Right recoverable
  recoverable@(LinkedProcessCrashed _) -> Right recoverable
  recoverable@(ErrorInterrupt         _) -> Right recoverable
  noRecovery@ExitNormally              -> Left noRecovery
  noRecovery@(ExitUnhandledError _) -> Left noRecovery
  noRecovery@ExitProcessCancelled -> Left noRecovery

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
logProcessExit (toCrashReason -> Just ex) = withFrozenCallStack (logError ex)
logProcessExit ex = withFrozenCallStack (logDebug (fromString (show ex)))


-- | Execute a and action and return the result;
-- if the process is interrupted by an error or exception, or an explicit
-- shutdown from another process, or through a crash of a linked process, i.e.
-- whenever the exit reason satisfies 'isRecoverable', return the exit reason.
executeAndResume
  :: forall q r v
   . (SetMember Process (Process q) r, HasCallStack)
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
   . (SetMember Process (Process q) r, HasCallStack)
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
  => Eff r ()
yieldProcess = executeAndResumeOrThrow YieldProcess

-- | Send a message to a process addressed by the 'ProcessId'.
-- See 'SendMessage'.
--
-- The message will be reduced to normal form ('rnf') by/in the caller process.
sendMessage
  :: forall r q o
   . ( SetMember Process (Process q) r
     , HasCallStack
     , Member Interrupts r
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
   . (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
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
   . (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
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
   . (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
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
--
-- @since 0.12.0
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
--
-- @since 0.12.0
isProcessAlive
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => ProcessId
  -> Eff r Bool
isProcessAlive pid = isJust <$> executeAndResumeOrThrow (GetProcessState pid)

-- | Block until a message was received.
-- See 'ReceiveSelectedMessage' for more documentation.
receiveAnyMessage
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
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
     , SetMember Process (Process q) r
     , Member Interrupts r
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
     , SetMember Process (Process q) r
     , Member Interrupts r
     )
  => Eff r a
receiveMessage = receiveSelectedMessage (MessageSelector fromStrictDynamic)

-- | Remove and return all messages currently enqueued in the process message
-- queue.
--
-- @since 0.12.0
flushMessages
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
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
   . (SetMember Process (Process q) r, HasCallStack)
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
   . (SetMember Process (Process q) r, HasCallStack)
  => (Either (Interrupt 'Recoverable) StrictDynamic -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveAnyLoop = receiveSelectedLoop selectAnyMessage

-- | Like 'receiveSelectedLoop' but refined to casting to a specific 'Typeable'
-- using 'selectMessage'.
receiveLoop
  :: forall r q a endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack, NFData a, Typeable a)
  => (Either (Interrupt 'Recoverable) a -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveLoop = receiveSelectedLoop selectMessage

-- | Returns the 'ProcessId' of the current process.
self :: (HasCallStack, SetMember Process (Process q) r) => Eff r ProcessId
self = executeAndResumeOrExit SelfPid

-- | Generate a unique 'Int' for the current process.
makeReference
  :: (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => Eff r Int
makeReference = executeAndResumeOrThrow MakeReference

-- | A value that contains a unique reference of a process
-- monitoring.
--
-- @since 0.12.0
data MonitorReference =
  MonitorReference { monitorIndex :: Int
                   , monitoredProcess :: ProcessId
                   }
  deriving (Read, Eq, Ord, Generic, Typeable)

instance NFData MonitorReference

instance Show MonitorReference where
  showsPrec d m = showParen
    (d >= 10)
    (showString "monitor: " . shows (monitorIndex m) . showChar ' ' . shows
      (monitoredProcess m)
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
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => ProcessId
  -> Eff r MonitorReference
monitor = executeAndResumeOrThrow . Monitor . force

-- | Remove a monitor created with 'monitor'.
--
-- @since 0.12.0
demonitor
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => MonitorReference
  -> Eff r ()
demonitor = executeAndResumeOrThrow . Demonitor . force

-- | 'monitor' another process before while performing an action
-- and 'demonitor' afterwards.
--
-- @since 0.12.0
withMonitor
  :: ( HasCallStack
     , Member Interrupts r
     , SetMember Process (Process q) r
     , Member Interrupts r
     )
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
     , Member Interrupts r
     , SetMember Process (Process q) r
     , Member Interrupts r
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
    , downReason    :: !SomeExitReason
    }
  deriving (Typeable, Generic, Eq, Ord)

-- | Make an 'Interrupt' for a 'ProcessDown' message.
--
-- For example: @doSomething >>= either (interrupt . becauseProcessIsDown) return@
--
-- @since 0.12.0
becauseProcessIsDown :: ProcessDown -> Interrupt 'Recoverable
becauseProcessIsDown = OtherProcessNotRunning . monitoredProcess . downReference

instance NFData ProcessDown

instance Show ProcessDown where
  showsPrec d =
    showParen (d >= 10)
      . (\case
          ProcessDown ref reason ->
            showString "monitored process down "
              . showsPrec 11 ref
              . showChar ' '
              . showsPrec 11 reason
        )

-- | A 'MessageSelector' for the 'ProcessDown' message of a specific
-- process.
--
-- @since 0.12.0
selectProcessDown :: MonitorReference -> MessageSelector ProcessDown
selectProcessDown ref0 =
  filterMessage (\(ProcessDown ref _reason) -> ref0 == ref)

-- | Connect the calling process to another process, such that
-- if one of the processes crashes (i.e. 'isCrash' returns 'True'), the other
-- is shutdown with the 'Interrupt' 'LinkedProcessCrashed'.
--
-- @since 0.12.0
linkProcess
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => ProcessId
  -> Eff r ()
linkProcess = executeAndResumeOrThrow . Link . force

-- | Unlink the calling process from the other process.
--
-- @since 0.12.0
unlinkProcess
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => ProcessId
  -> Eff r ()
unlinkProcess = executeAndResumeOrThrow . Unlink . force

-- | Exit the process with a 'Interrupt'.
exitBecause
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => Interrupt 'NoRecovery
  -> Eff r a
exitBecause = send . Shutdown @q . force

-- | Exit the process.
exitNormally
  :: forall r q a . (HasCallStack, SetMember Process (Process q) r) => Eff r a
exitNormally = exitBecause ExitNormally

-- | Exit the process with an error.
exitWithError
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => String
  -> Eff r a
exitWithError = exitBecause . interruptToExit . ErrorInterrupt

-- | Each process is identified by a single process id, that stays constant
-- throughout the life cycle of a process. Also, message sending relies on these
-- values to address messages to processes.
newtype ProcessId = ProcessId { _fromProcessId :: Int }
  deriving (Eq,Ord,Typeable,Bounded,Num, Enum, Integral, Real, NFData)

instance Read ProcessId where
  readsPrec _ ('!' : rest1) = case reads rest1 of
    [(c, rest2)] -> [(ProcessId c, rest2)]
    _            -> []
  readsPrec _ _ = []

instance Show ProcessId where
  showsPrec _ (ProcessId !c) = showChar '!' . shows c

makeLenses ''ProcessId
