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
  , ProcessExitReason(..)
  , isCrash
  , toCrashReason
  , thisSchedulerProxy
  , executeAndCatch
  , executeAndResume
  , yieldProcess
  , sendMessage
  , sendMessageAs
  , sendMessageChecked
  , spawn
  , spawn_
  , receiveAnyMessage
  , receiveMessageAs
  , receiveSelectedMessage
  , receiveAnyLoop
  , receiveLoop
  , receiveSelectedLoop
  , self
  , sendShutdown
  , sendShutdownChecked
  , makeReference
  , catchRaisedError
  , raiseError
  , ignoreProcessError
  , exitBecause
  , exitNormally
  , exitWithError
  , logProcessExit
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
  -- | Process exit, this is the same as if the function that was applied to a
  -- spawn function returned.
  Shutdown :: ProcessExitReason -> Process r a
  -- | Raise an error, that can be handled.
  RaiseError :: String -> Process r b
  -- | Request that another a process exits. The targeted process is interrupted
  -- and gets a 'ShutdownRequested', the target process may decide to ignore the shutdown
  -- requests.
  SendShutdown :: ProcessId -> ProcessExitReason -> Process r (ResumeProcess Bool)
  --  LinkProcesses :: ProcessId -> ProcessId -> Process ()
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

instance Show (Process r b) where
  showsPrec d = \case
    YieldProcess -> showString "yield process"
    SelfPid      -> showString "lookup the current process id"
    Spawn    _   -> showString "spawn a new process"
    Shutdown sr  ->
      showParen (d >= 10) (showString "shutdown " . showsPrec 10 sr)
    RaiseError sr ->
      showParen (d >= 10) (showString "raise an error " . showsPrec 10 sr)
    SendShutdown toPid sr -> showParen
      (d >= 10)
      ( showString "send shutdown to "
      . showsPrec 10 toPid
      . showChar ' '
      . showsPrec 10 sr
      )
    SendMessage toPid sr -> showParen
      (d >= 10)
      ( showString "send a message to "
      . showsPrec 10 toPid
      . showChar ' '
      . showsPrec 10 sr
      )
    ReceiveSelectedMessage _ -> showString "receive a message"
    MakeReference            -> showString "generate a unique reference"

-- | Every 'Process' action returns it's actual result wrapped in this type. It
-- will allow to signal errors as well as pass on normal results such as
-- incoming messages.
data ResumeProcess v where
  -- | The process received a 'ProcessExitReason'.
  ShutdownRequested :: ProcessExitReason -> ResumeProcess v
  -- | The process is required to exit from an error condition, that cannot be
  -- recovered from.
  OnError :: String -> ResumeProcess v
  -- | The process may resume to do work, using the given result.
  ResumeWith :: a -> ResumeProcess a
  deriving ( Typeable, Foldable, Functor, Show, Eq, Ord
           , Traversable, Generic, Generic1)

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
    ProcessBooting             -- ^ The process has just been started but not
                               --   called 'handleProcess' yet.
  | ProcessIdle                -- ^ The process yielded it's timeslice
  | ProcessBusy                -- ^ The process is busy with non-blocking
  | ProcessBusySending         -- ^ The process is busy with sending a message
  | ProcessBusySendingShutdown -- ^ The process is busy with killing
  | ProcessBusyReceiving       -- ^ The process blocked by a 'receiveAnyMessage'
  | ProcessShuttingDown        -- ^ The process was shutdown or crashed
  deriving (Read, Show, Ord, Eq, Enum, Generic)

instance NFData ProcessState

instance Default ProcessState where def = ProcessBusy

-- | A sum-type with reasons for why a process exists the scheduling loop,
-- this includes errors, that can occur when scheduleing messages.
data ProcessExitReason =
    ExitNormally
    -- ^ A process function returned or exitted without any error.
  | ExitWithError String
    -- ^ A process called 'raiseError'.
  | LinkedProcessCrashed ProcessId
    -- ^ A linked process is down
  | UnexpectedException String String
    -- ^ An unexpected runtime exception was thrown, i.e. an exception
    -- derived from 'Control.Exception.Safe.SomeException'
  | Killed
    -- ^ A process was cancelled (e.g. killed, in 'Async.cancel')
  deriving (Typeable, Generic, Eq, Ord)

-- | A predicate for crashes. A /crash/ happens when a process exits
-- with an 'ExitReason' other than 'ExitNormally'
isCrash :: ProcessExitReason -> Bool
isCrash ExitNormally = False
isCrash _            = True

-- | A predicate for recoverable exit reasons. This predicate defines the
-- exit reasonson which functions such as 'executeAndCatch'
isCrash :: ProcessExitReason -> Bool
isCrash ExitNormally = False
isCrash _            = True

-- | Print a 'ProcessExitReaon' to 'Just' a formatted 'String' when 'isCrash'
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
toCrashReason :: ProcessExitReason -> Maybe String
toCrashReason e | isCrash e = Just (show e)
                | otherwise = Nothing

instance Show ProcessExitReason where
  showsPrec d =
    showParen (d>=10) .
    (\case
        ExitNormally            -> showString "exit normally"
        ExitWithError reason    -> showString "error: " . showString reason
        LinkedProcessCrashed m  -> showString "linked process crashed: " . shows m
        UnexpectedException w m ->   showString "unhandled runtime exception: "
                                   . showString m
                                   . showString " caught here: "
                                   . showString w
        Killed                  -> showString "killed"
    )

instance NFData ProcessExitReason

instance Semigroup ProcessExitReason where
   ExitNormally <> x = x
   x <> ExitNormally = x
   e1 <> e2 = ExitWithError (show e1 ++ " and " ++ show e2 )

instance Monoid ProcessExitReason where
  mempty = ExitNormally
  mappend = (<>)

instance Exc.Exception ProcessExitReason

-- | Execute a 'Process' action and resume the process, retry the action or exit
-- the process when a shutdown was requested.
executeAndResume
  :: forall r q v
   . (SetMember Process (Process q) r, HasCallStack)
  => Process q (ResumeProcess v)
  -> Eff r v
executeAndResume processAction = do
  result <- send processAction
  case result of
    ResumeWith        !value -> return value
    ShutdownRequested r      -> send (Shutdown @q r)
    OnError           e      -> send (Shutdown @q (ExitWithError e))

-- | Execute a and action and resume the process, retry the action, shutdown the process or return an error.
executeAndCatch
  :: forall q r v
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> Eff r (ResumeProcess v)
  -> Eff r (Either String v)
executeAndCatch _px processAction = do
  result <- processAction
  case result of
    ResumeWith        !value -> return (Right value)
    ShutdownRequested r      -> send (Shutdown @q r)
    OnError           e      -> return (Left e)

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

-- | Exit a process addressed by the 'ProcessId'.
-- See 'SendShutdown'.
sendShutdown
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> ProcessExitReason
  -> Eff r ()
sendShutdown px pid s = void (sendShutdownChecked px pid s)

-- | Like 'sendShutdown', but also return @True@ iff the process to exit exists.
sendShutdownChecked
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> ProcessExitReason
  -> Eff r Bool
sendShutdownChecked _ pid s = executeAndResume (SendShutdown pid s)

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
-- See 'selectAnyMessageLazy' or 'ReceiveSelectedMessage' for more documentation.
receiveAnyLoop
  :: forall r q endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> (Either ProcessExitReason Dynamic -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveAnyLoop px handlers = do
  mReq <- send (ReceiveSelectedMessage @q selectAnyMessageLazy)
  mRes <- case mReq of
    ShutdownRequested reason  -> handlers (Left reason)
    OnError           reason  -> handlers (Left (ExitWithError reason))
    ResumeWith        message -> handlers (Right message)
  maybe (receiveAnyLoop px handlers) return mRes

-- | Run receive message of a certain type until the handler
-- function returns 'Just' a result.
-- Like 'receiveSelectedLoop' applied to 'fromDynamic'
receiveLoop
  :: forall r q a endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack, Typeable a)
  => SchedulerProxy q
  -> (Either ProcessExitReason a -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveLoop px handlers = do
  mReq <- send (ReceiveSelectedMessage @q @a (MessageSelector fromDynamic))
  mRes <- case mReq of
    ShutdownRequested reason  -> handlers (Left reason)
    OnError           reason  -> handlers (Left (ExitWithError reason))
    ResumeWith        message -> handlers (Right message)
  maybe (receiveLoop px handlers) return mRes


-- | Like 'receiveAnyLoop' but /selective/: Only the messages of the given type will be
-- received.
receiveSelectedLoop
  :: forall r q a endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> MessageSelector a
  -> (Either ProcessExitReason a -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveSelectedLoop px selectMesage handlers = do
  mReq <- send (ReceiveSelectedMessage @q @a selectMesage)
  mRes <- case mReq of
    ShutdownRequested reason  -> handlers (Left reason)
    OnError           reason  -> handlers (Left (ExitWithError reason))
    ResumeWith        message -> handlers (Right message)
  maybe (receiveSelectedLoop px selectMesage handlers) return mRes

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

-- | Exit the process with a 'ProcessExitReaon'.
exitBecause
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessExitReason
  -> Eff r a
exitBecause _ = send . Shutdown @q

-- | Exit the process.
exitNormally
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r a
exitNormally _ = send (Shutdown @q ExitNormally)

-- | Exit the process with an error.
exitWithError
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> String
  -> Eff r a
exitWithError _ = send . (Shutdown @q . (ExitWithError $!))

-- | Thrown an error, can be caught by 'catchRaisedError'.
raiseError
  :: forall r q b
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> String
  -> Eff r b
raiseError _ = send . (RaiseError @q $!)

-- | Catch and handle an error raised by 'raiseError'. Works independent of the
-- handler implementation.
catchRaisedError
  :: forall r q w
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> (String -> Eff r w)
  -> Eff r w
  -> Eff r w
catchRaisedError _ onErr = interpose return go
 where
  go :: forall b . Process q b -> (b -> Eff r w) -> Eff r w
  go (RaiseError emsg) _k = onErr emsg
  go s                 k  = send s >>= k

-- | Like 'catchRaisedError' it catches 'raiseError', but instead of invoking a
-- user provided handler, the result is wrapped into an 'Either'.
ignoreProcessError
  :: (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r a
  -> Eff r (Either String a)
ignoreProcessError px = catchRaisedError px (return . Left) . fmap Right

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

-- | Log the 'ProcessExitReaons'
logProcessExit
  :: (HasCallStack, HasLogWriter LogMessage h e)
  => ProcessExitReason
  -> Eff e ()
logProcessExit (toCrashReason -> Just ex) = withFrozenCallStack (logError ex)
logProcessExit ex = withFrozenCallStack (logInfo (show ex))

-- | Scheduler commands.
data SchedulerCommand b where
  -- | Monitor another process. When the monitored process exits a
  --  'MonitoredProcessDown' is sent to the calling process.
  -- The return value is a unique identifier for that monitor.
  -- There can be multiple monitors on the same process,
  -- and a message for each will be sent.
  -- If the process is already dead, the 'MonitoredProcessDown' message
  -- will be sent immediately, w.thout exit reason
  Monitor :: ProcessId -> SchedulerCommand Int
  -- | Remove a monitor.
  Demonitor :: Int -> SchedulerCommand Int
  -- | Connect the calling process to another process, such that
  -- if one of the processes crashes (i.e. 'isCrash' returns 'True'), the other
  -- is shutdown with the 'ProcessExitReaon' 'LinkedProcessCrashed'.
  Link :: ProcessId -> SchedulerCommand ()
  -- | Unlink the calling proccess from the other process.
  Unlink :: ProcessId -> SchedulerCommand ()
  deriving Typeable

instance Show (SchedulerCommand v) where
  showsPrec d =
    showParen
      (d>=10)
    . (\case
          Monitor pid -> showString "monitor " . shows pid
          Demonitor i -> showString "demonitor " . shows i
          Link l -> showString "link " . shows l
          Unlink l -> showString "unlink " . shows l
          CatchUnlink -> showString "catch crashes of linked processes"
          CrashOnUnlink -> showString "crash when a linked process crashes"
      )

deriving instance Eq (SchedulerCommand v)

deriving instance Ord (SchedulerCommand v)

-- * Scheduler Events

-- | A monitored process exitted.
-- This message is sent to a process by the scheduler, when
-- a process that was monitored via a 'SchedulerCommand' died.
data MonitoredProcessDown = MonitoredProcessDown ProcessId Int ProcessExitReason
  deriving (Typeable, Generic, Eq, Ord)

instance NFData MonitoredProcessDown

instance Show MonitoredProcessDown where
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
data CaughtLinkedProcessCrashed =
  CaughtLinkedProcessCrashed ProcessId ProcessExitReason
  deriving (Typeable, Generic, Eq, Ord)

instance NFData CaughtLinkedProcessCrashed

instance Show CaughtLinkedProcessCrashed where
  showsPrec d =
    showParen
      (d>=10)
    . (\case
          CaughtLinkedProcessCrashed pid reason ->
            showString "caught unlink from "
             . showsPrec 11 pid . showChar ' '
             . showsPrec 11 reason
      )
