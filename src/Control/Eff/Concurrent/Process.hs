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
  , MessageSelector(..)
  , ProcessState(..)
  , ProcessExitReason(..)
  , ShutdownRequest(..)
  , thisSchedulerProxy
  , executeAndCatch
  , executeAndResume
  , yieldProcess
  , sendMessage
  , sendMessageAs
  , sendMessageChecked
  , spawn
  , spawn_
  , receiveMessage
  , receiveMessageAs
  , receiveMessageSuchThat
  , receiveLoop
  , receiveLoopAs
  , receiveLoopSuchThat
  , self
  , sendShutdown
  , sendShutdownChecked
  , makeReference
  , exitWithError
  , exitNormally
  , raiseError
  , catchRaisedError
  , ignoreProcessError
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
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Dynamic
import           Data.Kind
import           GHC.Stack
import           Text.Printf
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
  Shutdown :: ShutdownRequest -> Process r a
  -- | Raise an error, that can be handled.
  RaiseError :: String -> Process r b
  -- | Request that another a process exits. The targeted process is interrupted
  -- and gets a 'ShutdownRequested', the target process may decide to ignore the shutdown
  -- requests.
  SendShutdown :: ProcessId -> ShutdownRequest -> Process r (ResumeProcess Bool)
  --  LinkProcesses :: ProcessId -> ProcessId -> Process ()
  -- | Send a message to a process addressed by the 'ProcessId'. Sending a
  -- message should **always succeed** and return **immediately**, even if the
  -- destination process does not exist, or does not accept messages of the
  -- given type.
  SendMessage :: ProcessId -> Dynamic -> Process r (ResumeProcess Bool)
  -- | Receive a message. This should block until an a message was received. The
  -- message is returned as a 'ProcessMessage' value. The function should also
  -- return if an exception was caught or a shutdown was requested.
  ReceiveMessage :: Process r (ResumeProcess Dynamic)
  -- | Wait for the next message that matches a criterium. Similar to 'ReceiveMessage'.
  ReceiveMessageSuchThat :: MessageSelector a -> Process r (ResumeProcess a)
  -- | Generate a unique 'Int' for the current process.
  MakeReference :: Process r (ResumeProcess Int)

instance Show (Process r b) where
  showsPrec d = \case
    YieldProcess -> showString "YieldProcess"
    SelfPid      -> showString "SelfPid"
    Spawn    _   -> showString "Spawn"
    Shutdown sr  ->
      showParen (d >= 10) (showString "Shutdown " . showsPrec 10 sr)
    RaiseError sr ->
      showParen (d >= 10) (showString "RaiseError " . showsPrec 10 sr)
    SendShutdown toPid sr -> showParen
      (d >= 10)
      ( showString "SendShutdown "
      . showsPrec 10 toPid
      . showChar ' '
      . showsPrec 10 sr
      )
    SendMessage toPid sr -> showParen
      (d >= 10)
      ( showString "SendMessage "
      . showsPrec 10 toPid
      . showChar ' '
      . showsPrec 10 sr
      )
    ReceiveMessage           -> showString "ReceiveMessage"
    ReceiveMessageSuchThat _ -> showString "ReceiveMessageSuchThat"
    MakeReference            -> showString "MakeReference"

-- | Every 'Process' action returns it's actual result wrapped in this type. It
-- will allow to signal errors as well as pass on normal results such as
-- incoming messages.
data ResumeProcess v where
  -- | The process received a 'ShutdownRequest'.
  ShutdownRequested :: ShutdownRequest -> ResumeProcess v
  -- | The process is required to exit from an error condition, that cannot be
  -- recovered from.
  OnError :: String -> ResumeProcess v
  -- | The process may resume to do work, using the given result.
  ResumeWith :: a -> ResumeProcess a
  -- | This indicates that the action did not complete, and maybe retried
  RetryLastAction :: ResumeProcess v
  deriving ( Typeable, Foldable, Functor, Show, Eq, Ord
           , Traversable, Generic, Generic1)

instance NFData a => NFData (ResumeProcess a)

instance NFData1 ResumeProcess

-- | A function that deciced if the next message will be received by
-- 'ReceiveMessageSuchThat'. It conveniently is an instance of 'Monoid'
-- with first come, first serve bais.
newtype MessageSelector a =
  MessageSelector {runMessageSelector :: Dynamic -> Maybe a }
  deriving (Semigroup, Monoid, Functor)

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
  | ProcessBusyReceiving       -- ^ The process blocked by a 'receiveMessage'
  | ProcessShuttingDown        -- ^ The process was shutdown or crashed
  deriving (Read, Show, Ord, Eq, Enum, Generic)

instance NFData ProcessState

instance Default ProcessState where def = ProcessBusy

-- | A sum-type with reasons for why a process exists the scheduling loop,
-- this includes errors, that can occur when scheduleing messages.
data ProcessExitReason =
    ProcessNotFound ProcessId -- TODO remove
    -- ^ No process info was found for a 'ProcessId' during internal
    -- processing. NOTE: This is **ONLY** caused by internal errors, probably by
    -- an incorrect 'MessagePassing' handler in this module. **Sending a message
    -- to a process ALWAYS succeeds!** Even if the process does not exist.
  | ProcessRaisedError String
    -- ^ A process called 'raiseError'.
  | ProcessCaughtIOException String String
    -- ^ A process called 'exitWithError'.
  | ProcessShutDown ShutdownRequest
    -- ^ A process exits.
  | ProcessReturned
    -- ^ A process function returned.
  | SchedulerShuttingDown
    -- ^ An action was not performed while the scheduler was exiting.
  deriving (Typeable, Show, Generic)

instance NFData ProcessExitReason

instance Semigroup ProcessExitReason where
   SchedulerShuttingDown <> _ = SchedulerShuttingDown
   _ <> SchedulerShuttingDown = SchedulerShuttingDown
   ProcessReturned <> x = x
   x <> ProcessReturned = x
   ProcessShutDown ExitNormally <> x = x
   x <> ProcessShutDown ExitNormally = x
   (ProcessRaisedError e1) <> (ProcessRaisedError e2) =
    ProcessRaisedError (e1 ++ " and " ++ e2 )
   e1 <> e2 =
    ProcessShutDown (ExitWithError (show e1 ++ " and " ++ show e2 ))

instance Monoid ProcessExitReason where
  mempty = ProcessShutDown ExitNormally
  mappend = (<>)

instance Exc.Exception ProcessExitReason

-- | When a process sends a process shutdown request to another process,
-- it can specify the reason for the shutdown using this data type.
data ShutdownRequest =
    ExitNormally
  | ExitWithError String
  deriving (Typeable, Show, Generic, Eq, Ord)

instance NFData ShutdownRequest

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
    ResumeWith !value   -> return value
    RetryLastAction     -> executeAndResume processAction
    ShutdownRequested r -> send (Shutdown @q r)
    OnError           e -> send (Shutdown @q (ExitWithError e))

-- | Execute a and action and resume the process, retry the action, shutdown the process or return an error.
executeAndCatch
  :: forall q r v
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> Eff r (ResumeProcess v)
  -> Eff r (Either String v)
executeAndCatch px processAction = do
  result <- processAction
  case result of
    ResumeWith !value   -> return (Right value)
    RetryLastAction     -> executeAndCatch px processAction
    ShutdownRequested r -> send (Shutdown @q r)
    OnError           e -> return (Left e)

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
  -> ShutdownRequest
  -> Eff r ()
sendShutdown px pid s = void (sendShutdownChecked px pid s)

-- | Like 'sendShutdown', but also return @True@ iff the process to exit exists.
sendShutdownChecked
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> ShutdownRequest
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
receiveMessage
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r Dynamic
receiveMessage _ = executeAndResume ReceiveMessage

-- | Block until a message was received, that is not 'Nothing' after applying
-- a callback to it.
-- See 'ReceiveMessageSuchThat' for more documentation.
receiveMessageSuchThat
  :: forall r q a
   . (HasCallStack, Typeable a, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> MessageSelector a
  -> Eff r a
receiveMessageSuchThat _ f = executeAndResume (ReceiveMessageSuchThat f)

-- | Receive and cast the message to some 'Typeable' instance.
-- See 'ReceiveMessageSuchThat' for more documentation.
-- This will wait for a message of the return type using 'receiveMessageSuchThat'
receiveMessageAs
  :: forall a r q
   . (HasCallStack, Typeable a, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r a
receiveMessageAs px = receiveMessageSuchThat px (MessageSelector fromDynamic)

-- | Enter a loop to receive messages and pass them to a callback, until the
-- function returns 'Just' a result.
-- See 'receiveMesage' or 'ReceiveMessage' for more documentation.
receiveLoop
  :: forall r q endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> (Either (Maybe String) Dynamic -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveLoop px handlers = do
  mReq <- send (ReceiveMessage @q)
  mRes <- case mReq of
    RetryLastAction                          -> return Nothing
    ShutdownRequested ExitNormally           -> handlers (Left Nothing)
    ShutdownRequested (ExitWithError reason) -> handlers (Left (Just reason))
    OnError           reason                 -> handlers (Left (Just reason))
    ResumeWith        message                -> handlers (Right message)
  maybe (receiveLoop px handlers) return mRes

-- | Run receive message of a certain type until the handler
-- function returns 'Just' a result.
-- Like 'receiveLoopSuchThat' applied to 'fromDynamic'
receiveLoopAs
  :: forall r q a endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack, Typeable a)
  => SchedulerProxy q
  -> (Either (Maybe String) a -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveLoopAs px handlers = do
  mReq <- send (ReceiveMessageSuchThat @a @q (MessageSelector fromDynamic))
  mRes <- case mReq of
    RetryLastAction                          -> return Nothing
    ShutdownRequested ExitNormally           -> handlers (Left Nothing)
    ShutdownRequested (ExitWithError reason) -> handlers (Left (Just reason))
    OnError           reason                 -> handlers (Left (Just reason))
    ResumeWith        message                -> handlers (Right message)
  maybe (receiveLoopAs px handlers) return mRes


-- | Like 'receiveLoop' but /selective/: Only the messages of the given type will be
-- received.
receiveLoopSuchThat
  :: forall r q a endOfLoopResult
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> MessageSelector a
  -> (Either (Maybe String) a -> Eff r (Maybe endOfLoopResult))
  -> Eff r endOfLoopResult
receiveLoopSuchThat px selectMesage handlers = do
  mReq <- send (ReceiveMessageSuchThat @a @q selectMesage)
  mRes <- case mReq of
    RetryLastAction                          -> return Nothing
    ShutdownRequested ExitNormally           -> handlers (Left Nothing)
    ShutdownRequested (ExitWithError reason) -> handlers (Left (Just reason))
    OnError           reason                 -> handlers (Left (Just reason))
    ResumeWith        message                -> handlers (Right message)
  maybe (receiveLoopSuchThat px selectMesage handlers) return mRes

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
  readsPrec _ ('<':'0':'.':rest1) =
    case reads rest1 of
      [(c, '.':'0':'>':rest2)] -> [(ProcessId c, rest2)]
      _ -> []
  readsPrec _ _ = []

instance Show ProcessId where
  show (ProcessId c) =
    printf "<0.%d.0>" c

makeLenses ''ProcessId

-- | Log the 'ProcessExitReaons'
logProcessExit
  :: ('[Logs LogMessage] <:: e, MonadIO (Eff e))
  => ProcessExitReason
  -> Eff e ()
logProcessExit ex = case ex of
  ProcessReturned                   -> logDebug "returned"
  ProcessShutDown ExitNormally      -> logDebug "shutdown"
  ProcessShutDown (ExitWithError m) -> logError ("exit with error: " ++ show m)
  ProcessCaughtIOException w m ->
    logError ("runtime exception: " ++ m ++ " caught here: " ++ w)
  ProcessRaisedError m -> logError ("unhandled process exception: " ++ show m)
  _                    -> logError ("scheduler error: " ++ show ex)
