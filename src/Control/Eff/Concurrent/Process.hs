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
  , receiveLoop
  , self
  , sendShutdown
  , sendShutdownChecked
  , exitWithError
  , exitNormally
  , raiseError
  , catchRaisedError
  , ignoreProcessError
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           GHC.Stack
import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.Extend
import           Control.Lens
import           Control.Monad                  ( void )
import           Data.Dynamic
import           Data.Kind
import           Text.Printf

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
  Shutdown :: Process r a
  -- | Exit the process due to an error, this cannot be caught.
  ExitWithError :: String -> Process  r b
  -- | Raise an error, that can be handled.
  RaiseError :: String -> Process r b
  -- | Request that another a process exits. The targeted process is interrupted
  -- and gets a 'ShutdownRequested', the target process may decide to ignore the shutdown
  -- requests.
  SendShutdown :: ProcessId -> Process r (ResumeProcess Bool)
  --  LinkProcesses :: ProcessId -> ProcessId -> Process ()
  -- | Send a message to a process addressed by the 'ProcessId'. Sending a
  -- message should **always succeed** and return **immediately**, even if the
  -- destination process does not exist, or does not accept messages of the
  -- given type.
  SendMessage :: ProcessId -> Dynamic -> Process r (ResumeProcess Bool)
  -- | Receive a message. This should block until an a message was received. The
  -- pure function may convert the incoming message into something, and the
  -- result is returned as 'ProcessMessage' value. Another reason why this function
  -- returns, is if a process control message was sent to the process. This can
  -- only occur from inside the runtime system, aka the effect handler
  -- implementation. (Currently there is one in 'Control.Eff.Concurrent.Process.ForkIOScheduler'.)
  ReceiveMessage :: Process r (ResumeProcess Dynamic)

-- | Every 'Process' action returns it's actual result wrapped in this type. It
-- will allow to signal errors as well as pass on normal results such as
-- incoming messages.
data ResumeProcess v where
  -- | The process is required to exit.
  ShutdownRequested :: ResumeProcess v
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

-- | /Cons/ 'Process' onto a list of effects.
type ConsProcess r = Process r ': r

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
    ResumeWith !value -> return value
    RetryLastAction   -> executeAndResume processAction
    ShutdownRequested -> send (Shutdown @q)
    OnError e         -> send (ExitWithError @q e)

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
    ResumeWith !value -> return (Right value)
    RetryLastAction   -> executeAndCatch px processAction
    ShutdownRequested -> send (Shutdown @q)
    OnError e         -> return (Left e)

-- | Every function for 'Process' things needs such a proxy value
-- for the low-level effect list, i.e. the effects identified by
-- @__r__@ in @'Process' r : r@, this might be dependent on the
-- scheduler implementation.
data SchedulerProxy :: [Type -> Type] -> Type where
  -- | Tell the typechecker what effects we have below 'Process'
  SchedulerProxy :: SchedulerProxy q
  -- | Like 'SchedulerProxy' but shorter
  SP :: SchedulerProxy q

-- | Return a 'SchedulerProxy' for a 'Process' effect.
thisSchedulerProxy :: Eff (Process r ': r) (SchedulerProxy r)
thisSchedulerProxy = return SchedulerProxy

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
  -> Eff r ()
sendShutdown px pid = void (sendShutdownChecked px pid)

-- | Like 'sendShutdown', but also return @True@ iff the process to exit exists.
sendShutdownChecked
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> ProcessId
  -> Eff r Bool
sendShutdownChecked _ pid = executeAndResume (SendShutdown pid)

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
receiveMessage
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r Dynamic
receiveMessage _ = do
  executeAndResume ReceiveMessage

-- | Receive and cast the message to some 'Typeable' instance.
receiveMessageAs
  :: forall a r q
   . (HasCallStack, Typeable a, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r a
receiveMessageAs px = do
  messageDynamic <- receiveMessage px
  let castAndCheck dm = case fromDynamic dm of
        Nothing -> Left ("Invalid message type received: " ++ show dm)
        Just m  -> Right m
      maybeMessage = castAndCheck messageDynamic
  either (raiseError px) return maybeMessage

-- | Enter a loop to receive messages and pass them to a callback, until the
-- function returns 'False'.
receiveLoop
  :: forall r q
   . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> (Either (Maybe String) Dynamic -> Eff r ())
  -> Eff r ()
receiveLoop px handlers = do
  mReq <- send (ReceiveMessage @q)
  case mReq of
    RetryLastAction    -> receiveLoop px handlers
    ShutdownRequested  -> handlers (Left Nothing) >> receiveLoop px handlers
    OnError reason -> handlers (Left (Just reason)) >> receiveLoop px handlers
    ResumeWith message -> handlers (Right message) >> receiveLoop px handlers

-- | Returns the 'ProcessId' of the current process.
self
  :: (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r ProcessId
self _px = executeAndResume SelfPid

-- | Exit the process.
exitNormally
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r a
exitNormally _ = send (Shutdown @q)

-- | Exit the process with an error.
exitWithError
  :: forall r q a
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> String
  -> Eff r a
exitWithError _ = send . (ExitWithError @q $!)

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
  deriving (Eq,Ord,Typeable,Bounded,Num, Enum, Integral, Real)

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
