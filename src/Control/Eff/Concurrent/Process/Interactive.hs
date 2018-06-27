module Control.Eff.Concurrent.Process.Interactive
where

import           Control.Arrow
import           Control.Concurrent
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Client
import           Control.Eff.Concurrent.Process
import           Control.Monad
import           Data.Typeable                  ( Typeable )


-- | This module provides support for executing 'Process' actions from 'IO'.
--
-- One use case is interacting with processes from the REPL, e.g.:
--
-- >>> import Control.Eff.Concurrent.Process.SingleThreadedScheduler (defaultMain)
--
-- >>> import Data.Dynamic
--
-- >>> import Data.Maybe
--
-- >>> s <- forkInteractiveScheduler Control.Eff.Concurrent.Process.SingleThreadedScheduler.defaultMain
--
-- >>> fooPid <- submit s (spawn (forever (receiveMessage SP >>= (logMsg . fromMaybe "Huh!??" . fromDynamic))))
--
-- >>> fooPid
-- <0.1.0>
--
-- >>> submit s (sendMessageAs SP fooPid "test")
-- test
--
-- >>> submit s (sendShutdown SP fooPid)
--
--
--
-- @since 0.3.0.0

data SchedulerVar r =
  SchedulerVar { _schedulerThreadId :: ThreadId
               , _schedulerInQueue :: MVar (Eff (Process r ': r) (Maybe String))
               }


-- | Fork a scheduler with a process that communicates with it via 'MVar',
-- which is also the reason for the @Lift IO@ constraint.
forkInteractiveScheduler
  :: forall r
   . (SetMember Lift (Lift IO) r)
  => (Eff (Process r ': r) () -> IO ())
  -> IO (SchedulerVar r)
forkInteractiveScheduler ioScheduler = do
  inQueue <- newEmptyMVar
  tid     <- forkIO (ioScheduler (readEvalPrintLoop inQueue))
  return (SchedulerVar tid inQueue)
 where
  readEvalPrintLoop = forever . (readAction >>> evalAction >=> printResult)
   where
    readAction v = do
      mr <- lift (tryTakeMVar v)
      case mr of
        Nothing -> do
          yieldProcess SP
          readAction v
        Just r -> return r
    evalAction  = join
    printResult = mapM_ (lift . putStrLn)

-- | Exit the schedulder immediately using an asynchronous exception.
killInteractiveScheduler :: SchedulerVar r -> IO ()
killInteractiveScheduler = killThread . _schedulerThreadId

-- | Send a 'Process' effect to the main process of a scheduler, this blocks
-- until the effect is executed.
submit
  :: forall r a
   . (SetMember Lift (Lift IO) r)
  => SchedulerVar r
  -> Eff (Process r ': r) a
  -> IO a
submit r theAction = do
  resVar <- newEmptyMVar
  worked <- tryPutMVar (_schedulerInQueue r) (runAndPutResult resVar)
  if worked then takeMVar resVar else fail "ERROR: Scheduler still busy"
 where
  runAndPutResult resVar = do
    res <- theAction
    lift (putMVar resVar $! res)
    return Nothing

-- | Send a 'Process' effect to the main process of a scheduler, this blocks
-- until the effect is executed, then the result is printed by the thread,
-- that runs the process 0 in the scheduler.
submitPrint
  :: forall r a
   . (Show a, SetMember Lift (Lift IO) r)
  => SchedulerVar r
  -> Eff (Process r ': r) a
  -> IO ()
submitPrint r theAction = do
  worked <- tryPutMVar (_schedulerInQueue r) runAndShowResult
  if worked then return () else fail "ERROR: Scheduler still busy"
 where
  runAndShowResult = do
    res <- theAction
    return (Just $! (show res))

-- | Combination of 'submit' and 'cast'.
submitCast
  :: forall o r
   . (SetMember Lift (Lift IO) r, Typeable o)
  => SchedulerVar r
  -> Server o
  -> Api o 'Asynchronous
  -> IO ()
submitCast sc svr request = submit sc (cast SchedulerProxy svr request)

-- | Combination of 'submit' and 'cast'.
submitCall
  :: forall o q r
   . (SetMember Lift (Lift IO) r, Typeable o, Typeable q)
  => SchedulerVar r
  -> Server o
  -> Api o ( 'Synchronous q)
  -> IO q
submitCall sc svr request = submit sc (call SchedulerProxy svr request)
