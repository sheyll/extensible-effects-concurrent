-- | This module provides support for executing 'Process' actions from 'IO'.
--
-- One use case is interacting with processes from the REPL, e.g.:
--
-- >>> import Control.Eff.Concurrent.Process.SingleThreadedScheduler (defaultMain)
--
-- >>> import Control.Eff.Loop
--
-- >>> import Data.Dynamic
--
-- >>> import Data.Maybe
--
-- >>> s <- forkInteractiveScheduler Control.Eff.Concurrent.Process.SingleThreadedScheduler.defaultMain
--
-- >>> fooPid <- submit s (spawn (foreverCheap (receiveAnyMessage SP >>= (sendLogEvent . fromMaybe "Huh!??" . fromDynamic))))
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
-- @since 0.3.0.1
module Control.Eff.Concurrent.Process.Interactive
  ( SchedulerSession (),
    forkInteractiveScheduler,
    killInteractiveScheduler,
    submit,
    submitCast,
    submitCall,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Client
import Control.Monad
import Data.Foldable
import System.Timeout

-- | Contains the communication channels to interact with a scheduler running in
-- its' own thread.
newtype SchedulerSession r = SchedulerSession (TMVar (SchedulerQueue r))

newtype SchedulerQueue r
  = SchedulerQueue (TChan (Eff (Processes r) (Maybe String)))

-- | Fork a scheduler with a process that communicates with it via 'MVar',
-- which is also the reason for the @Lift IO@ constraint.
forkInteractiveScheduler ::
  forall r.
  (SetMember Lift (Lift IO) r) =>
  (Eff (Processes r) () -> IO ()) ->
  IO (SchedulerSession r)
forkInteractiveScheduler ioScheduler = do
  inQueue <- newTChanIO
  queueVar <- newEmptyTMVarIO
  void $
    forkIO
      ( do
          ioScheduler
            ( do
                lift (atomically (putTMVar queueVar (SchedulerQueue inQueue)))
                readEvalPrintLoop queueVar
            )
          atomically (void (takeTMVar queueVar))
      )
  return (SchedulerSession queueVar)
  where
    readEvalPrintLoop ::
      TMVar (SchedulerQueue r) -> Eff (Processes r) ()
    readEvalPrintLoop queueVar = do
      nextActionOrExit <- readAction
      case nextActionOrExit of
        Left True -> return ()
        Left False -> readEvalPrintLoop queueVar
        Right nextAction -> do
          res <- nextAction
          traverse_ (lift . putStrLn . (">>> " ++)) res
          yieldProcess
          readEvalPrintLoop queueVar
      where
        readAction = lift $
          atomically $ do
            mInQueue <- tryReadTMVar queueVar
            case mInQueue of
              Nothing -> return (Left True)
              Just (SchedulerQueue inQueue) -> do
                mNextAction <- tryReadTChan inQueue
                case mNextAction of
                  Nothing -> return (Left False)
                  Just nextAction -> return (Right nextAction)

-- | Exit the scheduler immediately using an asynchronous exception.
killInteractiveScheduler :: SchedulerSession r -> IO ()
killInteractiveScheduler (SchedulerSession qVar) =
  atomically (void (tryTakeTMVar qVar))

-- | Send a 'Process' effect to the main process of a scheduler, this blocks
-- until the effect is executed.
submit ::
  forall r a.
  (SetMember Lift (Lift IO) r) =>
  SchedulerSession r ->
  Eff (Processes r) a ->
  IO a
submit (SchedulerSession qVar) theAction = do
  mResVar <-
    timeout 5000000 $
      atomically
        ( do
            SchedulerQueue inQueue <- readTMVar qVar
            resVar <- newEmptyTMVar
            writeTChan inQueue (runAndPutResult resVar)
            return resVar
        )
  case mResVar of
    Just resVar -> atomically (takeTMVar resVar)
    Nothing -> fail "ERROR: No Scheduler"
  where
    runAndPutResult resVar = do
      res <- theAction
      lift (atomically (putTMVar resVar $! res))
      return Nothing

-- | Combination of 'submit' and 'cast'.
submitCast ::
  forall o r.
  ( SetMember Lift (Lift IO) r,
    TangiblePdu o 'Asynchronous
  ) =>
  SchedulerSession r ->
  Endpoint o ->
  Pdu o 'Asynchronous ->
  IO ()
submitCast sc svr request = submit sc (cast svr request)

-- | Combination of 'submit' and 'cast'.
submitCall ::
  forall o q r.
  ( SetMember Lift (Lift IO) r,
    TangiblePdu o ('Synchronous q),
    Tangible q
  ) =>
  SchedulerSession r ->
  Endpoint o ->
  Pdu o ('Synchronous q) ->
  IO q
submitCall sc svr request = submit sc (call svr request)
