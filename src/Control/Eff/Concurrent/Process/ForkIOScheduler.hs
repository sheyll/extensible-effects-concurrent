-- | Implement Erlang style message passing concurrency.
--
-- This handles the 'MessagePassing' and 'Process' effects, using
-- 'STM.TQueue's and 'forkIO'.
--
-- This aims to be a pragmatic implementation, so even logging is
-- supported.
--
-- At the core is a /main process/ that enters 'schedule'
-- and creates all of the internal state stored in 'STM.TVar's
-- to manage processes with message queues.
--
-- The 'Eff' handler for 'Process' and 'MessagePassing' use
-- are implemented and available through 'spawn'.
--
module Control.Eff.Concurrent.Process.ForkIOScheduler
  ( schedule
  , defaultMain
  , defaultMainWithLogChannel
  , ProcEff
  , SchedulerIO
  , HasSchedulerIO
  , forkIoScheduler
  )
where

import           GHC.Stack
import           Data.Kind                      ( )
import           Control.Exception.Safe        as Safe
import qualified Control.Eff.ExceptionExtra    as ExcExtra ()
import           Control.Concurrent.STM        as STM
import           Control.Concurrent             (threadDelay, yield)
import qualified Control.Concurrent.Async      as Async
import           Control.Concurrent.Async      (Async(..))
import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.Concurrent.Process
import           Control.Eff.Lift
import           Control.Eff.Log
import           Control.Eff.Reader.Strict     as Reader
import           Control.Lens
import           Control.Monad                  ( when
                                                , void
                                                , (>=>)
                                                )
import           Control.Monad.Trans.Control     (MonadBaseControl(..))
import           Data.Dynamic
import           Data.Map                       ( Map )
import           Data.Default
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import Control.DeepSeq
import Text.Printf
import Data.Foldable
import Data.Maybe


-- * Process Types

-- | A message queue of a process, contains the actual queue and maybe an
-- exit reason.
data MessageQ = MessageQ { _incomingMessages :: Seq Dynamic
                         , _shutdownRequests :: Maybe ShutdownRequest
                         }

instance Default MessageQ where def = MessageQ def def

makeLenses ''MessageQ

-- | Return any '_shutdownRequests' from a 'MessageQ' in a 'TVar' and
-- reset the '_shutdownRequests' field to 'Nothing' in the 'TVar'.
tryTakeNextShutdownRequestSTM :: TVar MessageQ -> STM (Maybe ShutdownRequest)
tryTakeNextShutdownRequestSTM mqVar = do
  mq <- readTVar mqVar
  when (isJust (mq^.shutdownRequests))
    (writeTVar mqVar (mq & shutdownRequests .~ Nothing))
  return (mq^.shutdownRequests)


-- | Information about a process, needed to implement 'MessagePassing' and
-- 'Process' handlers. The message queue is backed by a 'STM.TQueue' and contains
-- 'MessageQEntry' values.
data ProcessInfo =
                 ProcessInfo { _processId         :: ProcessId
                             , _processState      :: TVar ProcessState
                             , _messageQ          :: TVar MessageQ
                             }

makeLenses ''ProcessInfo

-- * Scheduler Types

-- | Contains all process info'elements, as well as the state needed to
-- implement inter process communication. It contains also a 'LogChannel' to
-- which the logs of all processes are forwarded to.
data SchedulerState =
               SchedulerState { _nextPid :: TVar ProcessId
                              , _processTable :: TVar (Map ProcessId ProcessInfo)
                              , _processCancellationTable :: TVar (Map ProcessId (Async ProcessExitReason))
                              }

makeLenses ''SchedulerState

-- * Process Implementation

instance Show ProcessInfo where
  show p =  "process info: " ++ show (p ^. processId)

-- | Create a new 'ProcessInfo'
newProcessInfo :: HasCallStack => ProcessId -> STM ProcessInfo
newProcessInfo a = ProcessInfo a <$> newTVar ProcessBooting <*> newTVar def

-- * Scheduler Implementation

-- | Create a new 'SchedulerState'
newSchedulerState :: HasCallStack => STM SchedulerState
newSchedulerState = SchedulerState <$> newTVar 1 <*> newTVar def <*> newTVar def

-- | Create a new 'SchedulerState' run an IO action, catching all exceptions,
-- and when the actions returns, clean up and kill all processes.
withNewSchedulerState
  :: (HasLogging IO SchedulerIO, HasCallStack)
  => Eff SchedulerIO ()
  -> Eff LoggingAndIO ()
withNewSchedulerState mainProcessAction =
   Safe.bracketWithError
    (lift (atomically newSchedulerState))
    (\exceptions schedulerState -> do
      traverse_
        (logError . ("scheduler setup crashed " ++) . Safe.displayException)
        exceptions
      logDebug "scheduler cleanup begin"
      runReader schedulerState tearDownScheduler
      )
    (\schedulerState  -> do
      logDebug "scheduler loop entered"
      x <- runReader schedulerState mainProcessAction
      logDebug "scheduler loop returned"
      return x
    )

 where
    tearDownScheduler = do
      schedulerState <- getSchedulerState
      let cancelTableVar = schedulerState ^. processCancellationTable
      -- cancel all processes
      allProcesses <- lift (atomically (readTVar cancelTableVar <* writeTVar cancelTableVar def))
      logNotice ("cancelling processes: " ++ show (toListOf (ifolded.asIndex) allProcesses))
      void
        (liftBaseWith
          (\runS -> Async.race
            (Async.mapConcurrently
              (\a -> do
                Async.cancel a
                runS (logNotice ("process cancelled: "++ show (asyncThreadId a)))
              )
              allProcesses)
            (threadDelay 5000000)))

-- | The concrete list of 'Eff'ects of processes compatible with this scheduler.
-- This builds upon 'SchedulerIO'.
type ProcEff = ConsProcess SchedulerIO


-- | Type class constraint to indicate that an effect union contains the
-- effects required by every process and the scheduler implementation itself.
type HasSchedulerIO r = ( HasCallStack
                        , Lifted IO r
                        , SchedulerIO <:: r
                        )

-- | The concrete list of 'Eff'ects for this scheduler implementation.
type SchedulerIO = ( Reader SchedulerState : LoggingAndIO)

-- | Basic effects: 'Logs' 'LogMessage' and 'Lift' IO
type LoggingAndIO =
              '[ Logs LogMessage
               , LogWriterReader LogMessage IO
               , Lift IO
               ]

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMain :: HasCallStack => Eff ProcEff () -> IO ()
defaultMain c =
  withAsyncLogChannel 1024
    (multiMessageLogWriter ($ printLogMessage))
    (handleLoggingAndIO_ (schedule c))

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMainWithLogChannel
  :: HasCallStack => Eff ProcEff () -> LogChannel LogMessage -> IO ()
defaultMainWithLogChannel = handleLoggingAndIO_ . schedule

-- | A 'SchedulerProxy' for 'SchedulerIO'
forkIoScheduler :: SchedulerProxy SchedulerIO
forkIoScheduler = SchedulerProxy


-- ** MessagePassing execution

handleProcess
  :: (HasLogging IO SchedulerIO, HasCallStack)
  => ProcessInfo
  -> Eff ProcEff ProcessExitReason
  -> Eff SchedulerIO ProcessExitReason
handleProcess myProcessInfo =
   handle_relay_s
      0
      (const return)
      (\ !nextRef !request k ->
        stepProcessInterpreter nextRef request k return
      )
 where
  myPid = myProcessInfo ^. processId
  myProcessStateVar = myProcessInfo ^. processState
  setMyProcessState = lift . atomically . setMyProcessStateSTM
  setMyProcessStateSTM = writeTVar myProcessStateVar
  myMessageQVar = myProcessInfo ^. messageQ

  kontinueWith
    :: forall s v a
    .  HasCallStack
    => (s -> Arr SchedulerIO v a)
    -> s
    -> Arr SchedulerIO v a
  kontinueWith kontinue !nextRef !result = do
    setMyProcessState ProcessIdle
    lift yield
    kontinue nextRef result

  diskontinueWith
    :: forall a
    .  HasCallStack
    => Arr SchedulerIO ProcessExitReason a
    -> Arr SchedulerIO ProcessExitReason a
  diskontinueWith diskontinue !reason = do
    setMyProcessState ProcessShuttingDown
    diskontinue reason

  stepProcessInterpreter
    :: forall v a
     . HasCallStack
    => Int
    -> Process SchedulerIO v
    -> (Int -> Arr SchedulerIO v a)
    -> Arr SchedulerIO ProcessExitReason a
    -> Eff SchedulerIO a
  stepProcessInterpreter !nextRef !request kontinue diskontinue =

      -- handle process shutdown requests:
      --   1. take process exit reason
      --   2. set process state to ProcessShuttingDown
      --   3. apply kontinue to (Right ShutdownRequested)
      --
      tryTakeNextShutdownRequest
        >>= maybe noShutdownRequested onShutdownRequested
      where
        tryTakeNextShutdownRequest =
          lift (atomically (tryTakeNextShutdownRequestSTM myMessageQVar))

        onShutdownRequested shutdownRequest = do
           logDebug "shutdownRequested"
           setMyProcessState ProcessShuttingDown
           interpretRequestAfterShutdownRequest
             (kontinueWith kontinue nextRef)
             (diskontinueWith diskontinue)
             shutdownRequest
             request

        noShutdownRequested = do
           setMyProcessState ProcessBusy
           interpretRequest
             (kontinueWith kontinue)
             (diskontinueWith diskontinue)
             nextRef
             request

  -- This gets no nextRef and may not pass a Left value to the continuation.
  -- This forces the caller to defer the process exit to the next request
  -- and hence ensures that the scheduler code cannot forget to allow the
  -- client code to react to a shutdown request.
  interpretRequestAfterShutdownRequest
    :: forall v a
     . HasCallStack
    => Arr SchedulerIO v a
    -> Arr SchedulerIO ProcessExitReason a
    -> ShutdownRequest
    -> Process SchedulerIO v
    -> Eff SchedulerIO a
  interpretRequestAfterShutdownRequest kontinue diskontinue shutdownRequest =
    \case
      SendMessage _ _          -> kontinue (ShutdownRequested shutdownRequest)
      SendShutdown toPid r     ->
        if toPid == myPid
          then diskontinue (ProcessShutDown r)
          else kontinue (ShutdownRequested shutdownRequest)
      Spawn _                  -> kontinue (ShutdownRequested shutdownRequest)
      ReceiveSelectedMessage _ -> kontinue (ShutdownRequested shutdownRequest)
      SelfPid                  -> kontinue (ShutdownRequested shutdownRequest)
      MakeReference            -> kontinue (ShutdownRequested shutdownRequest)
      YieldProcess             -> kontinue (ShutdownRequested shutdownRequest)
      Shutdown r               -> diskontinue (ProcessShutDown r)
      RaiseError e             -> diskontinue (ProcessRaisedError e)

  interpretRequest
    :: forall v a
     . HasCallStack
    => (Int -> Arr SchedulerIO v a)
    -> Arr SchedulerIO ProcessExitReason a
    -> Int
    -> Process SchedulerIO v
    -> Eff SchedulerIO a
  interpretRequest kontinue diskontinue nextRef =
    \case
      SendMessage toPid msg    -> interpretSend toPid msg >>= kontinue nextRef . ResumeWith
      SendShutdown toPid msg   ->
        if toPid == myPid
          then kontinue nextRef (ShutdownRequested msg)
          else interpretSendShutdown toPid msg >>= kontinue nextRef . ResumeWith
      Spawn child              -> spawnNewProcess child >>= kontinue nextRef . ResumeWith . fst
      ReceiveSelectedMessage f -> interpretReceive f >>= kontinue nextRef
      SelfPid                  -> kontinue nextRef (ResumeWith myPid)
      MakeReference            -> kontinue (nextRef + 1) (ResumeWith nextRef)
      YieldProcess             -> kontinue nextRef (ResumeWith  ())
      Shutdown r               -> diskontinue (ProcessShutDown r)
      RaiseError e             -> diskontinue (ProcessRaisedError e)
    where

      interpretSend !toPid !msg =
        setMyProcessState ProcessBusySending *>
        getSchedulerState >>= lift . atomically . enqueueMessageOtherProcess toPid msg

      interpretSendShutdown !toPid !msg =
        setMyProcessState ProcessBusySendingShutdown *>
        getSchedulerState >>= lift . atomically . enqueueShutdownRequest toPid msg

      interpretReceive :: MessageSelector b -> Eff SchedulerIO (ResumeProcess b)
      interpretReceive f = do
        setMyProcessState ProcessBusyReceiving
        lift $ atomically $ do
          myMessageQ <- readTVar myMessageQVar
          case myMessageQ ^. shutdownRequests of
            Nothing ->
              case partitionMessages (myMessageQ ^. incomingMessages) Seq.Empty of
                Nothing -> retry
                Just (selectedMessage, otherMessages) -> do
                  modifyTVar' myMessageQVar (incomingMessages .~ otherMessages)
                  return (ResumeWith selectedMessage)
            Just shutdownRequest -> do
              modifyTVar' myMessageQVar (shutdownRequests .~ Nothing)
              return (ShutdownRequested shutdownRequest)

       where
         partitionMessages Seq.Empty       _acc = Nothing
         partitionMessages (m :<| msgRest) acc  = maybe
           (partitionMessages msgRest (acc :|> m))
           (\res -> Just (res, acc Seq.>< msgRest))
           (runMessageSelector f m)

-- | This is the main entry point to running a message passing concurrency
-- application. This function takes a 'Process' on top of the 'SchedulerIO'
-- effect and a 'LogChannel' for concurrent logging.
schedule
  :: (HasLogging IO SchedulerIO, HasCallStack)
  => Eff ProcEff ()
  -> Eff LoggingAndIO ()
schedule procEff =
  liftBaseWith (\runS ->
    Async.withAsync (runS $ withNewSchedulerState $ do
        (_, mainProcAsync) <- spawnNewProcess $ do
          logNotice "++++++++ main process started ++++++++"
          procEff
          logNotice "++++++++ main process returned ++++++++"
        lift (void (Async.wait mainProcAsync))
      )
      (\ast -> runS $ do
        a <- restoreM ast
        void $ lift (Async.wait a)
      )
  ) >>= restoreM

spawnNewProcess
  :: (HasLogging IO SchedulerIO, HasCallStack)
  => Eff ProcEff ()
  -> Eff SchedulerIO (ProcessId, Async ProcessExitReason)
spawnNewProcess mfa = do
  schedulerState <- getSchedulerState
  procInfo <- allocateProcInfo schedulerState
  let pid = procInfo ^. processId
  procAsync <- doForkProc procInfo schedulerState
  return (procInfo ^. processId, procAsync)
 where
    allocateProcInfo schedulerState =
      lift (atomically (do
          let nextPidVar = schedulerState ^. nextPid
              processInfosVar = schedulerState ^. processTable
          pid <- readTVar nextPidVar
          modifyTVar' nextPidVar (+1)
          procInfo <- newProcessInfo pid
          modifyTVar' processInfosVar (at pid ?~ procInfo)
          return procInfo
          ))

    logAppendProcInfo pid =
      let addProcessId = over lmProcessId
            (maybe (Just (printf "% 9s" (show pid))) Just)
      in traverseLogMessages
         (lift . traverse setLogMessageThreadId
          >=> traverse (return . addProcessId))

    doForkProc procInfo schedulerState =
      restoreM =<< liftBaseWith
        (\inScheduler -> do
          let cancellationsVar = schedulerState ^. processCancellationTable
              processInfosVar = schedulerState ^. processTable
              pid = procInfo ^. processId
          procAsync <- Async.async (inScheduler (logAppendProcInfo pid
            (Safe.bracketWithError
              (logDebug "enter process")
              (\mExc () ->
                  do let e = case mExc of
                               Nothing ->
                                 ProcessReturned
                               Just exc ->
                                 case Safe.fromException exc of
                                   Just Async.AsyncCancelled ->
                                     ProcessCancelled

                                   Nothing ->
                                     ProcessCaughtIOException
                                         (prettyCallStack callStack)
                                         (Safe.displayException exc)
                     lift (atomically
                       (do modifyTVar' processInfosVar (at pid .~ Nothing)
                           modifyTVar' cancellationsVar (at pid .~ Nothing)))
                     case e of
                      ProcessCancelled ->
                        logProcessExit e

                      _ -> do
                        logProcessExit e
                        lastProcessState <-
                          lift (readTVarIO (procInfo ^. processState))
                        when (  lastProcessState /= ProcessShuttingDown
                             && lastProcessState /= ProcessIdle )
                          (logNotice
                            ( "process was not in shutting down state: "
                            ++ show lastProcessState))

              )
              (const
                (handleProcess procInfo (mfa >> return ProcessReturned))))))
          atomically (modifyTVar' cancellationsVar (at pid ?~ procAsync))
          return procAsync)

-- * Scheduler Accessor

getSchedulerState :: HasSchedulerIO r => Eff r SchedulerState
getSchedulerState = ask

enqueueMessageOtherProcess ::
  HasCallStack => ProcessId -> Dynamic -> SchedulerState -> STM Bool
enqueueMessageOtherProcess toPid msg schedulerState =
  view (at toPid) <$> readTVar (schedulerState ^. processTable)
   >>= maybe (return False)
             (\toProcessTable -> do
                modifyTVar' (toProcessTable ^. messageQ ) (incomingMessages %~ (:|> msg))
                return True)

enqueueShutdownRequest ::
  HasCallStack => ProcessId -> ShutdownRequest -> SchedulerState -> STM Bool
enqueueShutdownRequest toPid msg schedulerState =
  view (at toPid) <$> readTVar (schedulerState ^. processTable)
   >>= maybe (return False)
             (\toProcessTable -> do
                modifyTVar' (toProcessTable ^. messageQ ) (shutdownRequests ?~ msg)
                return True)
