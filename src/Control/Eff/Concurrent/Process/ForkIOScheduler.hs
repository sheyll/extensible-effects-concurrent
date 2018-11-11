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

import           Control.Exception.Safe        as Safe
import qualified Control.Eff.ExceptionExtra    as ExcExtra ()
import           Control.Concurrent.STM        as STM
import           Control.Concurrent             (yield)
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
import           Data.Default
import           Data.Dynamic
import           Data.Foldable
import           Data.Kind                      ( )
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import qualified Data.Set                       as Set
import           Data.Maybe
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           GHC.Stack
import           Text.Printf
import           System.Timeout

-- * Process Types

-- | A message queue of a process, contains the actual queue and maybe an
-- exit reason.
data MessageQ = MessageQ { _incomingMessages :: Seq Dynamic
                         , _shutdownRequests :: Maybe SomeProcessExitReason
                         }

instance Default MessageQ where def = MessageQ def def

makeLenses ''MessageQ

-- | Return any '_shutdownRequests' from a 'MessageQ' in a 'TVar' and
-- reset the '_shutdownRequests' field to 'Nothing' in the 'TVar'.
tryTakeNextShutdownRequestSTM
  :: TVar MessageQ -> STM (Maybe SomeProcessExitReason)
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
                             , _processLinks      :: TVar (Set ProcessId)
                             }

makeLenses ''ProcessInfo

-- * Scheduler Types

-- | Contains all process info'elements, as well as the state needed to
-- implement inter process communication. It contains also a 'LogChannel' to
-- which the logs of all processes are forwarded to.
data SchedulerState =
               SchedulerState { _nextPid :: TVar ProcessId
                              , _processTable :: TVar (Map ProcessId ProcessInfo)
                              , _processCancellationTable
                                :: TVar (Map ProcessId
                                    (Async (ExitReason 'NoRecovery)))
                              }

makeLenses ''SchedulerState

-- * Process Implementation

instance Show ProcessInfo where
  show p =  "process info: " ++ show (p ^. processId)

-- | Create a new 'ProcessInfo'
newProcessInfo :: HasCallStack => ProcessId -> STM ProcessInfo
newProcessInfo a =
  ProcessInfo a <$> newTVar ProcessBooting <*> newTVar def <*> newTVar def

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
        (logError . ("scheduler setup crashed with: " ++) . Safe.displayException)
        exceptions
      logDebug "scheduler cleanup begin"
      runReader schedulerState
        (provideInterrupts tearDownScheduler
           >>= either logProcessExit return)
      )
    (\schedulerState  -> do
      logDebug "scheduler loop entered"
      x <- runReader schedulerState
        (provideInterrupts mainProcessAction
          >>= either logProcessExit return)
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
          (\runS -> timeout 5000000
            (Async.mapConcurrently
              (\a -> do
                Async.cancel a
                runS (logNotice ("process cancelled: "++ show (asyncThreadId a)))
              )
              allProcesses
             >> runS (logNotice "all processes cancelled"))))

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
type SchedulerIO = ( Interrupts ': Reader SchedulerState : LoggingAndIO)

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
  -> Eff ProcEff (ExitReason 'NoRecovery)
  -> Eff SchedulerIO (ExitReason 'NoRecovery)
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
    => Arr SchedulerIO (ExitReason 'NoRecovery) a
    -> Arr SchedulerIO (ExitReason 'NoRecovery) a
  diskontinueWith diskontinue !reason = do
    setMyProcessState ProcessShuttingDown
    diskontinue reason

  stepProcessInterpreter
    :: forall v a
     . HasCallStack
    => Int
    -> Process SchedulerIO v
    -> (Int -> Arr SchedulerIO v a)
    -> Arr SchedulerIO (ExitReason 'NoRecovery) a
    -> Eff SchedulerIO a
  stepProcessInterpreter !nextRef !request kontinue diskontinue =

      -- handle process shutdown requests:
      --   1. take process exit reason
      --   2. set process state to ProcessShuttingDown
      --   3. apply kontinue to (Right Interrupted)
      --
      tryTakeNextShutdownRequest
        >>= maybe noShutdownRequested
            (either onShutdownRequested onInterruptRequested
              . fromSomeProcessExitReason)
      where
        tryTakeNextShutdownRequest =
          lift (atomically (tryTakeNextShutdownRequestSTM myMessageQVar))

        onShutdownRequested shutdownRequest = do
           logDebug "shutdownRequested"
           setMyProcessState ProcessShuttingDown
           interpretRequestAfterShutdownRequest
             (diskontinueWith diskontinue)
             shutdownRequest
             request

        onInterruptRequested interruptRequest = do
           logDebug "interruptRequested"
           setMyProcessState ProcessShuttingDown
           interpretRequestAfterInterruptRequest
             (kontinueWith kontinue nextRef)
             (diskontinueWith diskontinue)
             interruptRequest
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
    => Arr SchedulerIO (ExitReason 'NoRecovery) a
    -> (ExitReason 'NoRecovery)
    -> Process SchedulerIO v
    -> Eff SchedulerIO a
  interpretRequestAfterShutdownRequest diskontinue shutdownRequest =
    \case
      SendMessage _ _          -> diskontinue shutdownRequest
      SendInterrupt _ _        -> diskontinue shutdownRequest
      SendShutdown toPid r     ->
        if toPid == myPid
          then diskontinue r
          else diskontinue shutdownRequest
      Spawn _                  -> diskontinue shutdownRequest
      ReceiveSelectedMessage _ -> diskontinue shutdownRequest
      SelfPid                  -> diskontinue shutdownRequest
      MakeReference            -> diskontinue shutdownRequest
      YieldProcess             -> diskontinue shutdownRequest
      Shutdown r               -> diskontinue r
      Monitor _                -> diskontinue shutdownRequest
      Demonitor _              -> diskontinue shutdownRequest
      Link _                   -> diskontinue shutdownRequest
      Unlink _                 -> diskontinue shutdownRequest

  interpretRequestAfterInterruptRequest
    :: forall v a
     . HasCallStack
    => Arr SchedulerIO v a
    -> Arr SchedulerIO (ExitReason 'NoRecovery) a
    -> (ExitReason 'Recoverable)
    -> Process SchedulerIO v
    -> Eff SchedulerIO a
  interpretRequestAfterInterruptRequest kontinue diskontinue interruptRequest =
    \case
      SendMessage _ _          -> kontinue (Interrupted interruptRequest)
      SendInterrupt _ _        -> kontinue (Interrupted interruptRequest)
      SendShutdown toPid r     ->
        if toPid == myPid
          then diskontinue r
          else kontinue (Interrupted interruptRequest)
      Spawn _                  -> kontinue (Interrupted interruptRequest)
      ReceiveSelectedMessage _ -> kontinue (Interrupted interruptRequest)
      SelfPid                  -> kontinue (Interrupted interruptRequest)
      MakeReference            -> kontinue (Interrupted interruptRequest)
      YieldProcess             -> kontinue (Interrupted interruptRequest)
      Shutdown r               -> diskontinue r
      Monitor _                -> kontinue (Interrupted interruptRequest)
      Demonitor _              -> kontinue (Interrupted interruptRequest)
      Link _                   -> kontinue (Interrupted interruptRequest)
      Unlink _                 -> kontinue (Interrupted interruptRequest)

  interpretRequest
    :: forall v a
     . HasCallStack
    => (Int -> Arr SchedulerIO v a)
    -> Arr SchedulerIO (ExitReason 'NoRecovery) a
    -> Int
    -> Process SchedulerIO v
    -> Eff SchedulerIO a
  interpretRequest kontinue diskontinue nextRef =
    \case
      SendMessage toPid msg    -> interpretSend toPid msg >>= kontinue nextRef . ResumeWith
      SendInterrupt toPid msg   ->
        if toPid == myPid
          then kontinue nextRef (Interrupted msg)
          else interpretSendShutdownOrInterrupt toPid (SomeProcessExitReason msg)
                 >>= kontinue nextRef . ResumeWith
      SendShutdown toPid msg   ->
        if toPid == myPid
          then diskontinue msg
          else interpretSendShutdownOrInterrupt toPid (SomeProcessExitReason msg)
                 >>= kontinue nextRef . ResumeWith
      Spawn child              -> spawnNewProcess child >>= kontinue nextRef . ResumeWith . fst
      ReceiveSelectedMessage f -> interpretReceive f >>= either diskontinue (kontinue nextRef)
      SelfPid                  -> kontinue nextRef (ResumeWith myPid)
      MakeReference            -> kontinue (nextRef + 1) (ResumeWith nextRef)
      YieldProcess             -> kontinue nextRef (ResumeWith  ())
      Shutdown r               -> diskontinue r
      Monitor _                -> kontinue nextRef (Interrupted (ProcessError "Not Yet Implemented: Monitor"))
      Demonitor _              -> kontinue nextRef (Interrupted (ProcessError "Not Yet Implemented: Demonitor"))
      Link toPid               -> interpretLink toPid
                                    >>= kontinue nextRef . either Interrupted ResumeWith
      Unlink _                 -> kontinue nextRef (Interrupted (ProcessError "Not Yet Implemented: Unlink"))
    where
      interpretLink !toPid = do
        setMyProcessState ProcessBusyLinking
        schedulerState <- getSchedulerState
        let procInfosVar = schedulerState^.processTable
        lift $ atomically $ do
          procInfos <- readTVar procInfosVar
          case procInfos ^. at toPid of
            Just toProcInfo -> do
              modifyTVar' (toProcInfo ^. processLinks) (Set.insert myPid)
              modifyTVar' (myProcessInfo ^. processLinks) (Set.insert toPid)
              return (Right ())
            Nothing ->
              return (Left
                (LinkedProcessCrashed toPid Crash Recoverable))

      interpretSend !toPid msg =
        setMyProcessState ProcessBusySending *>
        getSchedulerState
          >>= lift
          . atomically
          . enqueueMessageOtherProcess toPid msg

      interpretSendShutdownOrInterrupt !toPid !msg =
        setMyProcessState
          (either
            (const ProcessBusySendingShutdown)
            (const ProcessBusySendingInterrupt)
            (fromSomeProcessExitReason msg))
        *> getSchedulerState
           >>= lift
           . atomically
           . enqueueShutdownRequest toPid msg

      interpretReceive
        :: MessageSelector b
        -> Eff SchedulerIO (Either (ExitReason 'NoRecovery) (ResumeProcess b))
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
                  return (Right (ResumeWith selectedMessage))
            Just shutdownRequest -> do
              modifyTVar' myMessageQVar (shutdownRequests .~ Nothing)
              case fromSomeProcessExitReason shutdownRequest of
                Left sr ->
                  return (Left sr)
                Right ir ->
                  return (Right (Interrupted ir))

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
  -> Eff SchedulerIO (ProcessId, Async (ExitReason 'NoRecovery))
spawnNewProcess mfa = do
  schedulerState <- getSchedulerState
  procInfo <- allocateProcInfo schedulerState
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
         (traverse setLogMessageThreadId
          >=> traverse (return . addProcessId))

    triggerProcessLinks !pid !exitSeverity !exitRecovery linkSetVar = do
      logCritical "0 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx"
      schedulerState <- getSchedulerState
      let sendIt !linkedPid = do
            let msg = SomeProcessExitReason
                      (LinkedProcessCrashed pid exitSeverity exitRecovery)
            logCritical ("1 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx " ++ show linkedPid)
            lift $ atomically $ do
              procInfos <- readTVar (schedulerState ^. processTable)
              let mLinkedProcInfo = procInfos ^? at linkedPid . _Just
              case mLinkedProcInfo of
                Nothing ->
                  return (Left linkedPid)
                Just linkedProcInfo ->
                  let linkedMsgQVar    = linkedProcInfo ^. messageQ
                      linkedLinkSetVar = linkedProcInfo ^. processLinks
                  in do linkedLinkSet <- readTVar linkedLinkSetVar
                        if Set.member pid linkedLinkSet
                          then do
                            writeTVar   linkedLinkSetVar
                                        (Set.delete pid linkedLinkSet)
                            modifyTVar' linkedMsgQVar
                                        (shutdownRequests ?~ msg)
                            return (Right linkedPid)
                          else
                            return (Left linkedPid)
      linkedPids <- lift (atomically (do linkSet <- readTVar linkSetVar
                                         writeTVar linkSetVar def
                                         return linkSet))
      logCritical ("6 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx" ++ show linkedPids)
      res <- traverse sendIt (toList linkedPids)
      logCritical ("7 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx" ++ show linkedPids)
      traverse_ (logDebug . either (("linked process already dead: " ++) . show)
                (("sent linked process exit to: " ++) . show))
                res

    mergeEitherInterruptAndExitReason ::
      Either InterruptReason (ExitReason 'NoRecovery) -> ExitReason 'NoRecovery
    mergeEitherInterruptAndExitReason = either NotRecovered id

    doForkProc :: ProcessInfo -> SchedulerState -> Eff SchedulerIO (Async (ExitReason 'NoRecovery))
    doForkProc procInfo schedulerState =
      restoreM =<< liftBaseWith
        (\inScheduler -> do
          let cancellationsVar = schedulerState ^. processCancellationTable
              processInfosVar = schedulerState ^. processTable
              pid = procInfo ^. processId
          procAsync <- Async.async (
            mergeEitherInterruptAndExitReason <$>
            inScheduler (logAppendProcInfo pid
            (Safe.bracketWithError
              (logDebug "enter process")
              (\mExc () ->
                  do let e = case mExc of
                               Nothing ->
                                 ExitNormally

                               Just exc ->
                                 case Safe.fromException exc of
                                   Just Async.AsyncCancelled ->
                                     Killed

                                   Nothing ->
                                     UnexpectedException
                                         (prettyCallStack callStack)
                                         (Safe.displayException exc)
                     lift (atomically
                       (do modifyTVar' processInfosVar (at pid .~ Nothing)
                           modifyTVar' cancellationsVar (at pid .~ Nothing)))
                     -- handle links and monitors
                     --    links are stored as pids in a set in the process info
                     triggerProcessLinks pid (toExitSeverity e) (toExitRecovery e) (procInfo ^. processLinks)
                     logCritical "8 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx"
                     -- log the exit reason and die
                     case e of
                      Killed ->
                        logProcessExit e

                      _ -> do
                        logProcessExit e
                        lastProcessState <-
                          lift (readTVarIO (procInfo ^. processState))
                        logCritical "9 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx"
                        when (  lastProcessState /= ProcessShuttingDown
                             && lastProcessState /= ProcessIdle )
                          (logNotice
                            ( "process was not in shutting down state: "
                            ++ show lastProcessState))
                        logCritical "10 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx"

              )
              (const
                (handleProcess procInfo (mfa >> return ExitNormally))))))
          atomically (modifyTVar' cancellationsVar (at pid ?~ procAsync))
          return procAsync)

-- * Scheduler Accessor

getSchedulerState :: HasSchedulerIO r => Eff r SchedulerState
getSchedulerState = ask

enqueueMessageOtherProcess ::
  HasCallStack => ProcessId -> Dynamic -> SchedulerState -> STM ()
enqueueMessageOtherProcess toPid msg schedulerState =
  view (at toPid) <$> readTVar (schedulerState ^. processTable)
   >>= maybe (return ())
             (\toProcessTable -> do
                modifyTVar' (toProcessTable ^. messageQ ) (incomingMessages %~ (:|> msg))
                return ())

enqueueShutdownRequest ::
  HasCallStack => ProcessId -> SomeProcessExitReason -> SchedulerState -> STM ()
enqueueShutdownRequest toPid msg schedulerState =
  view (at toPid) <$> readTVar (schedulerState ^. processTable)
   >>= maybe (return ())
             (\toProcessTable -> do
                modifyTVar' (toProcessTable ^. messageQ ) (shutdownRequests ?~ msg)
                return ())
