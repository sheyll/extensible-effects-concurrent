-- | Implement Erlang style message passing concurrency.
--
-- This module contains 'spawn' which handles the 'Process' effects, using
-- 'STM.TQueue's and 'Control.Concurrent.Async.withAsync'.
--
-- This aims to be a pragmatic implementation, so even logging is
-- supported.
--
-- At the core is a /main process/ that enters 'schedule'
-- and creates all of the internal state stored in 'STM.TVar's
-- to manage processes with message queues.
--
module Control.Eff.Concurrent.Process.ForkIOScheduler
  ( schedule
  , defaultMain
  , defaultMainWithLogWriter
  , ProcEff
  , InterruptableProcEff
  , SchedulerIO
  , HasSchedulerIO
  )
where

import           Control.Concurrent             ( yield )
import qualified Control.Concurrent.Async      as Async
import           Control.Concurrent.Async       ( Async(..) )
import           Control.Concurrent.STM        as STM
import           Control.Eff
import           Control.Eff.Concurrent.Process
import qualified Control.Eff.ExceptionExtra    as ExcExtra
                                                ( )
import           Control.Eff.Extend
import           Control.Eff.Log
import           Control.Eff.LogWriter.IO
import           Control.Eff.LogWriter.Console
import           Control.Eff.LogWriter.Async
import           Control.Eff.Reader.Strict     as Reader
import           Control.Exception.Safe        as Safe
import           Control.Lens
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.Trans.Control    ( MonadBaseControl(..)
                                                , control
                                                )
import           Data.Default
import           Data.Foldable
import           Data.Function                  ( fix )
import           Data.Kind                      ( )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           GHC.Stack
import           System.Timeout
import           Text.Printf

-- * Process Types

-- | A message queue of a process, contains the actual queue and maybe an
-- exit reason. The message queue is backed by a 'Seq' sequence with 'StrictDynamic' values.
data MessageQ = MessageQ
  { _incomingMessages :: Seq StrictDynamic
  , _shutdownRequests :: Maybe SomeExitReason
  }

instance Default MessageQ where
  def = MessageQ def def

makeLenses ''MessageQ

-- | Return any '_shutdownRequests' from a 'MessageQ' in a 'TVar' and
-- reset the '_shutdownRequests' field to 'Nothing' in the 'TVar'.
tryTakeNextShutdownRequestSTM :: TVar MessageQ -> STM (Maybe SomeExitReason)
tryTakeNextShutdownRequestSTM mqVar = do
  mq <- readTVar mqVar
  when (isJust (mq ^. shutdownRequests))
       (writeTVar mqVar (mq & shutdownRequests .~ Nothing))
  return (mq ^. shutdownRequests)

-- | Information about a process, needed to implement
-- 'Process' handlers. The message queue is backed by a 'STM.TVar' that contains
-- a 'MessageQ'.
data ProcessInfo = ProcessInfo
  { _processId :: ProcessId
  , _processState :: TVar ProcessState
  , _messageQ :: TVar MessageQ
  , _processLinks :: TVar (Set ProcessId)
  }

makeLenses ''ProcessInfo

-- * Scheduler Types

-- | Contains all process info'elements, as well as the state needed to implement
-- inter-process communication.
data SchedulerState = SchedulerState
  { _nextPid :: TVar ProcessId
  , _processTable :: TVar (Map ProcessId ProcessInfo)
  , _processCancellationTable :: TVar (Map ProcessId (Async (ExitReason 'NoRecovery)))
  , _processMonitors :: TVar (Set (MonitorReference, ProcessId))  -- ^ Set of monitors and monitor owners
  , _nextMonitorIndex :: TVar Int
  }

makeLenses ''SchedulerState

logSchedulerState :: (HasCallStack, Member Logs e, Lifted IO e) => SchedulerState -> Eff e ()
logSchedulerState s = do
  (np, pt, pct, pm, nm) <- lift $ atomically $ do
    np  <- T.pack . show <$> readTVar (s ^. nextPid)
    pt  <- fmap (T.pack . show) <$> readTVar (s ^. processTable)
    pct <- fmap (T.pack . show) . Map.keys <$> readTVar (s ^. processCancellationTable)
    pm  <- fmap (T.pack . show) . Set.toList <$> readTVar (s ^. processMonitors)
    nm  <- T.pack . show <$> readTVar (s ^. nextMonitorIndex)
    return (np, pt, pct, pm, nm)
  logDebug "ForkIO Scheduler info"
  logDebug $ "nextPid: " <> np
  logDebug $ "process table:"
  traverse_ logDebug pt
  logDebug $ "process cancellation table:"
  traverse_ logDebug pct
  logDebug $ "process monitors:"
  traverse_ logDebug pm
  logDebug $ "nextMonitorIndex: " <> nm

-- | Add monitor: If the process is dead, enqueue a 'ProcessDown' message into the
-- owners message queue
addMonitoring
  :: ProcessId -> ProcessId -> SchedulerState -> STM MonitorReference
addMonitoring target owner schedulerState = do
  aNewMonitorIndex <- readTVar (schedulerState ^. nextMonitorIndex)
  modifyTVar' (schedulerState ^. nextMonitorIndex) (+ 1)
  let monitorRef = MonitorReference aNewMonitorIndex target
  when (target /= owner) $ do
    pt <- readTVar (schedulerState ^. processTable)
    if Map.member target pt
      then modifyTVar' (schedulerState ^. processMonitors)
                       (Set.insert (monitorRef, owner))
      else
        let processDownMessage =
              ProcessDown monitorRef (SomeExitReason (ProcessNotRunning target))
        in  enqueueMessageOtherProcess owner
                                       (toStrictDynamic processDownMessage)
                                       schedulerState
  return monitorRef

removeMonitoring :: MonitorReference -> SchedulerState -> STM ()
removeMonitoring monitorRef schedulerState = modifyTVar'
  (schedulerState ^. processMonitors)
  (Set.filter (\(ref, _) -> ref /= monitorRef))

triggerAndRemoveMonitor
  :: ProcessId -> SomeExitReason -> SchedulerState -> STM ()
triggerAndRemoveMonitor downPid reason schedulerState = do
  monRefs <- readTVar (schedulerState ^. processMonitors)
  traverse_ go monRefs
 where
  go (mr, owner) = when
    (monitoredProcess mr == downPid)
    (do
      let processDownMessage = ProcessDown mr reason
      enqueueMessageOtherProcess owner (toStrictDynamic processDownMessage) schedulerState
      removeMonitoring mr schedulerState
    )

-- * Process Implementation
instance Show ProcessInfo where
  show p = "process info: " ++ show (p ^. processId)

-- | Create a new 'ProcessInfo'
newProcessInfo :: HasCallStack => ProcessId -> STM ProcessInfo
newProcessInfo a =
  ProcessInfo a <$> newTVar ProcessBooting <*> newTVar def <*> newTVar def

-- * Scheduler Implementation

-- | Create a new 'SchedulerState'
newSchedulerState :: HasCallStack => STM SchedulerState
newSchedulerState =
  SchedulerState
    <$> newTVar 1
    <*> newTVar def
    <*> newTVar def
    <*> newTVar def
    <*> newTVar def

-- | Create a new 'SchedulerState' run an IO action, catching all exceptions,
-- and when the actions returns, clean up and kill all processes.
withNewSchedulerState
  :: (HasCallStack) => Eff SchedulerIO () -> Eff LoggingAndIo ()
withNewSchedulerState mainProcessAction = Safe.bracketWithError
  (lift (atomically newSchedulerState))
  (\exceptions schedulerState -> do
    traverse_
      ( logError
      . ("scheduler setup crashed with: " <>)
      . T.pack
      . Safe.displayException
      )
      exceptions
    logDebug "scheduler cleanup begin"
    runReader schedulerState tearDownScheduler
  )
  (\schedulerState -> do
    logDebug "scheduler loop entered"
    x <- runReader schedulerState mainProcessAction
    logDebug "scheduler loop returned"
    return x
  )
 where
  tearDownScheduler :: Eff SchedulerIO ()
  tearDownScheduler = do
    schedulerState <- getSchedulerState
    let cancelTableVar = schedulerState ^. processCancellationTable
    -- cancel all processes
    allProcesses <- lift
      (atomically (readTVar cancelTableVar <* writeTVar cancelTableVar def))
    logNotice
      (  "cancelling processes: "
      <> T.pack (show (toListOf (ifolded . asIndex) allProcesses))
      )
    void
      (liftBaseWith
        (\runS -> timeout
          5000000
          (  Async.mapConcurrently
              (\a -> do
                Async.cancel a
                runS
                  (logNotice
                    ("process cancelled: " <> T.pack (show (asyncThreadId a)))
                  )
              )
              allProcesses
          >> runS (logNotice "all processes cancelled")
          )
        )
      )

-- | The concrete list of 'Eff'ects of processes compatible with this scheduler.
-- This builds upon 'SchedulerIO'.
type ProcEff = ConsProcess SchedulerIO

-- | The concrete list of the effects, that the 'Process' uses
type InterruptableProcEff = InterruptableProcess SchedulerIO

-- | Type class constraint to indicate that an effect union contains the
-- effects required by every process and the scheduler implementation itself.
type HasSchedulerIO r = (HasCallStack, Lifted IO r, SchedulerIO <:: r)

-- | The concrete list of 'Eff'ects for this scheduler implementation.
type SchedulerIO = (Reader SchedulerState : LoggingAndIo)

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMain :: HasCallStack => Eff InterruptableProcEff () -> IO ()
defaultMain = defaultMainWithLogWriter consoleLogWriter

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMainWithLogWriter
  :: HasCallStack => LogWriter IO -> Eff InterruptableProcEff () -> IO ()
defaultMainWithLogWriter lw =
  runLift . withLogging lw . withAsyncLogWriter (1024 :: Int) . schedule

-- ** Process Execution

handleProcess
  :: (HasCallStack)
  => ProcessInfo
  -> Eff ProcEff (ExitReason 'NoRecovery)
  -> Eff SchedulerIO (ExitReason 'NoRecovery)
handleProcess myProcessInfo actionToRun = fix
  (handle_relay' singleStep (const . const (return ExitNormally)))
  actionToRun
  0
 where
  singleStep
    :: (Eff ProcEff xx -> (Int -> Eff SchedulerIO (ExitReason 'NoRecovery)))
    -> Arrs ProcEff x xx
    -> Process SchedulerIO x
    -> (Int -> Eff SchedulerIO (ExitReason 'NoRecovery))
  singleStep k q p !nextRef = stepProcessInterpreter
    nextRef
    p
    (\nextNextRef x -> k (qApp q x) nextNextRef)
    return
  myPid                = myProcessInfo ^. processId
  myProcessStateVar    = myProcessInfo ^. processState
  setMyProcessState    = lift . atomically . setMyProcessStateSTM
-- DEBUG variant:
-- setMyProcessState st = do
--  oldSt <- lift (atomically (readTVar myProcessStateVar <* setMyProcessStateSTM st))
--  logDebug ("state change: "<> show oldSt <> " -> " <> show st)
  setMyProcessStateSTM = writeTVar myProcessStateVar
  myMessageQVar        = myProcessInfo ^. messageQ
  kontinueWith
    :: forall s v a
     . HasCallStack
    => (s -> Arr SchedulerIO v a)
    -> (s -> Arr SchedulerIO v a)
  kontinueWith kontinue !nextRef !result = do
    setMyProcessState ProcessIdle
    lift yield
    kontinue nextRef result
  diskontinueWith
    :: forall a
     . HasCallStack
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
    tryTakeNextShutdownRequest >>= maybe
      noShutdownRequested
      (either onShutdownRequested onInterruptRequested . fromSomeExitReason)
    -- handle process shutdown requests:
    --   1. take process exit reason
    --   2. set process state to ProcessShuttingDown
    --   3. apply kontinue to (Right Interrupted)
    --
   where
    tryTakeNextShutdownRequest =
      lift (atomically (tryTakeNextShutdownRequestSTM myMessageQVar))
    onShutdownRequested shutdownRequest = do
      logDebug ("shutdown requested: " <> T.pack (show shutdownRequest))
      setMyProcessState ProcessShuttingDown
      interpretRequestAfterShutdownRequest (diskontinueWith diskontinue)
                                           shutdownRequest
                                           request
    onInterruptRequested interruptRequest = do
      logDebug ("interrupt requested: " <> T.pack (show interruptRequest))
      setMyProcessState ProcessShuttingDown
      interpretRequestAfterInterruptRequest (kontinueWith kontinue nextRef)
                                            (diskontinueWith diskontinue)
                                            interruptRequest
                                            request
    noShutdownRequested = do
      setMyProcessState ProcessBusy
      interpretRequest (kontinueWith kontinue)
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
    -> ExitReason 'NoRecovery
    -> Process SchedulerIO v
    -> Eff SchedulerIO a
  interpretRequestAfterShutdownRequest diskontinue shutdownRequest = \case
    SendMessage   _ _ -> diskontinue shutdownRequest
    SendInterrupt _ _ -> diskontinue shutdownRequest
    SendShutdown toPid r ->
      if toPid == myPid then diskontinue r else diskontinue shutdownRequest
    Spawn                  _ -> diskontinue shutdownRequest
    SpawnLink              _ -> diskontinue shutdownRequest
    ReceiveSelectedMessage _ -> diskontinue shutdownRequest
    FlushMessages            -> diskontinue shutdownRequest
    SelfPid                  -> diskontinue shutdownRequest
    MakeReference            -> diskontinue shutdownRequest
    YieldProcess             -> diskontinue shutdownRequest
    Shutdown        r        -> diskontinue r
    GetProcessState _        -> diskontinue shutdownRequest
    Monitor         _        -> diskontinue shutdownRequest
    Demonitor       _        -> diskontinue shutdownRequest
    Link            _        -> diskontinue shutdownRequest
    Unlink          _        -> diskontinue shutdownRequest
  interpretRequestAfterInterruptRequest
    :: forall v a
     . HasCallStack
    => Arr SchedulerIO v a
    -> Arr SchedulerIO (ExitReason 'NoRecovery) a
    -> ExitReason 'Recoverable
    -> Process SchedulerIO v
    -> Eff SchedulerIO a
  interpretRequestAfterInterruptRequest kontinue diskontinue interruptRequest =
    \case
      SendMessage   _     _ -> kontinue (Interrupted interruptRequest)
      SendInterrupt _     _ -> kontinue (Interrupted interruptRequest)
      SendShutdown  toPid r -> if toPid == myPid
        then diskontinue r
        else kontinue (Interrupted interruptRequest)
      Spawn                  _ -> kontinue (Interrupted interruptRequest)
      SpawnLink              _ -> kontinue (Interrupted interruptRequest)
      ReceiveSelectedMessage _ -> kontinue (Interrupted interruptRequest)
      FlushMessages            -> kontinue (Interrupted interruptRequest)
      SelfPid                  -> kontinue (Interrupted interruptRequest)
      MakeReference            -> kontinue (Interrupted interruptRequest)
      YieldProcess             -> kontinue (Interrupted interruptRequest)
      Shutdown        r        -> diskontinue r
      GetProcessState _        -> kontinue (Interrupted interruptRequest)
      Monitor         _        -> kontinue (Interrupted interruptRequest)
      Demonitor       _        -> kontinue (Interrupted interruptRequest)
      Link            _        -> kontinue (Interrupted interruptRequest)
      Unlink          _        -> kontinue (Interrupted interruptRequest)
  interpretRequest
    :: forall v a
     . HasCallStack
    => (Int -> Arr SchedulerIO v a)
    -> Arr SchedulerIO (ExitReason 'NoRecovery) a
    -> Int
    -> Process SchedulerIO v
    -> Eff SchedulerIO a
  interpretRequest kontinue diskontinue nextRef = \case
    SendMessage toPid msg ->
      interpretSend toPid msg >>= kontinue nextRef . ResumeWith
    SendInterrupt toPid msg -> if toPid == myPid
      then kontinue nextRef (Interrupted msg)
      else
        interpretSendShutdownOrInterrupt toPid (SomeExitReason msg)
        >>= kontinue nextRef
        .   ResumeWith
    SendShutdown toPid msg -> if toPid == myPid
      then diskontinue msg
      else
        interpretSendShutdownOrInterrupt toPid (SomeExitReason msg)
        >>= kontinue nextRef
        .   ResumeWith
    Spawn child ->
      spawnNewProcess Nothing child >>= kontinue nextRef . ResumeWith . fst
    SpawnLink child ->
      spawnNewProcess (Just myProcessInfo) child
        >>= kontinue nextRef
        .   ResumeWith
        .   fst
    ReceiveSelectedMessage f ->
      interpretReceive f >>= either diskontinue (kontinue nextRef)
    FlushMessages -> interpretFlush >>= kontinue nextRef
    SelfPid       -> kontinue nextRef (ResumeWith myPid)
    MakeReference -> kontinue (nextRef + 1) (ResumeWith nextRef)
    YieldProcess  -> kontinue nextRef (ResumeWith ())
    Shutdown r    -> diskontinue r
    GetProcessState toPid ->
      interpretGetProcessState toPid >>= kontinue nextRef . ResumeWith
    Monitor target ->
      interpretMonitor target >>= kontinue nextRef . ResumeWith
    Demonitor ref -> interpretDemonitor ref >>= kontinue nextRef . ResumeWith
    Link toPid ->
      interpretLink toPid >>= kontinue nextRef . either Interrupted ResumeWith
    Unlink toPid -> interpretUnlink toPid >>= kontinue nextRef . ResumeWith
   where
    interpretMonitor !target = do
      setMyProcessState ProcessBusyMonitoring
      schedulerState <- getSchedulerState
      lift (atomically (addMonitoring target myPid schedulerState))
    interpretDemonitor !ref = do
      setMyProcessState ProcessBusyMonitoring
      schedulerState <- getSchedulerState
      lift (atomically (removeMonitoring ref schedulerState))
    interpretUnlink !toPid = do
      setMyProcessState ProcessBusyUnlinking
      schedulerState <- getSchedulerState
      let procInfoVar = schedulerState ^. processTable
      lift $ atomically $ do
        procInfo <- readTVar procInfoVar
        traverse_
          (\toProcInfo ->
            modifyTVar' (toProcInfo ^. processLinks) (Set.delete myPid)
          )
          (procInfo ^. at toPid)
        modifyTVar' (myProcessInfo ^. processLinks) (Set.delete toPid)
    interpretGetProcessState !toPid = do
      setMyProcessState ProcessBusy
      schedulerState <- getSchedulerState
      when (toPid == 1) $
        logSchedulerState schedulerState
      let procInfoVar = schedulerState ^. processTable
      lift $ atomically $ do
        procInfoTable <- readTVar procInfoVar
        traverse (\toProcInfo -> readTVar (toProcInfo ^. processState))
                 (procInfoTable ^. at toPid)
    interpretLink !toPid = do
      setMyProcessState ProcessBusyLinking
      schedulerState <- getSchedulerState
      let procInfoVar = schedulerState ^. processTable
      lift $ atomically $ do
        procInfoTable <- readTVar procInfoVar
        case procInfoTable ^. at toPid of
          Just toProcInfo -> do
            modifyTVar' (toProcInfo ^. processLinks)    (Set.insert myPid)
            modifyTVar' (myProcessInfo ^. processLinks) (Set.insert toPid)
            return (Right ())
          Nothing -> return (Left (LinkedProcessCrashed toPid))
    interpretSend !toPid msg =
      setMyProcessState ProcessBusySending
        *>  getSchedulerState
        >>= lift
        .   atomically
        .   enqueueMessageOtherProcess toPid msg
    interpretSendShutdownOrInterrupt !toPid !msg =
      setMyProcessState
          (either (const ProcessBusySendingShutdown)
                  (const ProcessBusySendingInterrupt)
                  (fromSomeExitReason msg)
          )
        *>  getSchedulerState
        >>= lift
        .   atomically
        .   enqueueShutdownRequest toPid msg
    interpretFlush :: Eff SchedulerIO (ResumeProcess [StrictDynamic])
    interpretFlush = do
      setMyProcessState ProcessBusyReceiving
      lift $ atomically $ do
        myMessageQ <- readTVar myMessageQVar
        modifyTVar' myMessageQVar (incomingMessages .~ Seq.Empty)
        return (ResumeWith (toList (myMessageQ ^. incomingMessages)))
    interpretReceive
      :: MessageSelector b
      -> Eff SchedulerIO (Either (ExitReason 'NoRecovery) (ResumeProcess b))
    interpretReceive f = do
      setMyProcessState ProcessBusyReceiving
      lift $ atomically $ do
        myMessageQ <- readTVar myMessageQVar
        case myMessageQ ^. shutdownRequests of
          Nothing ->
            case
                partitionMessages (myMessageQ ^. incomingMessages) Seq.Empty
              of
                Nothing                               -> retry
                Just (selectedMessage, otherMessages) -> do
                  modifyTVar' myMessageQVar (incomingMessages .~ otherMessages)
                  return (Right (ResumeWith selectedMessage))
          Just shutdownRequest -> do
            modifyTVar' myMessageQVar (shutdownRequests .~ Nothing)
            case fromSomeExitReason shutdownRequest of
              Left  sr -> return (Left sr)
              Right ir -> return (Right (Interrupted ir))
     where
      partitionMessages Seq.Empty       _acc = Nothing
      partitionMessages (m :<| msgRest) acc  = maybe
        (partitionMessages msgRest (acc :|> m))
        (\res -> Just (res, acc Seq.>< msgRest))
        (runMessageSelector f m)

-- | This is the main entry point to running a message passing concurrency
-- application. This function takes a 'Process' on top of the 'SchedulerIO'
-- effect for concurrent logging.
schedule :: (HasCallStack) => Eff InterruptableProcEff () -> Eff LoggingAndIo ()
schedule procEff =
  liftBaseWith
      (\runS -> Async.withAsync
        (runS $ withNewSchedulerState $ do
          (_, mainProcAsync) <- spawnNewProcess Nothing $ do
            logNotice "++++++++ main process started ++++++++"
            provideInterruptsShutdown procEff
            logNotice "++++++++ main process returned ++++++++"
          lift (void (Async.wait mainProcAsync))
        )
        (\ast -> runS $ do
          a <- restoreM ast
          void $ lift (Async.wait a)
        )
      )
    >>= restoreM

spawnNewProcess
  :: (HasCallStack)
  => Maybe ProcessInfo
  -> Eff ProcEff ()
  -> Eff SchedulerIO (ProcessId, Async (ExitReason 'NoRecovery))
spawnNewProcess mLinkedParent mfa = do
  schedulerState <- getSchedulerState
  procInfo       <- allocateProcInfo schedulerState
  traverse_ (linkToParent procInfo) mLinkedParent
  procAsync <- doForkProc procInfo schedulerState
  return (procInfo ^. processId, procAsync)
 where
  linkToParent toProcInfo parent = do
    let toPid     = toProcInfo ^. processId
        parentPid = parent ^. processId
    lift $ atomically $ do
      modifyTVar' (toProcInfo ^. processLinks) (Set.insert parentPid)
      modifyTVar' (parent ^. processLinks)     (Set.insert toPid)
  allocateProcInfo schedulerState = lift
    (atomically
      (do
        let nextPidVar     = schedulerState ^. nextPid
            processInfoVar = schedulerState ^. processTable
        pid <- readTVar nextPidVar
        modifyTVar' nextPidVar (+ 1)
        procInfo <- newProcessInfo pid
        modifyTVar' processInfoVar (at pid ?~ procInfo)
        return procInfo
      )
    )
  logAppendProcInfo pid =
    let addProcessId = over
          lmProcessId
          (maybe (Just (T.pack (printf "% 9s" (show pid)))) Just)
    in  censorLogs @IO addProcessId
  triggerProcessLinksAndMonitors
    :: ProcessId -> ExitReason e -> TVar (Set ProcessId) -> Eff SchedulerIO ()
  triggerProcessLinksAndMonitors !pid !reason !linkSetVar = do
    schedulerState <- getSchedulerState
    lift $ atomically $ triggerAndRemoveMonitor pid
                                                (SomeExitReason reason)
                                                schedulerState
    let exitSeverity = toExitSeverity reason
        sendIt !linkedPid = do
          let msg = SomeExitReason (LinkedProcessCrashed pid)
          lift $ atomically $ do
            procInfoTable <- readTVar (schedulerState ^. processTable)
            let mLinkedProcInfo = procInfoTable ^? ix linkedPid
            case mLinkedProcInfo of
              Nothing -> return (Left linkedPid)
              Just linkedProcInfo ->
                let linkedMsgQVar    = linkedProcInfo ^. messageQ
                    linkedLinkSetVar = linkedProcInfo ^. processLinks
                in  do
                      linkedLinkSet <- readTVar linkedLinkSetVar
                      if Set.member pid linkedLinkSet
                        then do
                          writeTVar linkedLinkSetVar
                                    (Set.delete pid linkedLinkSet)
                          when
                            (exitSeverity == Crash)
                            (modifyTVar' linkedMsgQVar
                                         (shutdownRequests ?~ msg)
                            )
                          return (Right linkedPid)
                        else return (Left linkedPid)
    linkedPids <- lift
      (atomically
        (do
          linkSet <- readTVar linkSetVar
          writeTVar linkSetVar def
          return linkSet
        )
      )
    res <- traverse sendIt (toList linkedPids)
    traverse_
      (logDebug . either
        (("linked process no found: " <>) . T.pack . show)
        (("sent shutdown to linked process: " <>) . T.pack . show)
      )
      res
  doForkProc
    :: ProcessInfo
    -> SchedulerState
    -> Eff SchedulerIO (Async (ExitReason 'NoRecovery))
  doForkProc procInfo schedulerState = control
    (\inScheduler -> do
      let cancellationsVar = schedulerState ^. processCancellationTable
          processInfoVar   = schedulerState ^. processTable
          pid              = procInfo ^. processId
      procAsync <- Async.async
        (inScheduler
          (logAppendProcInfo
            pid
            (Safe.bracketWithError
              (logDebug "enter process")
              (\mExc () -> do
                lift
                  (atomically
                    (do
                      modifyTVar' processInfoVar   (at pid .~ Nothing)
                      modifyTVar' cancellationsVar (at pid .~ Nothing)
                    )
                  )
                traverse_
                  (\exc -> logExitAndTriggerLinksAndMonitors
                    (exitReasonFromException exc)
                    pid
                  )
                  mExc
              )
              (const
                (do
                  res <- handleProcess procInfo (mfa >> return ExitNormally)
                  logExitAndTriggerLinksAndMonitors res pid
                )
              )
            )
          )
        )
      atomically (modifyTVar' cancellationsVar (at pid ?~ procAsync))
      return procAsync
    )
   where
    exitReasonFromException exc = case Safe.fromException exc of
      Just Async.AsyncCancelled -> Killed
      Nothing -> UnexpectedException (prettyCallStack callStack)
                                     (Safe.displayException exc)
    logExitAndTriggerLinksAndMonitors reason pid = do
      triggerProcessLinksAndMonitors pid reason (procInfo ^. processLinks)
      logProcessExit reason
      return reason

-- * Scheduler Accessor
getSchedulerState :: HasSchedulerIO r => Eff r SchedulerState
getSchedulerState = ask

enqueueMessageOtherProcess
  :: HasCallStack => ProcessId -> StrictDynamic -> SchedulerState -> STM ()
enqueueMessageOtherProcess toPid msg schedulerState =
  view (at toPid) <$> readTVar (schedulerState ^. processTable) >>= maybe
    (return ())
    (\toProcessTable -> do
      modifyTVar' (toProcessTable ^. messageQ) (incomingMessages %~ (:|> msg))
      return ()
    )

enqueueShutdownRequest
  :: HasCallStack => ProcessId -> SomeExitReason -> SchedulerState -> STM ()
enqueueShutdownRequest toPid msg schedulerState =
  view (at toPid) <$> readTVar (schedulerState ^. processTable) >>= maybe
    (return ())
    (\toProcessTable -> do
      modifyTVar' (toProcessTable ^. messageQ) (shutdownRequests ?~ msg)
      return ()
    )
