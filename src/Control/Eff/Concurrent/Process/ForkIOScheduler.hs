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
module Control.Eff.Concurrent.Process.ForkIOScheduler
  ( schedule,
    defaultMain,
    defaultMainWithLogWriter,
    SafeEffects,
    Effects,
    BaseEffects,
    HasBaseEffects,
  )
where

import Control.Concurrent (forkIO, killThread, threadDelay, yield)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async (..))
import Control.Concurrent.STM as STM
import Control.Eff
import Control.Eff.Concurrent.Process
import qualified Control.Eff.ExceptionExtra as ExcExtra
  (
  )
import Control.Eff.Extend
import Control.Eff.Log
import Control.Eff.LogWriter.Async
import Control.Eff.LogWriter.Console
import Control.Eff.Reader.Strict as Reader
import Control.Exception.Safe as Safe
import Control.Lens
import Control.Monad
  ( unless,
    void,
    when,
  )
import Control.Monad.Trans.Control
  ( MonadBaseControl (..),
    control,
  )
import Data.Default
import Data.Foldable
import Data.Function (fix)
import Data.Kind ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Stack
import System.Timeout

-- * Process Types

-- | A message queue of a process, contains the actual queue and maybe an
-- exit reason. The message queue is backed by a 'Seq' sequence with 'Message' values.
data MessageQ
  = MessageQ
      { _incomingMessages :: Seq Message,
        _shutdownRequests :: Maybe InterruptOrShutdown
      }

instance Default MessageQ where
  def = MessageQ def def

makeLenses ''MessageQ

-- | Return any '_shutdownRequests' from a 'MessageQ' in a 'TVar' and
-- reset the '_shutdownRequests' field to 'Nothing' in the 'TVar'.
tryTakeNextShutdownRequestSTM :: TVar MessageQ -> STM (Maybe InterruptOrShutdown)
tryTakeNextShutdownRequestSTM mqVar = do
  mq <- readTVar mqVar
  when
    (isJust (mq ^. shutdownRequests))
    (writeTVar mqVar (mq & shutdownRequests .~ Nothing))
  return (mq ^. shutdownRequests)

-- | Information about a process, needed to implement
-- 'Process' handlers. The message queue is backed by a 'STM.TVar' that contains
-- a 'MessageQ'.
data ProcessInfo
  = ProcessInfo
      { _processId :: ProcessId,
        _processTitle :: ProcessTitle,
        _processState :: TVar (ProcessDetails, ProcessState),
        _messageQ :: TVar MessageQ,
        _processLinks :: TVar (Set ProcessId)
      }

makeLenses ''ProcessInfo

-- * Scheduler Types

-- | Contains all process info'elements, as well as the state needed to implement
-- inter-process communication.
data SchedulerState
  = SchedulerState
      { _nextPid :: TVar ProcessId,
        _processTable :: TVar (Map ProcessId ProcessInfo),
        _processCancellationTable :: TVar (Map ProcessId (Async ShutdownReason)),
        -- | Set of monitors and monitor owners
        _processMonitors :: TVar (Set (MonitorReference, ProcessId)),
        _nextMonitorIndex :: TVar Int
      }

makeLenses ''SchedulerState

renderSchedulerState :: SchedulerState -> IO ProcessDetails
renderSchedulerState s = do
  (np, pt, pct, pm, nm) <- atomically $ do
    np <- T.pack . show <$> readTVar (s ^. nextPid)
    pt <- T.pack . show . Map.size <$> readTVar (s ^. processTable)
    pct <- T.pack . show . Map.size <$> readTVar (s ^. processCancellationTable)
    pm <- T.pack . show . Set.size <$> readTVar (s ^. processMonitors)
    nm <- T.pack . show <$> readTVar (s ^. nextMonitorIndex)
    return (np, pt, pct, pm, nm)
  return
    $ MkProcessDetails
    $ T.unlines
      [ T.pack "ForkIO Scheduler nextPid: " <> np,
        T.pack "ForkIO Scheduler process table entries: " <> pt,
        T.pack "ForkIO Scheduler process cancellation table entries: " <> pct,
        T.pack "ForkIO Scheduler process monitors entries: " <> pm,
        T.pack "ForkIO Scheduler nextMonitorIndex: " <> nm
      ]

-- | Allocate a new 'MonitorReference'
nextMonitorReference :: ProcessId -> SchedulerState -> STM MonitorReference
nextMonitorReference target schedulerState = do
  aNewMonitorIndex <- readTVar (schedulerState ^. nextMonitorIndex)
  modifyTVar' (schedulerState ^. nextMonitorIndex) (+ 1)
  return (MkMonitorReference aNewMonitorIndex target)

-- | Add monitor: If the process is dead, enqueue a 'ProcessDown' message into the
-- owners message queue
addMonitoring ::
  HasCallStack => MonitorReference -> ProcessId -> SchedulerState -> STM Int
addMonitoring monitorRef@(MkMonitorReference _ target) owner schedulerState =
  if target == owner
    then pure 0
    else do
      pt <- readTVar (schedulerState ^. processTable)
      case Map.lookup target pt of
        Just targetProcessInfo ->
          do
            (_, targetState) <- readTVar (targetProcessInfo ^. processState)
            check
              ( targetState == ProcessShuttingDown
                  || targetState == ProcessBusyReceiving
                  || targetState == ProcessIdle
              )
            if targetState /= ProcessShuttingDown
              then insertMonitoringReference >> pure 1
              else processAlreadyDead >> pure 2
        Nothing ->
          processAlreadyDead >> pure 3
  where
    insertMonitoringReference =
      modifyTVar'
        (schedulerState ^. processMonitors)
        (Set.insert (monitorRef, owner))
    processAlreadyDead = do
      let processDownMessage =
            ProcessDown monitorRef (ExitOtherProcessNotRunning target) target
      wasEnqueued <-
        enqueueMessageOtherProcess
          owner
          (toMessage processDownMessage)
          schedulerState
      check wasEnqueued

triggerAndRemoveMonitor ::
  ProcessId -> ShutdownReason -> SchedulerState -> STM [ProcessId]
triggerAndRemoveMonitor downPid reason schedulerState = do
  -- remove the monitor entries that the downPid process owned:
  modifyTVar'
    (schedulerState ^. processMonitors)
    (Set.filter (\(_, downPid') -> downPid' /= downPid))
  -- now send the process down message and remove the entries that monitor
  -- the downPid process:
  monRefs <- readTVar (schedulerState ^. processMonitors)
  catMaybes <$> traverse go (toList monRefs)
  where
    go (mr, owner) =
      if view monitoredProcess mr == downPid
        then do
          let processDownMessage = ProcessDown mr reason downPid
          wasEnqueued <-
            enqueueMessageOtherProcess
              owner
              (toMessage processDownMessage)
              schedulerState
          removeMonitoring mr schedulerState
          pure $ if wasEnqueued then Nothing else Just owner
        else pure Nothing

removeMonitoring :: MonitorReference -> SchedulerState -> STM ()
removeMonitoring monitorRef schedulerState =
  modifyTVar'
    (schedulerState ^. processMonitors)
    (Set.filter (\(ref, _) -> ref /= monitorRef))

-- * Process Implementation

instance Show ProcessInfo where
  show p = "process info: " ++ show (p ^. processId)

-- | Create a new 'ProcessInfo'
newProcessInfo :: HasCallStack => ProcessId -> ProcessTitle -> STM ProcessInfo
newProcessInfo a t =
  ProcessInfo a t <$> newTVar (mempty, ProcessBooting) <*> newTVar def <*> newTVar def

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
withNewSchedulerState :: HasCallStack => Eff BaseEffects () -> Eff LoggingAndIo ()
withNewSchedulerState mainProcessAction =
  Safe.bracketWithError
    (lift (atomically newSchedulerState))
    ( \exceptions schedulerState -> do
        traverse_
          ( logError
              ("scheduler setup crashed with: ")
              . packLogMsg
              . Safe.displayException
          )
          exceptions
        logDebug "scheduler cleanup begin"
        runReader schedulerState tearDownScheduler
    )
    ( \schedulerState -> do
        logDebug "scheduler loop entered"
        x <- runReader schedulerState mainProcessAction
        logDebug "scheduler loop returned"
        return x
    )
  where
    tearDownScheduler :: Eff BaseEffects ()
    tearDownScheduler = do
      schedulerState <- getSchedulerState
      let cancelTableVar = schedulerState ^. processCancellationTable
      -- cancel all processes
      allProcesses <-
        lift
          (atomically (readTVar cancelTableVar <* writeTVar cancelTableVar def))
      logNotice
        "cancelling processes: "
        (packLogMsg (show (toListOf (ifolded . asIndex) allProcesses)))
      void
        ( liftBaseWith
            ( \runS ->
                timeout
                  5_000_000
                  ( Async.mapConcurrently
                      ( \a -> do
                          Async.cancel a
                          runS
                            ( logNotice
                                "process cancelled: "
                                (packLogMsg (show (asyncThreadId a)))
                            )
                      )
                      allProcesses
                      >> runS (logNotice "all processes cancelled")
                  )
            )
        )

-- | The concrete list of 'Eff'ects of processes compatible with this scheduler.
-- This builds upon 'BaseEffects'.
--
-- @since 0.25.0
type SafeEffects = SafeProcesses BaseEffects

-- | The 'Eff'ects for interruptable, concurrent processes, scheduled via 'forkIO'.
--
-- @since 0.25.0
type Effects = Processes BaseEffects

-- | Type class constraint to indicate that an effect union contains the
-- effects required by every process and the scheduler implementation itself.
--
-- @since 0.25.0
type HasBaseEffects r = (HasCallStack, Lifted IO r, BaseEffects <:: r)

-- | The concrete list of 'Eff'ects for this scheduler implementation.
--
-- @since 0.25.0
type BaseEffects = Reader SchedulerState : LoggingAndIo

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'BaseEffects' effect. All logging is sent to standard output.
defaultMain :: HasCallStack => Eff Effects () -> IO ()
defaultMain e = do
  lw <- consoleLogWriter
  defaultMainWithLogWriter lw e

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'BaseEffects' effect. All logging is sent to standard output.
defaultMainWithLogWriter ::
  HasCallStack => LogWriter -> Eff Effects () -> IO ()
defaultMainWithLogWriter lw =
  runLift . withLogging lw . withAsyncLogWriter (1024 :: Int) . schedule

-- ** Process Execution

handleProcess ::
  (HasCallStack) =>
  ProcessInfo ->
  Eff SafeEffects ShutdownReason ->
  Eff BaseEffects ShutdownReason
handleProcess myProcessInfo actionToRun =
  fix
    (handle_relay' singleStep (\er _nextRef -> setMyProcessState ProcessShuttingDown >> return er))
    actionToRun
    0
  where
    singleStep ::
      (Eff SafeEffects xx -> (Int -> Eff BaseEffects ShutdownReason)) ->
      Arrs SafeEffects x xx ->
      Process BaseEffects x ->
      (Int -> Eff BaseEffects ShutdownReason)
    singleStep k q p !nextRef =
      stepProcessInterpreter
        nextRef
        p
        (\nextNextRef x -> k (qApp q x) nextNextRef)
        return
    myPid = myProcessInfo ^. processId
    myProcessStateVar = myProcessInfo ^. processState
    setMyProcessState = lift . atomically . setMyProcessStateSTM
    -- DEBUG variant:
    -- setMyProcessState st = do
    --  oldSt <- lift (atomically (readTVar myProcessStateVar <* setMyProcessStateSTM st))
    --  logDebug ("state change: "<> show oldSt <> " -> " <> show st)
    setMyProcessStateSTM = modifyTVar myProcessStateVar . set _2
    setMyProcessDetailsSTM = modifyTVar myProcessStateVar . set _1
    myMessageQVar = myProcessInfo ^. messageQ
    kontinueWith ::
      forall s v a.
      HasCallStack =>
      (s -> Arr BaseEffects v a) ->
      (s -> Arr BaseEffects v a)
    kontinueWith kontinue !nextRef !result = do
      setMyProcessState ProcessIdle
      lift yield
      kontinue nextRef result
    diskontinueWith ::
      forall a.
      HasCallStack =>
      Arr BaseEffects ShutdownReason a ->
      Arr BaseEffects ShutdownReason a
    diskontinueWith diskontinue !reason = do
      setMyProcessState ProcessShuttingDown
      diskontinue reason
    stepProcessInterpreter ::
      forall v a.
      HasCallStack =>
      Int ->
      Process BaseEffects v ->
      (Int -> Arr BaseEffects v a) ->
      Arr BaseEffects ShutdownReason a ->
      Eff BaseEffects a
    stepProcessInterpreter !nextRef !request kontinue diskontinue =
      tryTakeNextShutdownRequest
        >>= maybe
          noShutdownRequested
          (either onShutdownRequested onInterruptRequested . fromInterruptOrShutdown)
      where
        -- handle process shutdown requests:
        --   1. take process exit reason
        --   2. set process state to ProcessShuttingDown
        --   3. apply kontinue to (Right Interrupted)
        --

        tryTakeNextShutdownRequest =
          lift (atomically (tryTakeNextShutdownRequestSTM myMessageQVar))
        onShutdownRequested shutdownRequest = do
          setMyProcessState ProcessShuttingDown
          interpretRequestAfterShutdownRequest
            (diskontinueWith diskontinue)
            shutdownRequest
            request
        onInterruptRequested interruptRequest = do
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
    interpretRequestAfterShutdownRequest ::
      forall v a.
      HasCallStack =>
      Arr BaseEffects ShutdownReason a ->
      ShutdownReason ->
      Process BaseEffects v ->
      Eff BaseEffects a
    interpretRequestAfterShutdownRequest diskontinue shutdownRequest = \case
      SendMessage _ _ -> diskontinue shutdownRequest
      SendInterrupt _ _ -> diskontinue shutdownRequest
      SendShutdown toPid r ->
        if toPid == myPid then diskontinue r else diskontinue shutdownRequest
      Spawn _ _ -> diskontinue shutdownRequest
      SpawnLink _ _ -> diskontinue shutdownRequest
      ReceiveSelectedMessage _ -> diskontinue shutdownRequest
      FlushMessages -> diskontinue shutdownRequest
      SelfPid -> diskontinue shutdownRequest
      MakeReference -> diskontinue shutdownRequest
      YieldProcess -> diskontinue shutdownRequest
      Delay _ -> diskontinue shutdownRequest
      Shutdown r -> diskontinue r
      GetProcessState _ -> diskontinue shutdownRequest
      UpdateProcessDetails _ -> diskontinue shutdownRequest
      Monitor _ -> diskontinue shutdownRequest
      Demonitor _ -> diskontinue shutdownRequest
      Link _ -> diskontinue shutdownRequest
      Unlink _ -> diskontinue shutdownRequest
    interpretRequestAfterInterruptRequest ::
      forall v a.
      HasCallStack =>
      Arr BaseEffects v a ->
      Arr BaseEffects ShutdownReason a ->
      InterruptReason ->
      Process BaseEffects v ->
      Eff BaseEffects a
    interpretRequestAfterInterruptRequest kontinue diskontinue interruptRequest =
      \case
        SendMessage _ _ -> kontinue (Interrupted interruptRequest)
        SendInterrupt _ _ -> kontinue (Interrupted interruptRequest)
        SendShutdown toPid r ->
          if toPid == myPid
            then diskontinue r
            else kontinue (Interrupted interruptRequest)
        Spawn _ _ -> kontinue (Interrupted interruptRequest)
        SpawnLink _ _ -> kontinue (Interrupted interruptRequest)
        ReceiveSelectedMessage _ -> kontinue (Interrupted interruptRequest)
        FlushMessages -> kontinue (Interrupted interruptRequest)
        SelfPid -> kontinue (Interrupted interruptRequest)
        MakeReference -> kontinue (Interrupted interruptRequest)
        YieldProcess -> kontinue (Interrupted interruptRequest)
        Delay _ -> kontinue (Interrupted interruptRequest)
        Shutdown r -> diskontinue r
        GetProcessState _ -> kontinue (Interrupted interruptRequest)
        UpdateProcessDetails _ -> kontinue (Interrupted interruptRequest)
        Monitor _ -> kontinue (Interrupted interruptRequest)
        Demonitor _ -> kontinue (Interrupted interruptRequest)
        Link _ -> kontinue (Interrupted interruptRequest)
        Unlink _ -> kontinue (Interrupted interruptRequest)
    interpretRequest ::
      forall v a.
      HasCallStack =>
      (Int -> Arr BaseEffects v a) ->
      Arr BaseEffects ShutdownReason a ->
      Int ->
      Process BaseEffects v ->
      Eff BaseEffects a
    interpretRequest kontinue diskontinue nextRef = \case
      SendMessage toPid msg ->
        void (interpretSend toPid msg) >>= kontinue nextRef . ResumeWith
      SendInterrupt toPid msg ->
        if toPid == myPid
          then kontinue nextRef (Interrupted msg)
          else
            interpretSendShutdownOrInterrupt toPid (InterruptOrShutdown (Right msg))
              >>= kontinue nextRef
              . ResumeWith
      SendShutdown toPid msg ->
        if toPid == myPid
          then diskontinue msg
          else
            interpretSendShutdownOrInterrupt toPid (InterruptOrShutdown (Left msg))
              >>= kontinue nextRef
              . ResumeWith
      Spawn title child ->
        spawnNewProcess Nothing title child >>= kontinue nextRef . ResumeWith . fst
      SpawnLink title child ->
        spawnNewProcess (Just myProcessInfo) title child
          >>= kontinue nextRef
          . ResumeWith
          . fst
      ReceiveSelectedMessage f -> do
        recvRes <- interpretReceive f
        either diskontinue (kontinue nextRef) recvRes
      Shutdown r -> diskontinue r
      FlushMessages -> interpretFlush >>= kontinue nextRef
      SelfPid -> kontinue nextRef (ResumeWith myPid)
      MakeReference -> kontinue (nextRef + 1) (ResumeWith nextRef)
      YieldProcess -> kontinue nextRef (ResumeWith ())
      Delay t ->
        interpretDelay t >>= either diskontinue (kontinue nextRef)
      GetProcessState toPid ->
        interpretGetProcessState toPid >>= kontinue nextRef . ResumeWith
      UpdateProcessDetails d ->
        interpretUpdateDetails d >>= kontinue nextRef . ResumeWith
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
          monitoringReference <- lift (atomically (nextMonitorReference target schedulerState))
          void $ lift (atomically (addMonitoring monitoringReference myPid schedulerState))
          return monitoringReference
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
              ( \toProcInfo ->
                  modifyTVar' (toProcInfo ^. processLinks) (Set.delete myPid)
              )
              (procInfo ^. at toPid)
            modifyTVar' (myProcessInfo ^. processLinks) (Set.delete toPid)
        interpretGetProcessState !toPid = do
          setMyProcessState ProcessBusy
          schedulerState <- getSchedulerState
          let procInfoVar = schedulerState ^. processTable
          initPd <-
            if toPid == 1
              then Just <$> lift (renderSchedulerState schedulerState)
              else pure Nothing
          lift $ atomically $ do
            procInfoTable <- readTVar procInfoVar
            traverse
              ( \toProcInfo -> do
                  (pDetails, pState) <- readTVar (toProcInfo ^. processState)
                  let pDetails' = fromMaybe pDetails initPd
                  return (toProcInfo ^. processTitle, pDetails', pState)
              )
              (procInfoTable ^. at toPid)
        interpretUpdateDetails !td = do
          setMyProcessState ProcessBusyUpdatingDetails
          lift $ atomically $ setMyProcessDetailsSTM td
        interpretLink !toPid = do
          setMyProcessState ProcessBusyLinking
          schedulerState <- getSchedulerState
          let procInfoVar = schedulerState ^. processTable
          lift $ atomically $ do
            procInfoTable <- readTVar procInfoVar
            case procInfoTable ^. at toPid of
              Just toProcInfo -> do
                modifyTVar' (toProcInfo ^. processLinks) (Set.insert myPid)
                modifyTVar' (myProcessInfo ^. processLinks) (Set.insert toPid)
                return (Right ())
              Nothing -> return (Left (LinkedProcessCrashed toPid))
        interpretSend !toPid msg =
          setMyProcessState ProcessBusySending
            *> getSchedulerState
            >>= lift
            . atomically
            . enqueueMessageOtherProcess toPid msg
        interpretSendShutdownOrInterrupt !toPid !msg =
          setMyProcessState
            ( either
                (const ProcessBusySendingShutdown)
                (const ProcessBusySendingInterrupt)
                (fromInterruptOrShutdown msg)
            )
            *> getSchedulerState
            >>= lift
            . atomically
            . enqueueShutdownRequest toPid msg
        interpretFlush :: Eff BaseEffects (ResumeProcess [Message])
        interpretFlush = do
          setMyProcessState ProcessBusyReceiving
          lift $ atomically $ do
            myMessageQ <- readTVar myMessageQVar
            modifyTVar' myMessageQVar (incomingMessages .~ Seq.Empty)
            return (ResumeWith (toList (myMessageQ ^. incomingMessages)))
        interpretDelay ::
          Timeout ->
          Eff BaseEffects (Either ShutdownReason (ResumeProcess ()))
        interpretDelay (TimeoutMicros t) = do
          setMyProcessState ProcessBusySleeping
          lift $ do
            timeoutTVar <- newTVarIO False
            newDelayThreadId <- forkIO $ do
              atomically $ writeTVar timeoutTVar False
              threadDelay t
              atomically $ writeTVar timeoutTVar True
            (elapsed, res) <- atomically $ do
              myMessageQ <- readTVar myMessageQVar
              case myMessageQ ^. shutdownRequests of
                Nothing -> do
                  done <- readTVar timeoutTVar
                  unless done retry
                  return (True, Right (ResumeWith ()))
                Just shutdownRequest -> do
                  modifyTVar' myMessageQVar (shutdownRequests .~ Nothing)
                  case fromInterruptOrShutdown shutdownRequest of
                    Left sr -> return (False, Left sr)
                    Right ir -> return (False, Right (Interrupted ir))
            unless elapsed (killThread newDelayThreadId)
            return res
        interpretReceive ::
          MessageSelector b ->
          Eff BaseEffects (Either ShutdownReason (ResumeProcess b))
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
                case fromInterruptOrShutdown shutdownRequest of
                  Left sr -> return (Left sr)
                  Right ir -> return (Right (Interrupted ir))
          where
            partitionMessages Seq.Empty _acc = Nothing
            partitionMessages (m :<| msgRest) acc =
              maybe
                (partitionMessages msgRest (acc :|> m))
                (\res -> Just (res, acc Seq.>< msgRest))
                (runMessageSelector f m)

-- | This is the main entry point to running a message passing concurrency
-- application. This function takes a 'Process' on top of the 'BaseEffects'
-- effect for concurrent logging.
schedule :: (HasCallStack) => Eff Effects () -> Eff LoggingAndIo ()
schedule procEff =
  liftBaseWith
    ( \runS ->
        Async.withAsync
          ( runS $ withNewSchedulerState $ do
              (_, mainProcAsync) <- spawnNewProcess Nothing (toProcessTitle "init") $ do
                logNotice "++++++++ main process started ++++++++"
                provideInterruptsShutdown procEff
                logNotice "++++++++ main process returned ++++++++"
              lift (void (Async.wait mainProcAsync))
          )
          ( \ast -> runS $ do
              a <- restoreM ast
              void $ lift (Async.wait a)
          )
    )
    >>= restoreM

spawnNewProcess ::
  (HasCallStack) =>
  Maybe ProcessInfo ->
  ProcessTitle ->
  Eff SafeEffects () ->
  Eff BaseEffects (ProcessId, Async ShutdownReason)
spawnNewProcess mLinkedParent title mfa = do
  schedulerState <- getSchedulerState
  procInfo <- allocateProcInfo schedulerState
  traverse_ (linkToParent procInfo) mLinkedParent
  procAsync <- doForkProc procInfo schedulerState
  return (procInfo ^. processId, procAsync)
  where
    allocateProcInfo schedulerState =
      lift
        ( atomically
            ( do
                let nextPidVar = schedulerState ^. nextPid
                    processInfoVar = schedulerState ^. processTable
                pid <- readTVar nextPidVar
                modifyTVar' nextPidVar (+ 1)
                procInfo <- newProcessInfo pid title
                modifyTVar' processInfoVar (at pid ?~ procInfo)
                return procInfo
            )
        )
    linkToParent toProcInfo parent = do
      let toPid = toProcInfo ^. processId
          parentPid = parent ^. processId
      logDebug "linked to new child: " toPid
      lift $ atomically $ do
        modifyTVar' (toProcInfo ^. processLinks) (Set.insert parentPid)
        modifyTVar' (parent ^. processLinks) (Set.insert toPid)
    logAppendProcInfo pid =
      let addProcessId =
            over
              logEventProcessId
              (maybe (Just (T.pack (show title ++ show pid))) Just)
       in censorLogs addProcessId
    triggerProcessLinksAndMonitors ::
      ProcessId -> ShutdownReason -> TVar (Set ProcessId) -> Eff BaseEffects ()
    triggerProcessLinksAndMonitors !pid !reason !linkSetVar = do
      schedulerState <- getSchedulerState
      let exitSeverity = toExitSeverity reason
          sendIt !linkedPid = do
            let msg = InterruptOrShutdown (Right (LinkedProcessCrashed pid))
            lift $ atomically $ do
              procInfoTable <- readTVar (schedulerState ^. processTable)
              let mLinkedProcInfo = procInfoTable ^? ix linkedPid
              case mLinkedProcInfo of
                Nothing -> return (Left linkedPid)
                Just linkedProcInfo ->
                  let linkedMsgQVar = linkedProcInfo ^. messageQ
                      linkedLinkSetVar = linkedProcInfo ^. processLinks
                   in do
                        linkedLinkSet <- readTVar linkedLinkSetVar
                        if Set.member pid linkedLinkSet
                          then do
                            writeTVar
                              linkedLinkSetVar
                              (Set.delete pid linkedLinkSet)
                            if exitSeverity == Crash
                              then do
                                modifyTVar'
                                  linkedMsgQVar
                                  (shutdownRequests ?~ msg)
                                return (Right (Left linkedPid))
                              else return (Right (Right linkedPid))
                          else return (Left linkedPid)
      downMessageSendResults <- lift . atomically $ triggerAndRemoveMonitor pid reason schedulerState
      linkedPids <-
        lift
          ( atomically
              ( do
                  linkSet <- readTVar linkSetVar
                  writeTVar linkSetVar def
                  return linkSet
              )
          )
      res <- traverse sendIt (toList linkedPids)
      traverse_
        ( either
            (logNotice "linked process not found: ")
            ( either
                (logWarning "process crashed, interrupting linked process: ")
                (logDebug "linked process exited peacefully, not sending shutdown to linked process: ")
            )
        )
        res
      unless (null downMessageSendResults) $
        traverse_
          (logWarning "failed to enqueue monitor down messages for: ")
          downMessageSendResults
    doForkProc ::
      ProcessInfo ->
      SchedulerState ->
      Eff BaseEffects (Async ShutdownReason)
    doForkProc procInfo schedulerState =
      control
        ( \inScheduler -> do
            let cancellationsVar = schedulerState ^. processCancellationTable
                processInfoVar = schedulerState ^. processTable
                pid = procInfo ^. processId
            procAsync <-
              Async.async
                ( inScheduler
                    ( logAppendProcInfo
                        pid
                        ( Safe.bracketWithError
                            (logDebug "enter process")
                            ( \mExc () -> do
                                lift
                                  ( atomically
                                      ( do
                                          modifyTVar' processInfoVar (at pid .~ Nothing)
                                          modifyTVar' cancellationsVar (at pid .~ Nothing)
                                      )
                                  )
                                traverse_
                                  ( \exc ->
                                      logExitAndTriggerLinksAndMonitors
                                        (exitReasonFromException exc)
                                        pid
                                  )
                                  mExc
                            )
                            ( const
                                ( do
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
          Just Async.AsyncCancelled -> ExitProcessCancelled Nothing
          Nothing ->
            ExitUnhandledError
              ( packLogMsg "runtime exception: "
                  <> packLogMsg (prettyCallStack callStack)
                  <> packLogMsg " "
                  <> packLogMsg (Safe.displayException exc)
              )
        logExitAndTriggerLinksAndMonitors reason pid = do
          (_, currentState) <-
            lift
              ( atomically
                  ( readTVar (procInfo ^. processState)
                      <* modifyTVar' (procInfo ^. processState) (_2 .~ ProcessShuttingDown)
                  )
              )
          when
            (currentState /= ProcessShuttingDown)
            (logNotice "aborted in state: " currentState " because: " reason)
          triggerProcessLinksAndMonitors pid reason (procInfo ^. processLinks)
          logProcessExit reason
          return reason

-- * Scheduler Accessor

getSchedulerState :: HasBaseEffects r => Eff r SchedulerState
getSchedulerState = ask

enqueueMessageOtherProcess ::
  HasCallStack => ProcessId -> Message -> SchedulerState -> STM Bool
enqueueMessageOtherProcess toPid msg schedulerState =
  view (at toPid) <$> readTVar (schedulerState ^. processTable)
    >>= maybe
      (return False)
      ( \toProcessTable -> do
          modifyTVar' (toProcessTable ^. messageQ) (incomingMessages %~ (:|> msg))
          return True
      )

enqueueShutdownRequest ::
  HasCallStack => ProcessId -> InterruptOrShutdown -> SchedulerState -> STM ()
enqueueShutdownRequest toPid msg schedulerState =
  view (at toPid) <$> readTVar (schedulerState ^. processTable)
    >>= maybe
      (return ())
      ( \toProcessTable -> do
          modifyTVar' (toProcessTable ^. messageQ) (shutdownRequests ?~ msg)
          return ()
      )
