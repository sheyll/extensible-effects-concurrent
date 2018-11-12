-- | A coroutine based, single threaded scheduler for 'Process'es.
module Control.Eff.Concurrent.Process.SingleThreadedScheduler
  ( scheduleM
  , schedulePure
  , scheduleIO
  , scheduleMonadIOEff
  , scheduleIOWithLogging
  , defaultMain
  , singleThreadedIoScheduler
  )
where

import           Control.Concurrent             ( yield )
import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Extend
import           Control.Eff.Concurrent.Process
import           Control.Eff.Log
import           Control.Lens            hiding ( (|>)
                                                , Empty
                                                )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.IO.Class
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           GHC.Stack
import           Data.Kind                      ( )
import           Data.Dynamic
import           Data.Foldable
import qualified Control.Monad.State.Strict    as State

-- -----------------------------------------------------------------------------
--  STS
-- -----------------------------------------------------------------------------

data STS r m = STS
  { _nextPid :: !ProcessId
  , _nextRef :: !Int
  , _msgQs :: !(Map.Map ProcessId (Seq Dynamic))
  , _monitors :: !(Set.Set (MonitorReference, ProcessId))
  , _runEff :: (forall a . Eff r a -> m a)
  , _yieldEff :: m ()
  }

initStsMainProcess :: (forall a . Eff r a -> m a) -> m () -> STS r m
initStsMainProcess = STS 1 0 (Map.singleton 0 Seq.empty) Set.empty

makeLenses ''STS

dropMsgQ :: ProcessId -> STS r m -> STS r m
dropMsgQ pid = msgQs . at pid .~ Nothing

getProcessState :: ProcessId -> STS r m -> Maybe ProcessState
getProcessState pid sts = toPS <$> sts ^. msgQs . at pid
 where
  toPS Empty     = ProcessIdle
  toPS (_ :<| _) = ProcessBusy -- TODO get more detailed state

incRef :: STS r m -> (Int, STS r m)
incRef sts = (sts ^. nextRef, sts & nextRef %~ (+ 1))

enqueueMsg :: ProcessId -> Dynamic -> STS r m -> STS r m
enqueueMsg toPid msg = msgQs . at toPid . _Just %~ (:|> msg)

newProcessQ :: STS r m -> (ProcessId, STS r m)
newProcessQ sts =
  ( sts ^. nextPid
  , sts & nextPid %~ (+ 1) & msgQs . at (sts ^. nextPid) ?~ Seq.empty
  )

receiveMsg
  :: ProcessId -> MessageSelector a -> STS m r -> Maybe (Maybe (a, STS m r))
receiveMsg pid messageSelector sts = case sts ^. msgQs . at pid of
  Nothing   -> Nothing
  Just msgQ -> Just $ case partitionMessages msgQ Empty of
    Nothing -> Nothing
    Just (result, otherMessages) ->
      Just (result, sts & msgQs . at pid . _Just .~ otherMessages)
 where
  partitionMessages Empty           _acc = Nothing
  partitionMessages (m :<| msgRest) acc  = maybe
    (partitionMessages msgRest (acc :|> m))
    (\res -> Just (res, acc Seq.>< msgRest))
    (runMessageSelector messageSelector m)

-- | Add monitor: If the process is dead, enqueue a ProcessDown message into the
-- owners message queue
addMonitoring
  :: ProcessId -> ProcessId -> STS m r -> (MonitorReference, STS m r)
addMonitoring target owner sts = flip State.runState sts $ do
  mi <- State.state incRef
  let mref = MonitorReference mi target
  when (target /= owner) $ do
    pt <- use msgQs
    if Map.member target pt
      then monitors %= Set.insert (mref, owner)
      else
        let pdown =
              (ProcessDown mref (SomeExitReason (ProcessNotRunning target)))
        in  State.modify' (enqueueMsg owner (toDyn pdown))
  return mref

removeMonitoring :: MonitorReference -> STS m r -> STS m r
removeMonitoring mref = monitors %~ Set.filter (\(ref, _) -> ref /= mref)

triggerAndRemoveMonitor :: ProcessId -> SomeExitReason -> STS m r -> STS m r
triggerAndRemoveMonitor downPid reason sts = flip State.execState sts $ do
  monRefs <- use monitors
  traverse_ go monRefs
 where
  go (mr, owner) = when
    (monitoredProcess mr == downPid)
    (let pdown = ProcessDown mr reason
     in  State.modify' (enqueueMsg owner (toDyn pdown) . removeMonitoring mr)
    )

kontinue :: STS r m -> (ResumeProcess a -> Eff r a1) -> a -> m a1
kontinue sts k x = (sts ^. runEff) (k (ResumeWith x))

diskontinue :: STS r m -> (ResumeProcess v -> Eff r a) -> InterruptReason -> m a
diskontinue sts k e = (sts ^. runEff) (k (Interrupted e))

-- -----------------------------------------------------------------------------
--  Meat Of The Thing
-- -----------------------------------------------------------------------------

-- | Like 'schedule' but /pure/. The @yield@ effect is just @return ()@.
-- @schedulePure == runIdentity . 'scheduleM' (Identity . run)  (return ())@
--
-- @since 0.3.0.2
schedulePure
  :: Eff (InterruptableProcess '[Logs LogMessage]) a
  -> Either (ExitReason 'NoRecovery) a
schedulePure e = run (scheduleM ignoreLogs (return ()) e)

-- | Invoke 'schedule' with @lift 'Control.Concurrent.yield'@ as yield effect.
-- @scheduleIO runEff == 'scheduleM' (runLift . runEff) (liftIO 'yield')@
--
-- @since 0.4.0.0
scheduleIO
  :: MonadIO m
  => (forall b . Eff r b -> Eff '[Lift m] b)
  -> Eff (InterruptableProcess r) a
  -> m (Either (ExitReason 'NoRecovery) a)
scheduleIO r = scheduleM (runLift . r) (liftIO yield)

-- | Invoke 'schedule' with @lift 'Control.Concurrent.yield'@ as yield effect.
-- @scheduleMonadIOEff == 'scheduleM' id (liftIO 'yield')@
--
-- @since 0.3.0.2
scheduleMonadIOEff
  :: MonadIO (Eff r)
  => Eff (InterruptableProcess r) a
  -> Eff r (Either (ExitReason 'NoRecovery) a)
scheduleMonadIOEff = -- schedule (lift yield)
  scheduleM id (liftIO yield)

-- | Run processes that have the 'Logs' and the 'Lift' effects.
-- The user must provide a log handler function.
--
-- Log messages are evaluated strict.
--
-- @scheduleIOWithLogging == 'run' . 'captureLogs' . 'schedule' (return ())@
--
-- @since 0.4.0.0
scheduleIOWithLogging
  :: (NFData l)
  => LogWriter l IO
  -> Eff (InterruptableProcess '[Logs l, LogWriterReader l IO, Lift IO]) a
  -> IO (Either (ExitReason 'NoRecovery) a)
scheduleIOWithLogging h = scheduleIO (writeLogs h)

-- | Handle the 'Process' effect, as well as all lower effects using an effect handler function.
--
-- Execute the __main__ 'Process' and all the other processes 'spawn'ed by it in the
-- current thread concurrently, using a co-routine based, round-robin
-- scheduler. If a process exits with 'exitNormally', 'exitWithError',
-- 'raiseError' or is killed by another process @Left ...@ is returned.
-- Otherwise, the result will be wrapped in a @Right@.
--
-- Every time a process _yields_ the effects are evaluated down to the a value
-- of type @m (Either String a)@.
--
-- If the evaluator function runs the action down e.g. @IO@ this might improve
-- memory consumption, for long running services, with processes that loop
-- endlessly.
--
-- @since 0.4.0.0
scheduleM
  :: forall m r a
   . Monad m
  => (forall b . Eff r b -> m b)
  -> m () -- ^ An that performs a __yield__ w.r.t. the underlying effect
  --  @r@. E.g. if @Lift IO@ is present, this might be:
  --  @lift 'Control.Concurrent.yield'.
  -> Eff (InterruptableProcess r) a
  -> m (Either (ExitReason 'NoRecovery) a)
scheduleM r y e = do
  c <- runAsCoroutinePure r (provideInterruptsShutdown e)
  handleProcess (initStsMainProcess r y) (Seq.singleton (c, 0))

-- | Internal data structure that is part of the coroutine based scheduler
-- implementation.
data OnYield r a where
  OnYield :: (ResumeProcess () -> Eff r (OnYield r a))
         -> OnYield r a
  OnSelf :: (ResumeProcess ProcessId -> Eff r (OnYield r a))
         -> OnYield r a
  OnSpawn :: Bool
          -> Eff (Process r ': r) ()
          -> (ResumeProcess ProcessId -> Eff r (OnYield r a))
          -> OnYield r a
  OnDone :: !a -> OnYield r a
  OnShutdown :: ExitReason 'NoRecovery -> OnYield r a
  OnInterrupt :: ExitReason 'Recoverable
                -> (ResumeProcess b -> Eff r (OnYield r a))
                -> OnYield r a
  OnSend :: !ProcessId -> !Dynamic
         -> (ResumeProcess () -> Eff r (OnYield r a))
         -> OnYield r a
  OnRecv :: MessageSelector b -> (ResumeProcess b -> Eff r (OnYield r a))
         -> OnYield r a
  OnGetProcessState
         :: ProcessId
         -> (ResumeProcess (Maybe ProcessState) -> Eff r (OnYield r a))
         -> OnYield r a
  OnSendShutdown :: !ProcessId -> ExitReason 'NoRecovery
                    -> (ResumeProcess () -> Eff r (OnYield r a)) -> OnYield r a
  OnSendInterrupt :: !ProcessId -> ExitReason 'Recoverable
                    -> (ResumeProcess () -> Eff r (OnYield r a)) -> OnYield r a
  OnMakeReference :: (ResumeProcess Int -> Eff r (OnYield r a)) -> OnYield r a
  OnMonitor
    :: ProcessId
    -> (ResumeProcess MonitorReference -> Eff r (OnYield r a))
    -> OnYield r a
  OnDemonitor
    :: MonitorReference
    -> (ResumeProcess () -> Eff r (OnYield r a))
    -> OnYield r a


runAsCoroutinePure
  :: forall v r m
   . Monad m
  => (forall a . Eff r a -> m a)
  -> Eff (ConsProcess r) v
  -> m (OnYield r v)
runAsCoroutinePure r = r . handle_relay (return . OnDone) cont
 where
  cont :: Process r x -> (x -> Eff r (OnYield r v)) -> Eff r (OnYield r v)
  cont YieldProcess                 k  = return (OnYield k)
  cont SelfPid                      k  = return (OnSelf k)
  cont (Spawn     e               ) k  = return (OnSpawn False e k)
  cont (SpawnLink e               ) k  = return (OnSpawn True e k)
  cont (Shutdown  !sr             ) _k = return (OnShutdown sr)
  cont (SendMessage !tp !msg      ) k  = return (OnSend tp msg k)
  cont (ReceiveSelectedMessage f  ) k  = return (OnRecv f k)
  cont (GetProcessState        !tp) k  = return (OnGetProcessState tp k)
  cont (SendInterrupt !tp  !er    ) k  = return (OnSendInterrupt tp er k)
  cont (SendShutdown  !pid !sr    ) k  = return (OnSendShutdown pid sr k)
  cont MakeReference                k  = return (OnMakeReference k)
  cont (Monitor   !pid)             k  = return (OnMonitor pid k)
  cont (Demonitor !ref)             k  = return (OnDemonitor ref k)
  cont (Link _) k =
    return (OnInterrupt (ProcessError "Not Yet Implemented: Link") k)
  cont (Unlink _) k =
    return (OnInterrupt (ProcessError "Not Yet Implemented: Unlink") k)


-- | Internal 'Process' handler function.
handleProcess
  :: Monad m
  => STS r m
  -> Seq (OnYield r finalResult, ProcessId)
  -> m (Either (ExitReason 'NoRecovery) finalResult)
handleProcess _sts Empty =
  return $ Left (NotRecovered (ProcessError "no main process"))

handleProcess sts allProcs@((!processState, !pid) :<| rest)
  = let
      handleExit res = if pid == 0
        then return res
        else handleProcess
          (dropMsgQ
            pid
            (triggerAndRemoveMonitor
              pid
              (either SomeExitReason (const (SomeExitReason ExitNormally)) res)
              sts
            )
          )
          rest
    in
      case processState of
        OnDone     r    -> handleExit (Right r)

        OnShutdown e    -> handleExit (Left e)

        OnInterrupt e k -> do
          nextK <- diskontinue sts k e
          handleProcess sts (rest :|> (nextK, pid))

        OnSendInterrupt targetPid sr k -> do
          let allButTarget =
                Seq.filter (\(_, e) -> e /= pid && e /= targetPid) allProcs
              targets = Seq.filter (\(_, e) -> e == targetPid) allProcs
              suicide = targetPid == pid
          if suicide
            then do
              nextK <- diskontinue sts k sr
              handleProcess sts (rest :|> (nextK, pid))
            else do
              let deliverTheGoodNews (targetState, tPid) = do
                    nextTargetState <- case targetState of
                      OnSendInterrupt _ _ tk -> tk (Interrupted sr)
                      OnSendShutdown  _ _ tk -> tk (Interrupted sr)
                      OnYield tk             -> tk (Interrupted sr)
                      OnSelf  tk             -> tk (Interrupted sr)
                      OnSend _ _ tk          -> tk (Interrupted sr)
                      OnRecv _ tk            -> tk (Interrupted sr)
                      OnSpawn _ _ tk         -> tk (Interrupted sr)
                      OnDone x               -> return (OnDone x)
                      OnGetProcessState _ tk -> tk (Interrupted sr)
                      OnShutdown sr'         -> return (OnShutdown sr')
                      OnInterrupt er tk      -> tk (Interrupted er)
                      OnMakeReference tk     -> tk (Interrupted sr)
                      OnMonitor   _ tk       -> tk (Interrupted sr)
                      OnDemonitor _ tk       -> tk (Interrupted sr)
                    return (nextTargetState, tPid)
              nextTargets <- _runEff sts $ traverse deliverTheGoodNews targets
              nextK       <- kontinue sts k ()
              handleProcess
                sts
                (allButTarget Seq.>< (nextTargets :|> (nextK, pid)))

        OnSendShutdown targetPid sr k -> do
          let allButTarget =
                Seq.filter (\(_, e) -> e /= pid && e /= targetPid) allProcs
              targets = Seq.filter (\(_, e) -> e == targetPid) allProcs
              suicide = targetPid == pid
          if suicide
            then handleExit (Left sr)
            else do
              let deliverTheGoodNews (targetState, tPid) = do
                    nextTargetState <- case targetState of
                      OnSendInterrupt _ _ _tk -> return (OnShutdown sr)
                      OnSendShutdown  _ _ _tk -> return (OnShutdown sr)
                      OnYield _tk             -> return (OnShutdown sr)
                      OnSelf  _tk             -> return (OnShutdown sr)
                      OnSend _ _ _tk          -> return (OnShutdown sr)
                      OnRecv _ _tk            -> return (OnShutdown sr)
                      OnSpawn _ _ _tk         -> return (OnShutdown sr)
                      OnDone x                -> return (OnDone x)
                      OnGetProcessState _ _tk -> return (OnShutdown sr)
                      OnShutdown sr'          -> return (OnShutdown sr')
                      OnInterrupt _er _tk     -> return (OnShutdown sr)
                      OnMakeReference _tk     -> return (OnShutdown sr)
                      OnMonitor   _ _tk       -> return (OnShutdown sr)
                      OnDemonitor _ _tk       -> return (OnShutdown sr)
                    return (nextTargetState, tPid)
              nextTargets <- _runEff sts $ traverse deliverTheGoodNews targets
              nextK       <- kontinue sts k ()
              handleProcess
                sts
                (allButTarget Seq.>< (nextTargets :|> (nextK, pid)))

        OnSelf k -> do
          nextK <- kontinue sts k pid
          handleProcess sts (rest :|> (nextK, pid))

        OnMakeReference k -> do
          let (ref, stsNext) = incRef sts
          nextK <- kontinue sts k ref
          handleProcess stsNext (rest :|> (nextK, pid))

        OnYield k -> do
          sts ^. yieldEff
          nextK <- kontinue sts k ()
          handleProcess sts (rest :|> (nextK, pid))

        OnSend toPid msg k -> do
          nextK <- kontinue sts k ()
          handleProcess (enqueueMsg toPid msg sts) (rest :|> (nextK, pid))

        OnGetProcessState toPid k -> do
          nextK <- kontinue sts k (getProcessState toPid sts)
          handleProcess sts (rest :|> (nextK, pid))

        OnSpawn _link f k -> do
          let (newPid, newSts) = newProcessQ sts
          nextK <- kontinue newSts k newPid
          fk    <- runAsCoroutinePure (newSts ^. runEff) (f >> exitNormally SP)
          handleProcess newSts (rest :|> (nextK, pid) :|> (fk, newPid))

        recv@(OnRecv messageSelector k) ->
          case receiveMsg pid messageSelector sts of
            Nothing -> do
              nextK <- diskontinue
                sts
                k
                (ProcessError (show pid ++ " has no message queue"))

              handleProcess sts (rest :|> (nextK, pid))
            Just Nothing -> if Seq.length rest == 0
              then do
                nextK <- diskontinue
                  sts
                  k
                  (ProcessError (show pid ++ " deadlocked"))
                handleProcess sts (rest :|> (nextK, pid))
              else handleProcess sts (rest :|> (recv, pid))
            Just (Just (result, newSts)) -> do
              nextK <- kontinue newSts k result
              handleProcess newSts (rest :|> (nextK, pid))

        OnMonitor toPid k -> do
          let (ref, stsNew) = addMonitoring pid toPid sts
          nextK <- kontinue stsNew k ref
          handleProcess stsNew (rest :|> (nextK, pid))

        OnDemonitor monRef k -> do
          let stsNew = removeMonitoring monRef sts
          nextK <- kontinue stsNew k ()
          handleProcess stsNew (rest :|> (nextK, pid))

-- | The concrete list of 'Eff'ects for running this pure scheduler on @IO@ and
-- with string logging.
type LoggingAndIo =
              '[ Logs LogMessage
               , LogWriterReader LogMessage IO
               , Lift IO
               ]

-- | A 'SchedulerProxy' for 'LoggingAndIo'.
singleThreadedIoScheduler :: SchedulerProxy LoggingAndIo
singleThreadedIoScheduler = SchedulerProxy

-- | Execute a 'Process' using 'schedule' on top of 'Lift' @IO@ and 'Logs'
-- @String@ effects.
defaultMain
  :: HasCallStack
  => Eff
       ( InterruptableProcess
           '[Logs LogMessage, LogWriterReader LogMessage IO, Lift IO]
       )
       ()
  -> IO ()
defaultMain =
  void
    . runLift
    . writeLogs (multiMessageLogWriter ($ printLogMessage))
    . scheduleMonadIOEff
