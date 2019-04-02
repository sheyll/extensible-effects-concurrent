-- | A coroutine based, single threaded scheduler for 'Process'es.
module Control.Eff.Concurrent.Process.SingleThreadedScheduler
  ( scheduleM
  , schedulePure
  , scheduleIO
  , scheduleMonadIOEff
  , scheduleIOWithLogging
  , defaultMainSingleThreaded
  , singleThreadedIoScheduler
  )
where

import           Control.Concurrent             ( yield )
import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.Concurrent.Process
import           Control.Eff.Log
import           Control.Lens            hiding ( (|>)
                                                , Empty
                                                )
import           Control.Monad                  ( void
                                                , when
                                                , foldM
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
import           Data.Monoid
import qualified Control.Monad.State.Strict    as State
import Data.Function (fix)

-- -----------------------------------------------------------------------------
--  STS
-- -----------------------------------------------------------------------------

data STS r m = STS
  { _nextPid :: !ProcessId
  , _nextRef :: !Int
  , _msgQs :: !(Map.Map ProcessId (Seq Dynamic))
  , _monitors :: !(Set.Set (MonitorReference, ProcessId))
  , _processLinks :: !(Set.Set (ProcessId, ProcessId))
  , _runEff :: forall a . Eff r a -> m a
  , _yieldEff :: m ()
  }

initStsMainProcess :: (forall a . Eff r a -> m a) -> m () -> STS r m
initStsMainProcess = STS 1 0 (Map.singleton 0 Seq.empty) Set.empty Set.empty

makeLenses ''STS

instance Show (STS r m) where
  showsPrec d sts = showParen
    (d >= 10)
    ( showString "STS "
    . showString "nextRef: "
    . shows (_nextRef sts)
    . showString " msgQs: "
    . appEndo
        (foldMap
          (\(pid, msgs) ->
            Endo (showString "  " . shows pid . showString ": ")
              <> foldMap (Endo . shows . dynTypeRep) (toList msgs)
          )
          (sts ^.. msgQs . itraversed . withIndex)
        )
    )

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
enqueueMsg toPid msg = msgQs . ix toPid %~ (:|> msg)

newProcessQ :: Maybe ProcessId -> STS r m -> (ProcessId, STS r m)
newProcessQ parentLink sts =
  ( sts ^. nextPid
  , let stsQ =
          sts & nextPid %~ (+ 1) & msgQs . at (sts ^. nextPid) ?~ Seq.empty
    in  case parentLink of
          Nothing -> stsQ
          Just pid ->
            let (Nothing, stsQL) = addLink pid (sts ^. nextPid) stsQ in stsQL
  )

flushMsgs :: ProcessId -> STS m r -> ([Dynamic], STS m r)
flushMsgs pid = State.runState $ do
  msgs <- msgQs . at pid . _Just <<.= Empty
  return (toList msgs)

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
addMonitoring owner target = State.runState $ do
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
triggerAndRemoveMonitor downPid reason = State.execState $ do
  monRefs <- use monitors
  traverse_ go monRefs
 where
  go (mr, owner) = when
    (monitoredProcess mr == downPid)
    (let pdown = ProcessDown mr reason
     in  State.modify' (enqueueMsg owner (toDyn pdown) . removeMonitoring mr)
    )

addLink :: ProcessId -> ProcessId -> STS m r -> (Maybe InterruptReason, STS m r)
addLink fromPid toPid = State.runState $ do
  hasToPid <- use (msgQs . to (Map.member toPid))
  if hasToPid
    then do
      let (a, b) =
            if fromPid <= toPid then (fromPid, toPid) else (toPid, fromPid)
      processLinks %= Set.insert (a, b)
      return Nothing
    else return (Just (LinkedProcessCrashed toPid))

removeLinksTo :: ProcessId -> STS m r -> ([ProcessId], STS m r)
removeLinksTo pid sts = flip State.runState sts $ do
  pl <- use processLinks
  let aPids = pl ^.. folded . filtered (\(_, b) -> b == pid) . _1
  let bPids = pl ^.. folded . filtered (\(a, _) -> a == pid) . _2
  processLinks %= Set.filter (\(a, b) -> a /= pid && b /= pid)
  return (aPids ++ bPids)

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
  OnFlushMessages :: (ResumeProcess [Dynamic] -> Eff r (OnYield r a))
                  -> OnYield r a
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
  OnLink
    :: ProcessId
    -> (ResumeProcess () -> Eff r (OnYield r a))
    -> OnYield r a
  OnUnlink
    :: ProcessId
    -> (ResumeProcess () -> Eff r (OnYield r a))
    -> OnYield r a

instance Show (OnYield r a) where
  show = \case
    OnFlushMessages _     -> "OnFlushMessages"
    OnYield         _     -> "OnYield"
    OnSelf          _     -> "OnSelf"
    OnSpawn False _ _     -> "OnSpawn"
    OnSpawn True  _ _     -> "OnSpawn (link)"
    OnDone     _          -> "OnDone"
    OnShutdown e          -> "OnShutdown " ++ show e
    OnInterrupt e _       -> "OnInterrupt " ++ show e
    OnSend toP _ _        -> "OnSend " ++ show toP
    OnRecv            _ _ -> "OnRecv"
    OnGetProcessState p _ -> "OnGetProcessState " ++ show p
    OnSendShutdown  p e _ -> "OnSendShutdow " ++ show p ++ " " ++ show e
    OnSendInterrupt p e _ -> "OnSendInterrupt " ++ show p ++ " " ++ show e
    OnMakeReference _     -> "OnMakeReference"
    OnMonitor   p _       -> "OnMonitor " ++ show p
    OnDemonitor p _       -> "OnDemonitor " ++ show p
    OnLink      p _       -> "OnLink " ++ show p
    OnUnlink    p _       -> "OnUnlink " ++ show p

runAsCoroutinePure
  :: forall v r m
   . Monad m
  => (forall a . Eff r a -> m a)
  -> Eff (ConsProcess r) v
  -> m (OnYield r v)
runAsCoroutinePure r = r . fix (handle_relay' cont (return . OnDone))
 where
  cont :: (Eff (ConsProcess r) v -> Eff r (OnYield r v))
       -> Arrs (ConsProcess r) x v
       -> Process r x
       -> Eff r (OnYield r v)
  cont k q FlushMessages                = return (OnFlushMessages (k . qApp q))
  cont k q YieldProcess                 = return (OnYield (k . qApp q))
  cont k q SelfPid                      = return (OnSelf (k . qApp q))
  cont k q (Spawn     e               ) = return (OnSpawn False e (k . qApp q))
  cont k q (SpawnLink e               ) = return (OnSpawn True e (k . qApp q))
  cont _ _ (Shutdown  !sr             ) = return (OnShutdown sr)
  cont k q (SendMessage !tp !msg      ) = return (OnSend tp msg (k . qApp q))
  cont k q (ReceiveSelectedMessage f  ) = return (OnRecv f (k . qApp q))
  cont k q (GetProcessState        !tp) = return (OnGetProcessState tp (k . qApp q))
  cont k q (SendInterrupt !tp  !er    ) = return (OnSendInterrupt tp er (k . qApp q))
  cont k q (SendShutdown  !pid !sr    ) = return (OnSendShutdown pid sr (k . qApp q))
  cont k q MakeReference                = return (OnMakeReference (k . qApp q))
  cont k q (Monitor   !pid)             = return (OnMonitor pid (k . qApp q))
  cont k q (Demonitor !ref)             = return (OnDemonitor ref (k . qApp q))
  cont k q (Link      !pid)             = return (OnLink pid (k . qApp q))
  cont k q (Unlink    !pid)             = return (OnUnlink pid (k . qApp q))

-- | Internal 'Process' handler function.
handleProcess
  :: Monad m
  => STS r m
  -> Seq (OnYield r finalResult, ProcessId)
  -> m (Either (ExitReason 'NoRecovery) finalResult)
handleProcess _sts Empty =
  return $ Left (NotRecovered (ProcessError "no main process"))

handleProcess sts allProcs@((!processState, !pid) :<| rest) =
  let handleExit res =
        if pid == 0
          then return res
          else do
            let (downPids, stsNew) = removeLinksTo pid sts
                linkedPids = filter (/= pid) downPids
                reason = LinkedProcessCrashed pid
                unlinkLoop dPidRest ps = foldM (\ps dPid -> sendInterruptToOtherPid dPid reason ps) ps dPidRest
            let allProcsWithoutPid = Seq.filter (\(_, p) -> p /= pid) rest
            nextTargets <- unlinkLoop linkedPids allProcsWithoutPid
            handleProcess
              (dropMsgQ
                 pid
                 (triggerAndRemoveMonitor pid (either SomeExitReason (const (SomeExitReason ExitNormally)) res) stsNew))
              nextTargets
   in case processState of
        OnDone r -> handleExit (Right r)
        OnShutdown e -> handleExit (Left e)
        OnInterrupt e k -> do
          nextK <- diskontinue sts k e
          handleProcess sts (rest :|> (nextK, pid))
        OnSendInterrupt targetPid sr k -> doSendInterrupt targetPid sr k
        OnSendShutdown targetPid sr k -> do
          let allButTarget = Seq.filter (\(_, e) -> e /= pid && e /= targetPid) allProcs
              targets = Seq.filter (\(_, e) -> e == targetPid) allProcs
              suicide = targetPid == pid
          if suicide
            then handleExit (Left sr)
            else do
              let deliverTheGoodNews (targetState, tPid) = do
                    nextTargetState <-
                      case targetState of
                        OnSendInterrupt _ _ _tk -> return (OnShutdown sr)
                        OnSendShutdown _ _ _tk -> return (OnShutdown sr)
                        OnFlushMessages _tk -> return (OnShutdown sr)
                        OnYield _tk -> return (OnShutdown sr)
                        OnSelf _tk -> return (OnShutdown sr)
                        OnSend _ _ _tk -> return (OnShutdown sr)
                        OnRecv _ _tk -> return (OnShutdown sr)
                        OnSpawn _ _ _tk -> return (OnShutdown sr)
                        OnDone x -> return (OnDone x)
                        OnGetProcessState _ _tk -> return (OnShutdown sr)
                        OnShutdown sr' -> return (OnShutdown sr')
                        OnInterrupt _er _tk -> return (OnShutdown sr)
                        OnMakeReference _tk -> return (OnShutdown sr)
                        OnMonitor _ _tk -> return (OnShutdown sr)
                        OnDemonitor _ _tk -> return (OnShutdown sr)
                        OnLink _ _tk -> return (OnShutdown sr)
                        OnUnlink _ _tk -> return (OnShutdown sr)
                    return (nextTargetState, tPid)
              nextTargets <- _runEff sts $ traverse deliverTheGoodNews targets
              nextK <- kontinue sts k ()
              handleProcess sts (allButTarget Seq.>< (nextTargets :|> (nextK, pid)))
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
        OnSpawn link f k -> do
          let (newPid, newSts) =
                newProcessQ
                  (if link
                     then Just pid
                     else Nothing)
                  sts
          fk <- runAsCoroutinePure (newSts ^. runEff) (f >> exitNormally)
          nextK <- kontinue newSts k newPid
          handleProcess newSts (rest :|> (fk, newPid) :|> (nextK, pid))
        OnFlushMessages k -> do
          let (msgs, newSts) = flushMsgs pid sts
          nextK <- kontinue newSts k msgs
          handleProcess newSts (rest :|> (nextK, pid))
        recv@(OnRecv messageSelector k) ->
          case receiveMsg pid messageSelector sts of
            Nothing -> do
              nextK <- diskontinue sts k (ProcessError (show pid ++ " has no message queue"))
              handleProcess sts (rest :|> (nextK, pid))
            Just Nothing ->
              if Seq.length rest == 0
                then do
                  nextK <- diskontinue sts k (ProcessError (show pid ++ " deadlocked"))
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
        OnLink toPid k -> do
          let (downInterrupts, stsNew) = addLink pid toPid sts
          nextK <-
            case downInterrupts of
              Nothing -> kontinue stsNew k ()
              Just i -> diskontinue stsNew k i
          handleProcess stsNew (rest :|> (nextK, pid))
        OnUnlink toPid k -> do
          let (_, stsNew) = removeLinksTo toPid sts
          nextK <- kontinue stsNew k ()
          handleProcess stsNew (rest :|> (nextK, pid))
  where
    doSendInterrupt targetPid sr k = do
      let suicide = targetPid == pid
      if suicide
        then do
          nextK <- diskontinue sts k sr
          handleProcess sts (rest :|> (nextK, pid))
        else do
          nextTargets <- sendInterruptToOtherPid targetPid sr rest
          nextK <- kontinue sts k ()
          handleProcess sts (nextTargets :|> (nextK, pid))
    sendInterruptToOtherPid targetPid sr procs = do
      let allButTarget = Seq.filter (\(_, e) -> e /= targetPid) procs
          targets = Seq.filter (\(_, e) -> e == targetPid) procs
          deliverTheGoodNews (targetState, tPid) = do
            nextTargetState <-
              case targetState of
                OnSendInterrupt _ _ tk -> tk (Interrupted sr)
                OnSendShutdown _ _ tk -> tk (Interrupted sr)
                OnFlushMessages tk -> tk (Interrupted sr)
                OnYield tk -> tk (Interrupted sr)
                OnSelf tk -> tk (Interrupted sr)
                OnSend _ _ tk -> tk (Interrupted sr)
                OnRecv _ tk -> tk (Interrupted sr)
                OnSpawn _ _ tk -> tk (Interrupted sr)
                OnDone x -> return (OnDone x)
                OnGetProcessState _ tk -> tk (Interrupted sr)
                OnShutdown sr' -> return (OnShutdown sr')
                OnInterrupt er tk -> tk (Interrupted er)
                OnMakeReference tk -> tk (Interrupted sr)
                OnMonitor _ tk -> tk (Interrupted sr)
                OnDemonitor _ tk -> tk (Interrupted sr)
                OnLink _ tk -> tk (Interrupted sr)
                OnUnlink _ tk -> tk (Interrupted sr)
            return (nextTargetState, tPid)
      nextTargets <- _runEff sts $ traverse deliverTheGoodNews targets
      return (nextTargets Seq.>< allButTarget)

-- | The concrete list of 'Eff'ects for running this pure scheduler on @IO@ and
-- with string logging.
type LoggingAndIo = '[Logs LogMessage, LogWriterReader LogMessage IO, Lift IO]

-- | A 'SchedulerProxy' for 'LoggingAndIo'.
singleThreadedIoScheduler :: SchedulerProxy LoggingAndIo
singleThreadedIoScheduler = SchedulerProxy

-- | Execute a 'Process' using 'schedule' on top of 'Lift' @IO@ and 'Logs'
-- @String@ effects.
defaultMainSingleThreaded :: HasCallStack => Eff (InterruptableProcess LoggingAndIo) ()-> IO ()
defaultMainSingleThreaded =
  void
    . runLift
    . writeLogs (MkLogWriter printLogMessage)
    . scheduleMonadIOEff
