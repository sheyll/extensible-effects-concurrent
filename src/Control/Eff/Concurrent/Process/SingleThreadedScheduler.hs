-- | A coroutine based, single threaded scheduler for 'Process'es.
module Control.Eff.Concurrent.Process.SingleThreadedScheduler
  ( scheduleM
  , scheduleMonadIOEff
  , scheduleIOWithLogging
  , schedulePure
  , PureEffects
  , PureSafeEffects
  , PureBaseEffects
  , HasPureBaseEffects
  , defaultMain
  , defaultMainWithLogWriter
  , scheduleIO
  , EffectsIo
  , SafeEffectsIo
  , BaseEffectsIo
  , HasBaseEffectsIo
  )
where

import           Control.Concurrent             ( yield )
import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.Concurrent.Misc
import           Control.Eff.Concurrent.Process
import           Control.Eff.Log
import           Control.Eff.LogWriter.IO
import           Control.Eff.LogWriter.Console
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
import           Data.Foldable
import           Data.Monoid
import qualified Control.Monad.State.Strict    as State
import           Data.Function                  ( fix)
import           Data.Dynamic                   ( dynTypeRep )

-- -----------------------------------------------------------------------------
--  STS and ProcessInfo
-- -----------------------------------------------------------------------------

data ProcessInfo =
  MkProcessInfo
  { _processInfoTitle :: !ProcessTitle
  , _processInfoDetails :: !ProcessDetails
  , _processInfoMessageQ :: !(Seq StrictDynamic)
  }

instance Show ProcessInfo where
  showsPrec d (MkProcessInfo pTitle pDetails pQ) =
    showParen
      (d >= 10)
      (appEndo
         (Endo (showChar ' ' . shows pTitle . showString ": ") <>
          foldMap (Endo . showSTypeRep . dynTypeRep . unwrapStrictDynamic) (toList pQ) <>
          Endo (shows pDetails)))

makeLenses ''ProcessInfo

newProcessInfo :: ProcessTitle -> ProcessInfo
newProcessInfo t = MkProcessInfo t "" Seq.empty

data STS r m = STS
  { _nextPid :: !ProcessId
  , _nextRef :: !Int
  , _msgQs :: !(Map.Map ProcessId ProcessInfo)
  , _monitors :: !(Set.Set (MonitorReference, ProcessId))
  , _processLinks :: !(Set.Set (ProcessId, ProcessId))
  , _runEff :: forall a . Eff r a -> m a
  , _yieldEff :: m ()
  }

initStsMainProcess :: (forall a . Eff r a -> m a) -> m () -> STS r m
initStsMainProcess = STS 1 0 (Map.singleton 0 (newProcessInfo "init" )) Set.empty Set.empty

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
          (\(pid, p) ->
            Endo (showChar ' ' . shows pid . showChar ' ' . shows p)
          )
          (sts ^.. msgQs . itraversed . withIndex)
        )
    )

dropMsgQ :: ProcessId -> STS r m -> STS r m
dropMsgQ pid = msgQs . at pid .~ Nothing

getProcessStateFromScheduler :: ProcessId -> STS r m -> Maybe (ProcessTitle, ProcessDetails, ProcessState)
getProcessStateFromScheduler pid sts = toPS <$> sts ^. msgQs . at pid
  where
    toPS p =
      ( p ^. processInfoTitle
      , p ^. processInfoDetails
      , case (p ^. processInfoMessageQ) -- TODO get more detailed state
              of
          _ :<| _ -> ProcessBusy
          _ -> ProcessIdle)

incRef :: STS r m -> (Int, STS r m)
incRef sts = (sts ^. nextRef, sts & nextRef %~ (+ 1))

enqueueMsg :: ProcessId -> StrictDynamic -> STS r m -> STS r m
enqueueMsg toPid msg = msgQs . ix toPid . processInfoMessageQ %~ (:|> msg)

newProcessQ :: Maybe ProcessId -> ProcessTitle -> STS r m -> (ProcessId, STS r m)
newProcessQ parentLink title sts =
  ( sts ^. nextPid
  , let stsQ = sts & nextPid %~ (+ 1) & msgQs . at (sts ^. nextPid) ?~ newProcessInfo title
     in case parentLink of
          Nothing -> stsQ
          Just pid ->
            let (Nothing, stsQL) = addLink pid (sts ^. nextPid) stsQ
             in stsQL)

flushMsgs :: ProcessId -> STS m r -> ([StrictDynamic], STS m r)
flushMsgs pid = State.runState $ do
  msgs <- msgQs . ix pid . processInfoMessageQ <<.= Empty
  return (toList msgs)

receiveMsg
  :: ProcessId -> MessageSelector a -> STS m r -> Maybe (Maybe (a, STS m r))
receiveMsg pid messageSelector sts =
  case sts ^? msgQs . at pid . _Just . processInfoMessageQ of
    Nothing -> Nothing
    Just msgQ ->
      Just $
      case partitionMessages msgQ Empty of
        Nothing -> Nothing
        Just (result, otherMessages) -> Just (result, sts & msgQs . ix pid . processInfoMessageQ .~ otherMessages)
  where
    partitionMessages Empty _acc = Nothing
    partitionMessages (m :<| msgRest) acc =
      maybe
        (partitionMessages msgRest (acc :|> m))
        (\res -> Just (res, acc Seq.>< msgRest))
        (runMessageSelector messageSelector m)

-- | Add monitor: If the process is dead, enqueue a 'ProcessDown' message into the
-- owners message queue
addMonitoring
  :: ProcessId -> ProcessId -> STS m r -> (MonitorReference, STS m r)
addMonitoring owner target =
  State.runState $ do
    mi <- State.state incRef
    let mref = MonitorReference mi target
    when (target /= owner) $ do
      pt <- use msgQs
      if Map.member target pt
        then monitors %= Set.insert (mref, owner)
        else let pdown = ProcessDown mref (ExitOtherProcessNotRunning target) target
              in State.modify' (enqueueMsg owner (toStrictDynamic pdown))
    return mref

removeMonitoring :: MonitorReference -> STS m r -> STS m r
removeMonitoring mref = monitors %~ Set.filter (\(ref, _) -> ref /= mref)

triggerAndRemoveMonitor :: ProcessId -> Interrupt 'NoRecovery -> STS m r -> STS m r
triggerAndRemoveMonitor downPid reason = State.execState $ do
  monRefs <- use monitors
  traverse_ go monRefs
 where
  go (mr, owner) = when
    (monitoredProcess mr == downPid)
    (let pdown = ProcessDown mr reason downPid
     in  State.modify' (enqueueMsg owner (toStrictDynamic pdown) . removeMonitoring mr)
    )

addLink :: ProcessId -> ProcessId -> STS m r -> (Maybe (Interrupt 'Recoverable), STS m r)
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

diskontinue :: STS r m -> (ResumeProcess v -> Eff r a) -> Interrupt 'Recoverable -> m a
diskontinue sts k e = (sts ^. runEff) (k (Interrupted e))

-- -----------------------------------------------------------------------------
--  Meat Of The Thing
-- -----------------------------------------------------------------------------

-- | Like 'scheduleIO' but /pure/. The @yield@ effect is just @return ()@.
-- @schedulePure == runIdentity . 'scheduleM' (Identity . run)  (return ())@
--
-- @since 0.3.0.2
schedulePure
  :: Eff (Processes '[Logs, LogWriterReader Logs]) a
  -> Either (Interrupt 'NoRecovery) a
schedulePure e = run (scheduleM (withSomeLogging @Logs) (return ()) e)

-- | Invoke 'scheduleM' with @lift 'Control.Concurrent.yield'@ as yield effect.
-- @scheduleIO runEff == 'scheduleM' (runLift . runEff) (liftIO 'yield')@
--
-- @since 0.4.0.0
scheduleIO
  :: MonadIO m
  => (forall b . Eff r b -> Eff '[Lift m] b)
  -> Eff (Processes r) a
  -> m (Either (Interrupt 'NoRecovery) a)
scheduleIO r = scheduleM (runLift . r) (liftIO yield)

-- | Invoke 'scheduleM' with @lift 'Control.Concurrent.yield'@ as yield effect.
-- @scheduleMonadIOEff == 'scheduleM' id (liftIO 'yield')@
--
-- @since 0.3.0.2
scheduleMonadIOEff
  :: MonadIO (Eff r)
  => Eff (Processes r) a
  -> Eff r (Either (Interrupt 'NoRecovery) a)
scheduleMonadIOEff = -- schedule (lift yield)
  scheduleM id (liftIO yield)

-- | Run processes that have the 'Logs' and the 'Lift' effects.
-- The user must provide a log handler function.
--
-- Log messages are evaluated strict.
--
-- @scheduleIOWithLogging == 'scheduleIO' . 'withLogging'@
--
-- @since 0.4.0.0
scheduleIOWithLogging
  :: HasCallStack
  => LogWriter (Lift IO)
  -> Eff EffectsIo a
  -> IO (Either (Interrupt 'NoRecovery) a)
scheduleIOWithLogging h = scheduleIO (withLogging h)

-- | Handle the 'Process' effect, as well as all lower effects using an effect handler function.
--
-- Execute the __main__ 'Process' and all the other processes 'spawn'ed by it in the
-- current thread concurrently, using a co-routine based, round-robin
-- scheduler. If a process exits with eg.g 'exitNormally' or 'exitWithError'
-- or is killed by another process @Left ...@ is returned.
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
  -> Eff (Processes r) a
  -> m (Either (Interrupt 'NoRecovery) a)
scheduleM r y e = do
  c <- runAsCoroutinePure r (provideInterruptsShutdown e)
  handleProcess (initStsMainProcess r y) (Seq.singleton (c, 0))

-- | Internal data structure that is part of the coroutine based scheduler
-- implementation.
data OnYield r a where
  OnFlushMessages :: (ResumeProcess [StrictDynamic] -> Eff r (OnYield r a))
                  -> OnYield r a
  OnYield :: (ResumeProcess () -> Eff r (OnYield r a))
         -> OnYield r a
  OnSelf :: (ResumeProcess ProcessId -> Eff r (OnYield r a))
         -> OnYield r a
  OnSpawn :: Bool
          -> ProcessTitle
          -> Eff (Process r ': r) ()
          -> (ResumeProcess ProcessId -> Eff r (OnYield r a))
          -> OnYield r a
  OnDone :: !a -> OnYield r a
  OnShutdown :: Interrupt 'NoRecovery -> OnYield r a
  OnInterrupt :: Interrupt 'Recoverable
                -> (ResumeProcess b -> Eff r (OnYield r a))
                -> OnYield r a
  OnSend :: !ProcessId -> !StrictDynamic
         -> (ResumeProcess () -> Eff r (OnYield r a))
         -> OnYield r a
  OnRecv :: MessageSelector b -> (ResumeProcess b -> Eff r (OnYield r a))
         -> OnYield r a
  OnGetProcessState
         :: ProcessId
         -> (ResumeProcess (Maybe (ProcessTitle, ProcessDetails, ProcessState)) -> Eff r (OnYield r a))
         -> OnYield r a
  OnUpdateProcessDetails
         :: ProcessDetails
         -> (ResumeProcess () -> Eff r (OnYield r a))
         -> OnYield r a
  OnSendShutdown :: !ProcessId -> Interrupt 'NoRecovery
                 -> (ResumeProcess () -> Eff r (OnYield r a)) -> OnYield r a
  OnSendInterrupt :: !ProcessId -> Interrupt 'Recoverable
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
    OnFlushMessages _          -> "OnFlushMessages"
    OnYield         _          -> "OnYield"
    OnSelf          _          -> "OnSelf"
    OnSpawn False t _ _        -> "OnSpawn" ++ show t
    OnSpawn True  t _ _        -> "OnSpawn (link)" ++ show t
    OnDone     _               -> "OnDone"
    OnShutdown e               -> "OnShutdown " ++ show e
    OnInterrupt e _            -> "OnInterrupt " ++ show e
    OnSend toP _ _             -> "OnSend " ++ show toP
    OnRecv            _ _      -> "OnRecv"
    OnGetProcessState p _      -> "OnGetProcessState " ++ show p
    OnUpdateProcessDetails p _ -> "OnUpdateProcessDetails " ++ show p
    OnSendShutdown  p e _      -> "OnSendShutdow " ++ show p ++ " " ++ show e
    OnSendInterrupt p e _      -> "OnSendInterrupt " ++ show p ++ " " ++ show e
    OnMakeReference _          -> "OnMakeReference"
    OnMonitor   p _            -> "OnMonitor " ++ show p
    OnDemonitor p _            -> "OnDemonitor " ++ show p
    OnLink      p _            -> "OnLink " ++ show p
    OnUnlink    p _            -> "OnUnlink " ++ show p

runAsCoroutinePure
  :: forall v r m
   . Monad m
  => (forall a . Eff r a -> m a)
  -> Eff (SafeProcesses r) v
  -> m (OnYield r v)
runAsCoroutinePure r = r . fix (handle_relay' cont (return . OnDone))
 where
  cont :: (Eff (SafeProcesses r) v -> Eff r (OnYield r v))
       -> Arrs (SafeProcesses r) x v
       -> Process r x
       -> Eff r (OnYield r v)
  cont k q FlushMessages                = return (OnFlushMessages (k . qApp q))
  cont k q YieldProcess                 = return (OnYield (k . qApp q))
  cont k q SelfPid                      = return (OnSelf (k . qApp q))
  cont k q (Spawn     t e             ) = return (OnSpawn False t e (k . qApp q))
  cont k q (SpawnLink t e             ) = return (OnSpawn True t e (k . qApp q))
  cont _ _ (Shutdown  !sr             ) = return (OnShutdown sr)
  cont k q (SendMessage !tp !msg      ) = return (OnSend tp msg (k . qApp q))
  cont k q (ReceiveSelectedMessage f  ) = return (OnRecv f (k . qApp q))
  cont k q (GetProcessState        !tp) = return (OnGetProcessState tp (k . qApp q))
  cont k q (UpdateProcessDetails   !td) = return (OnUpdateProcessDetails td (k . qApp q))
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
  -> m (Either (Interrupt 'NoRecovery) finalResult)
handleProcess _sts Empty =
  return $ Left (interruptToExit (ErrorInterrupt "no main process"))

handleProcess sts allProcs@((!processState, !pid) :<| rest) =
  let handleExit res =
        if pid == 0
          then return res
          else do
            let (downPids, stsNew) = removeLinksTo pid sts
                linkedPids = filter (/= pid) downPids
                reason = LinkedProcessCrashed pid
                unlinkLoop dPidRest ps = foldM sendInterruptOrNot ps dPidRest
                  where
                    sendInterruptOrNot ps' dPid =
                      case res of
                        Right _ -> return ps'
                        Left er ->
                          case toExitSeverity er of
                            NormalExit -> return ps'
                            Crash      -> sendInterruptToOtherPid dPid reason ps'

            let allButMe = Seq.filter (\(_, p) -> p /= pid) rest
            nextTargets <- unlinkLoop linkedPids allButMe
            handleProcess
              (dropMsgQ
                 pid
                 (triggerAndRemoveMonitor
                  pid
                  (either
                    id
                    (const ExitNormally) res)
                    stsNew))
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
                        OnSpawn _ _ _ _tk -> return (OnShutdown sr)
                        OnDone x -> return (OnDone x)
                        OnGetProcessState _ _tk -> return (OnShutdown sr)
                        OnUpdateProcessDetails _ _tk -> return (OnShutdown sr)
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
          nextK <- kontinue sts k (getProcessStateFromScheduler toPid sts)
          handleProcess sts (rest :|> (nextK, pid))
        OnUpdateProcessDetails pd k -> do
          let newSts = sts & msgQs . ix pid . processInfoDetails .~ pd
          nextK <- kontinue newSts k ()
          handleProcess newSts (rest :|> (nextK, pid))
        OnSpawn link title f k -> do
          let (newPid, newSts) =
                newProcessQ
                  (if link
                     then Just pid
                     else Nothing)
                  title
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
              nextK <- diskontinue sts k (ErrorInterrupt (show pid ++ " has no message queue"))
              handleProcess sts (rest :|> (nextK, pid))
            Just Nothing ->
              if Seq.length rest == 0
                then do
                  nextK <- diskontinue sts k (ErrorInterrupt (show pid ++ " deadlocked"))
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
                OnSpawn _ _ _ tk -> tk (Interrupted sr)
                OnDone x -> return (OnDone x)
                OnGetProcessState _ tk -> tk (Interrupted sr)
                OnUpdateProcessDetails _ tk -> tk (Interrupted sr)
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

-- | Execute a 'Process' using 'scheduleM' on top of 'Lift' @IO@.
-- All logging is written to the console using 'consoleLogWriter'.
--
-- To use another 'LogWriter' use 'defaultMainWithLogWriter' instead.
defaultMain :: HasCallStack => Eff EffectsIo () -> IO ()
defaultMain =
  void
    . runLift
    . withLogging consoleLogWriter
    . scheduleMonadIOEff

-- | Execute a 'Process' using 'scheduleM' on top of 'Lift' @IO@.
-- All logging is written using the given 'LogWriter'.
--
-- @since 0.25.0
defaultMainWithLogWriter :: HasCallStack => LogWriter (Lift IO) -> Eff EffectsIo () -> IO ()
defaultMainWithLogWriter lw =
  void
    . runLift
    . withLogging lw
    . scheduleMonadIOEff


-- | The effect list for 'Process' effects in the single threaded pure scheduler.
--
-- See 'PureBaseEffects' and 'Processes'
--
-- @since 0.25.0
type PureEffects = Processes PureBaseEffects

-- | The effect list for 'Process' effects in the single threaded pure scheduler.
--  This is like 'SafeProcesses', no 'Interrupts' are present.
--
-- See 'PureBaseEffects' and 'SafeProcesses'
--
-- @since 0.25.0
type PureSafeEffects = SafeProcesses PureBaseEffects

-- | The effect list for a pure, single threaded scheduler contains only
-- 'Logs' and the 'LogWriterReader' for 'PureLogWriter'.
--
-- @since 0.25.0
type PureBaseEffects = '[Logs, LogWriterReader Logs]

-- | Constraint for the existence of the underlying scheduler effects.
--
-- See 'PureBaseEffects'
--
-- @since 0.25.0
type HasPureBaseEffects e = (HasCallStack, PureBaseEffects <:: e)

-- | The effect list for 'Process' effects in the single threaded scheduler.
--
-- See 'BaseEffectsIo'
--
-- @since 0.25.0
type EffectsIo = Processes BaseEffectsIo

-- | The effect list for 'Process' effects in the single threaded scheduler.
--  This is like 'SafeProcesses', no 'Interrupts' are present.
--
-- See 'BaseEffectsIo'.
--
-- @since 0.25.0
type SafeEffectsIo = SafeProcesses BaseEffectsIo

-- | The effect list for the underlying scheduler.
--
-- See 'LoggingAndIo'
--
-- @since 0.25.0
type BaseEffectsIo = LoggingAndIo

-- | Constraint for the existence of the underlying scheduler effects.
--
-- @since 0.25.0
type HasBaseEffectsIo e = (HasCallStack, Lifted IO e, LoggingAndIo <:: e)
