-- | A coroutine based, single threaded scheduler for 'Process'es.
module Control.Eff.Concurrent.Process.SingleThreadedScheduler
  ( schedule
  , schedulePure
  , scheduleM
  , scheduleIO
  , scheduleWithLogging
  , defaultMain
  , singleThreadedIoScheduler
  , LoggingAndIo
  )
where

import           Control.Concurrent             ( yield )
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Extend
import           Control.Eff.Concurrent.Process
import           Control.Eff.Lift
import           Control.Eff.Log
import           Control.Lens            hiding ( (|>)
                                                , Empty
                                                )
import           Control.Monad                  ( void )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Map.Strict               as Map
import           GHC.Stack
import           Data.Kind                      ( )
import           Data.Dynamic
import           Data.Maybe

-- | Invoke 'schedule' with @lift 'yield'@ as yield effect.
--
-- @since 0.3.0.2
scheduleIO
  :: SetMember Lift (Lift IO) r
  => Eff (ConsProcess r) a
  -> Eff r (Either String a)
scheduleIO = schedule (lift yield)

-- | Like 'schedule' but /pure/. The @yield@ effect is just @return ()@.
-- @schedulePure == runIdentity . 'scheduleM' (Identity . run)@
--
-- @since 0.3.0.2
schedulePure :: Eff (ConsProcess '[]) a -> Either String a
schedulePure = runIdentity . scheduleM (Identity . run) (return ())

-- | Handle the 'Process' effect, as well as all lower effects using an effect handler function.
--
-- Execute the __main__ 'Process' and all the other processes 'spawn'ed by it in the
-- current thread concurrently, using a co-routine based, round-robin
-- scheduler. If a process exits with 'exitNormally', 'exitWithError',
-- 'raiseError' or is killed by another process @Left ...@ is returned.
-- Otherwise, the result will be wrapped in a @Right@.
--
-- @since 0.4.0.0
scheduleM
  :: Monad m
  => (forall b . Eff r b -> m b)
  -> m () -- ^ An that performs a __yield__ w.r.t. the underlying effect
  --  @r@. E.g. if @Lift IO@ is present, this might be:
  --  @lift 'Control.Concurrent.yield'.
  -> Eff (ConsProcess r) a
  -> m (Either String a)
scheduleM runEff yieldEff e = do
  y <- runAsCoroutinePure runEff e
  goPure runEff yieldEff 1 (Map.singleton 0 Seq.empty) (Seq.singleton (y, 0))


-- | Like 'schedulePure' but with logging.
-- @scheduleWithLogging == 'run' . 'captureLogs' . 'schedule' (return ())@
--
-- @since 0.3.0.2
scheduleWithLogging :: Eff (ConsProcess '[Logs m]) a -> (Either String a, Seq m)
scheduleWithLogging = run . captureLogs . schedule (return ())

-- | Execute a 'Process' and all the other processes 'spawn'ed by it in the
-- current thread concurrently, using a co-routine based, round-robin
-- scheduler. If a process exits with 'exitNormally', 'exitWithError',
-- 'raiseError' or is killed by another process @Left ...@ is returned.
-- Otherwise, the result will be wrapped in a @Right@.
schedule
  :: forall r finalResult
   . Eff r () -- ^ An that performs a __yield__ w.r.t. the underlying effect
              --  @r@. E.g. if @Lift IO@ is present, this might be:
              --  @lift 'Control.Concurrent.yield'.
  -> Eff (Process r ': r) finalResult
  -> Eff r (Either String finalResult)
schedule yieldEff mainProcessAction = do
  y <- runAsCoroutine mainProcessAction
  go yieldEff 1 (Map.singleton 0 Seq.empty) (Seq.singleton (y, 0))

go
  :: Eff r () -- ^ An that performs a __yield__ w.r.t. the underlying effect
    --  @r@. E.g. if @Lift IO@ is present, this might be:
    --  @lift 'Control.Concurrent.yield'.
  -> ProcessId
  -> Map.Map ProcessId (Seq Dynamic)
  -> Seq (OnYield r finalResult, ProcessId)
  -> Eff r (Either String finalResult)
go _yieldEff _newPid _msgQs Empty = return (Left "no main process")

go yieldEff !newPid !msgQs allProcs@((!processState, !pid) :<| rest)
  = let
      handleExit res = if pid == 0
        then return res
        else go yieldEff newPid (msgQs & at pid .~ Nothing) rest
      maybeYield = if pid == 0 then yieldEff else return ()
    in
      case processState of
        OnDone r                   -> handleExit (Right r)

        OnShutdown -> handleExit (Left "process exited normally")

        OnRaiseError errM          -> handleExit (Left errM)

        OnExitError  errM          -> handleExit (Left errM)

        OnSendShutdown targetPid k -> do
          let allButTarget =
                Seq.filter (\(_, e) -> e /= pid && e /= targetPid) allProcs
              targets     = Seq.filter (\(_, e) -> e == targetPid) allProcs
              suicide     = targetPid == pid
              targetFound = suicide || not (Seq.null targets)
          if suicide
            then do
              nextK <- k ShutdownRequested
              go yieldEff newPid msgQs (rest :|> (nextK, pid))
            else do
              let deliverTheGoodNews (targetState, tPid) = do
                    nextTargetState <- case targetState of
                      OnSendShutdown _ tk -> tk ShutdownRequested
                      OnYield tk          -> tk ShutdownRequested
                      OnSelf  tk          -> tk ShutdownRequested
                      OnSend _ _ tk       -> tk ShutdownRequested
                      OnRecv tk           -> tk ShutdownRequested
                      OnSpawn _ tk        -> tk ShutdownRequested
                      OnDone x            -> return (OnDone x)
                      OnShutdown          -> return OnShutdown
                      OnExitError  er     -> return (OnExitError er)
                      OnRaiseError er     -> return (OnExitError er)
                    return (nextTargetState, tPid)
              nextTargets <- traverse deliverTheGoodNews targets
              nextK       <- k (ResumeWith targetFound)
              maybeYield
              go yieldEff
                 newPid
                 msgQs
                 (allButTarget Seq.>< (nextTargets :|> (nextK, pid)))


        OnSelf k -> do
          nextK <- k (ResumeWith pid)
          maybeYield
          go yieldEff newPid msgQs (rest :|> (nextK, pid))

        OnYield k -> do
          yieldEff
          nextK <- k (ResumeWith ())
          go yieldEff newPid msgQs (rest :|> (nextK, pid))

        OnSend toPid msg k -> do
          nextK <- k (ResumeWith (msgQs ^. at toPid . to isJust))
          maybeYield
          go yieldEff
             newPid
             (msgQs & at toPid . _Just %~ (:|> msg))
             (rest :|> (nextK, pid))

        recv@(OnRecv k) -> case msgQs ^. at pid of
          Nothing -> do
            nextK <- k (OnError (show pid ++ " has no message queue!"))
            maybeYield
            go yieldEff newPid msgQs (rest :|> (nextK, pid))
          Just Empty -> if Seq.length rest == 0
            then do
              nextK <- k (OnError ("Process " ++ show pid ++ " deadlocked!"))
              maybeYield
              go yieldEff newPid msgQs (rest :|> (nextK, pid))
            else go yieldEff newPid msgQs (rest :|> (recv, pid))

          Just (nextMessage :<| restMessages) -> do
            nextK <- k (ResumeWith nextMessage)
            maybeYield
            go yieldEff
               newPid
               (msgQs & at pid . _Just .~ restMessages)
               (rest :|> (nextK, pid))

        OnSpawn f k -> do
          nextK <- k (ResumeWith newPid)
          fk    <- runAsCoroutine (f >> exitNormally SP)
          maybeYield
          go yieldEff
             (newPid + 1)
             (msgQs & at newPid .~ Just Seq.empty)
             (rest :|> (nextK, pid) :|> (fk, newPid))

data OnYield r a where
  OnYield :: (ResumeProcess () -> Eff r (OnYield r a))
         -> OnYield r a
  OnSelf :: (ResumeProcess ProcessId -> Eff r (OnYield r a))
         -> OnYield r a
  OnSpawn :: Eff (Process r ': r) ()
          -> (ResumeProcess ProcessId -> Eff r (OnYield r a))
          -> OnYield r a
  OnDone :: !a -> OnYield r a
  OnShutdown :: OnYield r a
  OnExitError :: !String -> OnYield r a
  OnRaiseError :: !String -> OnYield r a
  OnSend :: !ProcessId -> !Dynamic
         -> (ResumeProcess Bool -> Eff r (OnYield r a))
         -> OnYield r a
  OnRecv :: (ResumeProcess Dynamic -> Eff r (OnYield r a))
         -> OnYield r a
  OnSendShutdown :: !ProcessId -> (ResumeProcess Bool -> Eff r (OnYield r a)) -> OnYield r a

runAsCoroutine :: forall r v . Eff (Process r ': r) v -> Eff r (OnYield r v)
runAsCoroutine = handle_relay (return . OnDone) cont
 where
  cont :: Process r x -> (x -> Eff r (OnYield r v)) -> Eff r (OnYield r v)
  cont YieldProcess           k  = return (OnYield k)
  cont SelfPid                k  = return (OnSelf k)
  cont (Spawn e)              k  = return (OnSpawn e k)
  cont Shutdown               _k = return OnShutdown
  cont (ExitWithError !e    ) _k = return (OnExitError e)
  cont (RaiseError    !e    ) _k = return (OnRaiseError e)
  cont (SendMessage !tp !msg) k  = return (OnSend tp msg k)
  cont ReceiveMessage         k  = return (OnRecv k)
  cont (SendShutdown !pid)    k  = return (OnSendShutdown pid k)

goPure
  :: Monad m
  => (forall a . Eff r a -> m a)
  -> m ()
  -> ProcessId
  -> Map.Map ProcessId (Seq Dynamic)
  -> Seq (OnYield r finalResult, ProcessId)
  -> m (Either String finalResult)
goPure _runEff _yieldEff _newPid _msgQs Empty = return $ Left "no main process"

goPure runEff yieldEff !newPid !msgQs allProcs@((!processState, !pid) :<| rest)
  = let handleExit res = if pid == 0
          then return res
          else goPure runEff yieldEff newPid (msgQs & at pid .~ Nothing) rest
    in
      case processState of
        OnDone r                   -> handleExit (Right r)

        OnShutdown -> handleExit (Left "process exited normally")

        OnRaiseError errM          -> handleExit (Left errM)

        OnExitError  errM          -> handleExit (Left errM)

        OnSendShutdown targetPid k -> do
          let allButTarget =
                Seq.filter (\(_, e) -> e /= pid && e /= targetPid) allProcs
              targets     = Seq.filter (\(_, e) -> e == targetPid) allProcs
              suicide     = targetPid == pid
              targetFound = suicide || not (Seq.null targets)
          if suicide
            then do
              nextK <- runEff $ k ShutdownRequested
              goPure runEff yieldEff newPid msgQs (rest :|> (nextK, pid))
            else do
              let deliverTheGoodNews (targetState, tPid) = do
                    nextTargetState <- case targetState of
                      OnSendShutdown _ tk -> tk ShutdownRequested
                      OnYield tk          -> tk ShutdownRequested
                      OnSelf  tk          -> tk ShutdownRequested
                      OnSend _ _ tk       -> tk ShutdownRequested
                      OnRecv tk           -> tk ShutdownRequested
                      OnSpawn _ tk        -> tk ShutdownRequested
                      OnDone x            -> return (OnDone x)
                      OnShutdown          -> return OnShutdown
                      OnExitError  er     -> return (OnExitError er)
                      OnRaiseError er     -> return (OnExitError er)
                    return (nextTargetState, tPid)
              nextTargets <- runEff $ traverse deliverTheGoodNews targets
              nextK       <- runEff $ k (ResumeWith targetFound)
              goPure runEff
                     yieldEff
                     newPid
                     msgQs
                     (allButTarget Seq.>< (nextTargets :|> (nextK, pid)))


        OnSelf k -> do
          nextK <- runEff $ k (ResumeWith pid)
          goPure runEff yieldEff newPid msgQs (rest :|> (nextK, pid))

        OnYield k -> do
          yieldEff
          nextK <- runEff $ k (ResumeWith ())
          goPure runEff yieldEff newPid msgQs (rest :|> (nextK, pid))

        OnSend toPid msg k -> do
          nextK <- runEff $ k (ResumeWith (msgQs ^. at toPid . to isJust))
          goPure runEff
                 yieldEff
                 newPid
                 (msgQs & at toPid . _Just %~ (:|> msg))
                 (rest :|> (nextK, pid))

        recv@(OnRecv k) -> case msgQs ^. at pid of
          Nothing -> do
            nextK <- runEff $ k (OnError (show pid ++ " has no message queue!"))
            goPure runEff yieldEff newPid msgQs (rest :|> (nextK, pid))
          Just Empty -> if Seq.length rest == 0
            then do
              nextK <- runEff
                $ k (OnError ("Process " ++ show pid ++ " deadlocked!"))
              goPure runEff yieldEff newPid msgQs (rest :|> (nextK, pid))
            else goPure runEff yieldEff newPid msgQs (rest :|> (recv, pid))

          Just (nextMessage :<| restMessages) -> do
            nextK <- runEff $ k (ResumeWith nextMessage)
            goPure runEff
                   yieldEff
                   newPid
                   (msgQs & at pid . _Just .~ restMessages)
                   (rest :|> (nextK, pid))

        OnSpawn f k -> do
          nextK <- runEff $ k (ResumeWith newPid)
          fk    <- runAsCoroutinePure runEff (f >> exitNormally SP)
          goPure runEff
                 yieldEff
                 (newPid + 1)
                 (msgQs & at newPid .~ Just Seq.empty)
                 (rest :|> (nextK, pid) :|> (fk, newPid))

runAsCoroutinePure
  :: forall v r m
   . Monad m
  => (forall a . Eff r a -> m a)
  -> Eff (ConsProcess r) v
  -> m (OnYield r v)
runAsCoroutinePure runEff = runEff . handle_relay (return . OnDone) cont
 where
  cont :: Process r x -> (x -> Eff r (OnYield r v)) -> Eff r (OnYield r v)
  cont YieldProcess           k  = return (OnYield k)
  cont SelfPid                k  = return (OnSelf k)
  cont (Spawn e)              k  = return (OnSpawn e k)
  cont Shutdown               _k = return OnShutdown
  cont (ExitWithError !e    ) _k = return (OnExitError e)
  cont (RaiseError    !e    ) _k = return (OnRaiseError e)
  cont (SendMessage !tp !msg) k  = return (OnSend tp msg k)
  cont ReceiveMessage         k  = return (OnRecv k)
  cont (SendShutdown !pid)    k  = return (OnSendShutdown pid k)

-- | The concrete list of 'Eff'ects for running this pure scheduler on @IO@ and
-- with string logging.
type LoggingAndIo =
              '[ Logs String
               , Lift IO
               ]

-- | A 'SchedulerProxy' for 'LoggingAndIo'.
singleThreadedIoScheduler :: SchedulerProxy LoggingAndIo
singleThreadedIoScheduler = SchedulerProxy

-- | Execute a 'Process' using 'schedule' on top of 'Lift' @IO@ and 'Logs'
-- @String@ effects.
defaultMain
  :: HasCallStack
  => Eff '[Process '[Logs String, Lift IO], Logs String, Lift IO] ()
  -> IO ()
defaultMain e = void $ runLift $ handleLogsWith (scheduleIO e) ($! putStrLn)
