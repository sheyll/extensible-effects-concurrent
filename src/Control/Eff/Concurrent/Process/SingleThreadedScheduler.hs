-- | A coroutine based, single threaded scheduler for 'Process'es.
module Control.Eff.Concurrent.Process.SingleThreadedScheduler
  ( scheduleM
  , schedulePure
  , scheduleIO
  , scheduleMonadIOEff
  , scheduleIOWithLogging
  , defaultMain
  , singleThreadedIoScheduler
  , LoggingAndIo
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
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Map.Strict               as Map
import           GHC.Stack
import           Data.Kind                      ( )
import           Data.Dynamic
import           Data.Maybe


-- | Like 'schedule' but /pure/. The @yield@ effect is just @return ()@.
-- @schedulePure == runIdentity . 'scheduleM' (Identity . run)  (return ())@
--
-- @since 0.3.0.2
schedulePure
  :: (  HasLogWriterProxy Identity
     => Eff (ConsProcess '[LogsM LogMessage Identity, Lift Identity]) a
     )
  -> Either String a
schedulePure e = runIdentity
  (scheduleM (runLift . handleLogs traceLogMessageWriter)
             (return ())
             (usingLogWriterProxy (LogWriterProxy @Identity) e)
  )

-- | Invoke 'schedule' with @lift 'Control.Concurrent.yield'@ as yield effect.
-- @scheduleIO runEff == 'scheduleM' (runLift . runEff) (liftIO 'yield')@
--
-- @since 0.4.0.0
scheduleIO
  :: MonadIO m
  => (forall b . Eff r b -> Eff '[Lift m] b)
  -> Eff (ConsProcess r) a
  -> m (Either String a)
scheduleIO runEff = scheduleM (runLift . runEff) (liftIO yield)

-- | Invoke 'schedule' with @lift 'Control.Concurrent.yield'@ as yield effect.
-- @scheduleMonadIOEff == 'scheduleM' id (liftIO 'yield')@
--
-- @since 0.3.0.2
scheduleMonadIOEff
  :: MonadIO (Eff r) => Eff (ConsProcess r) a -> Eff r (Either String a)
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
  -> Eff (ConsProcess '[Logs l, Lift IO]) a
  -> IO (Either String a)
scheduleIOWithLogging h = scheduleIO (handleLogs h)

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
  -> (HasLogWriterProxy m => Eff (ConsProcess r) a)
  -> m (Either String a)
scheduleM runEff yieldEff e' = do
  let e = (usingLogWriterProxy (LogWriterProxy @m) e')
  y <- runAsCoroutinePure runEff e
  handleProcess runEff
                yieldEff
                1
                0
                (Map.singleton 0 Seq.empty)
                (Seq.singleton (y, 0))

-- | Internal data structure that is part of the coroutine based scheduler
-- implementation.
data OnYield r a where
  OnYield :: (ResumeProcess () -> Eff r (OnYield r a))
         -> OnYield r a
  OnSelf :: (ResumeProcess ProcessId -> Eff r (OnYield r a))
         -> OnYield r a
  OnSpawn :: Eff (Process r ': r) ()
          -> (ResumeProcess ProcessId -> Eff r (OnYield r a))
          -> OnYield r a
  OnDone :: !a -> OnYield r a
  OnShutdown :: ShutdownRequest -> OnYield r a
  OnRaiseError :: !String -> OnYield r a
  OnSend :: !ProcessId -> !Dynamic
         -> (ResumeProcess Bool -> Eff r (OnYield r a))
         -> OnYield r a
  OnRecv :: MessageSelector b -> (ResumeProcess b -> Eff r (OnYield r a))
         -> OnYield r a
  OnSendShutdown :: !ProcessId -> !ShutdownRequest -> (ResumeProcess Bool -> Eff r (OnYield r a)) -> OnYield r a
  OnMakeReference :: (ResumeProcess Int -> Eff r (OnYield r a)) -> OnYield r a

-- | Internal 'Process' handler function.
handleProcess
  :: Monad m
  => (forall a . Eff r a -> m a)
  -> m ()
  -> ProcessId
  -> Int
  -> Map.Map ProcessId (Seq Dynamic)
  -> Seq (OnYield r finalResult, ProcessId)
  -> m (Either String finalResult)
handleProcess _runEff _yieldEff _newPid _nextRef _msgQs Empty =
  return $ Left "no main process"

handleProcess runEff yieldEff !newPid !nextRef !msgQs allProcs@((!processState, !pid) :<| rest)
  = let handleExit res = if pid == 0
          then return res
          else handleProcess runEff
                             yieldEff
                             newPid
                             nextRef
                             (msgQs & at pid .~ Nothing)
                             rest
    in
      case processState of
        OnDone       r                 -> handleExit (Right r)

        OnShutdown ExitNormally -> handleExit (Left "process exited normally")
        OnShutdown   (ExitWithError e) -> handleExit (Left e)

        OnRaiseError errM              -> handleExit (Left errM)

        OnSendShutdown targetPid sr k  -> do
          let allButTarget =
                Seq.filter (\(_, e) -> e /= pid && e /= targetPid) allProcs
              targets     = Seq.filter (\(_, e) -> e == targetPid) allProcs
              suicide     = targetPid == pid
              targetFound = suicide || not (Seq.null targets)
          if suicide
            then do
              nextK <- runEff $ k (ShutdownRequested sr)
              handleProcess runEff
                            yieldEff
                            newPid
                            nextRef
                            msgQs
                            (rest :|> (nextK, pid))
            else do
              let
                deliverTheGoodNews (targetState, tPid) = do
                  nextTargetState <- case targetState of
                    OnSendShutdown _ _ tk -> tk (ShutdownRequested sr)
                    OnYield tk            -> tk (ShutdownRequested sr)
                    OnSelf  tk            -> tk (ShutdownRequested sr)
                    OnSend _ _ tk         -> tk (ShutdownRequested sr)
                    OnRecv  _ tk          -> tk (ShutdownRequested sr)
                    OnSpawn _ tk          -> tk (ShutdownRequested sr)
                    OnDone          x     -> return (OnDone x)
                    OnShutdown      _     -> return (OnShutdown sr)
                    OnRaiseError er -> return (OnShutdown (ExitWithError er))
                    OnMakeReference tk    -> tk (ShutdownRequested sr)
                  return (nextTargetState, tPid)
              nextTargets <- runEff $ traverse deliverTheGoodNews targets
              nextK       <- runEff $ k (ResumeWith targetFound)
              handleProcess
                runEff
                yieldEff
                newPid
                nextRef
                msgQs
                (allButTarget Seq.>< (nextTargets :|> (nextK, pid)))


        OnSelf k -> do
          nextK <- runEff $ k (ResumeWith pid)
          handleProcess runEff
                        yieldEff
                        newPid
                        nextRef
                        msgQs
                        (rest :|> (nextK, pid))

        OnMakeReference k -> do
          nextK <- runEff $ k (ResumeWith nextRef)
          handleProcess runEff
                        yieldEff
                        newPid
                        (nextRef + 1)
                        msgQs
                        (rest :|> (nextK, pid))

        OnYield k -> do
          yieldEff
          nextK <- runEff $ k (ResumeWith ())
          handleProcess runEff
                        yieldEff
                        newPid
                        nextRef
                        msgQs
                        (rest :|> (nextK, pid))

        OnSend toPid msg k -> do
          nextK <- runEff $ k (ResumeWith (msgQs ^. at toPid . to isJust))
          handleProcess runEff
                        yieldEff
                        newPid
                        nextRef
                        (msgQs & at toPid . _Just %~ (:|> msg))
                        (rest :|> (nextK, pid))

        OnSpawn f k -> do
          nextK <- runEff $ k (ResumeWith newPid)
          fk    <- runAsCoroutinePure runEff (f >> exitNormally SP)
          handleProcess runEff
                        yieldEff
                        (newPid + 1)
                        0
                        (msgQs & at newPid ?~ Seq.empty)
                        (rest :|> (nextK, pid) :|> (fk, newPid))

        recv@(OnRecv messageSelector k) -> case msgQs ^. at pid of
          Nothing -> do
            nextK <- runEff $ k (OnError (show pid ++ " has no message queue!"))
            handleProcess runEff
                          yieldEff
                          newPid
                          nextRef
                          msgQs
                          (rest :|> (nextK, pid))
          Just Empty -> if Seq.length rest == 0
            then do
              nextK <- runEff
                $ k (OnError ("Process " ++ show pid ++ " deadlocked!"))
              handleProcess runEff
                            yieldEff
                            newPid
                            nextRef
                            msgQs
                            (rest :|> (nextK, pid))
            else handleProcess runEff
                               yieldEff
                               newPid
                               nextRef
                               msgQs
                               (rest :|> (recv, pid))
          Just messages ->
            let partitionMessages Empty           _acc = Nothing
                partitionMessages (m :<| msgRest) acc  = maybe
                  (partitionMessages msgRest (acc :|> m))
                  (\res -> Just (res, acc Seq.>< msgRest))
                  (runMessageSelector messageSelector m)
            in  case partitionMessages messages Empty of
                  Nothing -> handleProcess runEff
                                           yieldEff
                                           newPid
                                           nextRef
                                           msgQs
                                           (rest :|> (recv, pid))
                  Just (result, otherMessages) -> do
                    nextK <- runEff $ k (ResumeWith result)
                    handleProcess runEff
                                  yieldEff
                                  newPid
                                  nextRef
                                  (msgQs & at pid . _Just .~ otherMessages)
                                  (rest :|> (nextK, pid))

runAsCoroutinePure
  :: forall v r m
   . Monad m
  => (forall a . Eff r a -> m a)
  -> Eff (ConsProcess r) v
  -> m (OnYield r v)
runAsCoroutinePure runEff = runEff . handle_relay (return . OnDone) cont
 where
  cont :: Process r x -> (x -> Eff r (OnYield r v)) -> Eff r (OnYield r v)
  cont YieldProcess               k  = return (OnYield k)
  cont SelfPid                    k  = return (OnSelf k)
  cont (Spawn      e        )     k  = return (OnSpawn e k)
  cont (Shutdown   !sr      )     _k = return (OnShutdown sr)
  cont (RaiseError !e       )     _k = return (OnRaiseError e)
  cont (SendMessage !tp !msg)     k  = return (OnSend tp msg k)
  cont (ReceiveSelectedMessage f) k  = return (OnRecv f k)
  cont (SendShutdown !pid !sr   ) k  = return (OnSendShutdown pid sr k)
  cont MakeReference              k  = return (OnMakeReference k)

-- | The concrete list of 'Eff'ects for running this pure scheduler on @IO@ and
-- with string logging.
type LoggingAndIo =
              '[ Logs LogMessage
               , Lift IO
               ]

-- | A 'SchedulerProxy' for 'LoggingAndIo'.
singleThreadedIoScheduler :: SchedulerProxy LoggingAndIo
singleThreadedIoScheduler = SchedulerProxy

-- | Execute a 'Process' using 'schedule' on top of 'Lift' @IO@ and 'Logs'
-- @String@ effects.
defaultMain
  :: HasCallStack
  => Eff '[Process '[Logs LogMessage, Lift IO], Logs LogMessage, Lift IO] ()
  -> IO ()
defaultMain =
  void
    . runLift
    . handleLogs (multiMessageLogWriter ($ printLogMessage))
    . scheduleMonadIOEff
