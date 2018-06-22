-- | A coroutine based, single threaded scheduler for 'Process'es.
module Control.Eff.Concurrent.Process.SingleThreadedScheduler
  ( schedule
  , defaultMain
  , singleThreadedIoScheduler
  , LoggingAndIo) where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.Log
import Control.Eff.Concurrent.Process
import Control.Lens hiding ((|>), Empty)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))
import qualified Data.Map.Strict as Map
import GHC.Stack
import Data.Kind ()
import Data.Dynamic
import Data.Maybe
import Control.Monad


-- | Execute a 'Process' in the current thread, all child processes spawned by
-- 'spawn' will be executed concurrently using a co-routine based, round-robin
-- scheduler.
schedule
  :: forall r . Eff (Process r ': r) ()
  -> Eff r ()
schedule mainProcessAction =
  do
    y <- runAsCoroutine mainProcessAction
    go 1 (Map.singleton 0 Seq.empty) (Seq.singleton (y, 0))
  where

    go :: ProcessId -> Map.Map ProcessId (Seq Dynamic) -> Seq (OnYield r, ProcessId) -> Eff r ()
    go _newPid _msgQs Empty = return ()

    go newPid msgQs allProcs@((processState, pid) :<| rest) =
      let handleExit =
            if pid == 0 then return ()
            else go newPid
                    (msgQs & at pid .~ Nothing)
                    rest

      in case processState of
             OnDone -> handleExit

             OnRaiseError _ -> handleExit

             OnExitError _ -> handleExit

             OnSendShutdown targetPid k ->
               do let allButTarget =
                        Seq.filter (\(_,e) -> e /= pid && e /= targetPid) allProcs
                      targets =
                        Seq.filter (\(_,e) -> e == targetPid) allProcs
                      suicide = targetPid == pid
                      targetFound = suicide || not (Seq.null targets)
                  if suicide
                   then
                    do nextK <- k ShutdownRequested
                       go newPid
                          msgQs
                          (rest :|> (nextK, pid))
                   else
                    do let deliverTheGoodNews (targetState, tPid) =
                             do nextTargetState <-
                                   case targetState of
                                     OnSendShutdown _ tk ->
                                       tk ShutdownRequested
                                     OnSelf tk ->
                                       tk ShutdownRequested
                                     OnSend _ _ tk ->
                                       tk ShutdownRequested
                                     OnRecv tk ->
                                       tk ShutdownRequested
                                     OnSpawn _ tk ->
                                       tk ShutdownRequested
                                     OnDone -> return OnDone
                                     OnExitError er -> return (OnExitError er)
                                     OnRaiseError er -> return (OnExitError er) -- return (error ("TODO write test "++er))
                                return (nextTargetState, tPid)
                       nextTargets <- traverse deliverTheGoodNews targets
                       nextK <- k (ResumeWith targetFound)
                       go newPid
                          msgQs
                          (allButTarget Seq.>< (nextTargets :|> (nextK, pid)))

             OnSelf k ->
               do nextK <- k (ResumeWith pid)
                  go newPid
                     msgQs
                     (rest :|> (nextK, pid))

             OnSend toPid msg k ->
               do nextK <- k (ResumeWith (msgQs ^. at toPid . to isJust))
                  go newPid
                     (msgQs & at toPid . _Just %~ (:|> msg))
                     (rest :|> (nextK, pid))

             recv@(OnRecv k) ->
               case msgQs ^. at pid of
                 Nothing ->
                   do nextK <- k (OnError (show pid ++ " has no message queue!"))
                      go newPid msgQs (rest :|> (nextK, pid))
                 Just Empty ->
                   if Seq.length rest == 0 then
                     do nextK <- k (OnError ("Process " ++ show pid ++ " deadlocked!"))
                        go newPid msgQs (rest :|> (nextK, pid))
                   else
                     go newPid msgQs (rest :|> (recv, pid))

                 Just (nextMessage :<| restMessages) ->
                   do nextK <- k (ResumeWith nextMessage)
                      go newPid
                         (msgQs & at pid . _Just .~ restMessages)
                         (rest :|> (nextK, pid))

             OnSpawn f k ->
               do nextK <- k (ResumeWith newPid)
                  fk <- runAsCoroutine f
                  go (newPid + 1)
                     (msgQs & at newPid .~ Just Seq.empty)
                     ( rest :|> (nextK, pid) :|> (fk, newPid))

data OnYield r where
  OnSelf :: (ResumeProcess ProcessId -> Eff r (OnYield r))
         -> OnYield r
  OnSpawn :: Eff (Process r ': r) ()
          -> (ResumeProcess ProcessId -> Eff r (OnYield r))
          -> OnYield r
  OnDone :: OnYield r
  OnExitError :: String -> OnYield r
  OnRaiseError :: String -> OnYield r
  OnSend :: ProcessId -> Dynamic
         -> (ResumeProcess Bool -> Eff r (OnYield r))
         -> OnYield r
  OnRecv :: (ResumeProcess Dynamic -> Eff r (OnYield r))
         -> OnYield r
  OnSendShutdown :: ProcessId -> (ResumeProcess Bool -> Eff r (OnYield r)) -> OnYield r

runAsCoroutine :: forall r v . Eff (Process r ': r) v
               -> Eff r (OnYield r)
runAsCoroutine m = handle_relay (const $ return OnDone) cont m
  where
    cont :: Process r x -> (x -> Eff r (OnYield r)) -> Eff r (OnYield r)
    cont SelfPid k = return (OnSelf k)
    cont (Spawn e) k = return (OnSpawn e k)
    cont Shutdown _k = return OnDone
    cont (ExitWithError e) _k = return (OnExitError e)
    cont (RaiseError e) _k = return (OnRaiseError e)
    cont (SendMessage tp msg) k = return (OnSend tp msg k)
    cont ReceiveMessage k = return (OnRecv k)
    cont (SendShutdown pid) k = return (OnSendShutdown pid k)

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
defaultMain go = runLift $ handleLogsWith (schedule go) ($ putStrLn)

_example :: IO ()
_example = defaultMain go
  where
    px :: SchedulerProxy '[Logs String, Lift IO]
    px = SchedulerProxy
    go :: Eff (ConsProcess '[Logs String, Lift IO]) ()
    go = do
           p1 <- spawn $ do
                    logMsg "Hello World!"
                    me <- self px
                    logMsg ("I am: " ++ show me)
                    d <- receiveMessage px
                    let m = fromDyn @String d ""
                    logMsg ("Got: " ++ m)
           void $ sendMessage px p1 (toDyn "huhu")
           return ()
