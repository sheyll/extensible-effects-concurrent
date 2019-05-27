module Common where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Eff
import Control.Eff.Concurrent hiding (Timeout)
import Control.Eff.Concurrent.Process.ForkIOScheduler as Scheduler
import Control.Eff.Extend
import Control.Monad (void)
import GHC.Stack
import Test.Tasty 
import Test.Tasty.HUnit
import Test.Tasty.Runners

setTravisTestOptions :: TestTree -> TestTree
setTravisTestOptions = localOption (timeoutSeconds 60) . localOption (NumThreads 1)

timeoutSeconds :: Integer -> Timeout
timeoutSeconds seconds = Timeout (seconds * 1000000) (show seconds ++ "s")

runTestCase :: TestName -> Eff InterruptableProcEff () -> TestTree
runTestCase msg =
  testCase msg .
  runLift . withTraceLogging "unit-tests" local0 allLogMessages . Scheduler.schedule . handleInterrupts onInt
  where
    onInt = lift . assertFailure . show

withTestLogC :: (e -> IO ()) -> (IO (e -> IO ()) -> TestTree) -> TestTree
withTestLogC doSchedule k = k (return doSchedule)

untilInterrupted :: Member t r => t (ResumeProcess v) -> Eff r ()
untilInterrupted pa = do
  r <- send pa
  case r of
    Interrupted _ -> return ()
    _ -> untilInterrupted pa

scheduleAndAssert ::
     forall r. (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> ((String -> Bool -> Eff (InterruptableProcess r) ()) -> Eff (InterruptableProcess r) ())
  -> IO ()
scheduleAndAssert schedulerFactory testCaseAction =
  withFrozenCallStack $ do
    resultVar <- newEmptyTMVarIO
    void
      (applySchedulerFactory
         schedulerFactory
         (testCaseAction (\title cond -> lift (atomically (putTMVar resultVar (title, cond))))))
    (title, result) <- atomically (takeTMVar resultVar)
    assertBool title result

applySchedulerFactory ::
     forall r. (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> Eff (InterruptableProcess r) ()
  -> IO ()
applySchedulerFactory factory procAction = do
  scheduler <- factory
  scheduler (procAction >> lift (threadDelay 20000))