module Common
  ( module Common,
    module Test.Tasty,
    module Test.Tasty.HUnit,
    module Test.Tasty.Runners,
    module Control.Eff.Extend,
    module Control.Monad,
    module GHC.Stack,
    module Control.Concurrent,
    module Control.Concurrent.STM,
    module Control.DeepSeq,
    module Control.Eff,
    module Control.Eff.Concurrent,
    module Data.Default,
    module Data.Foldable,
    module Data.Typeable,
    module Data.Text,
    module Data.Either,
    module Data.Maybe,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Eff
--import           Control.Eff.Log
import Control.Eff.Concurrent
import Control.Eff.Concurrent.Process.ForkIOScheduler as Scheduler
import Control.Eff.Extend
import Control.Monad
import Data.Default
import Data.Either
  ( fromRight,
    isLeft,
    isRight,
  )
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Text
  ( Text,
    pack,
  )
import Data.Typeable hiding (cast)
import GHC.Stack
import qualified System.IO as IO
import Test.Tasty hiding
  ( Timeout,
    defaultMain,
  )
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners

setTravisTestOptions :: TestTree -> TestTree
setTravisTestOptions =
  localOption (timeoutSeconds 60) . localOption (NumThreads 1)

timeoutSeconds :: Integer -> Tasty.Timeout
timeoutSeconds seconds =
  Tasty.Timeout (seconds * 1000000) (show seconds ++ "s")

runTestCase :: TestName -> Eff Effects () -> TestTree
runTestCase msg et =
  testCase msg $ do
    lw <- stdoutLogWriter renderConsoleMinimalisticWide
    runLift $
      withRichLogging lw "unit-tests" local0 allLogEvents $
        Scheduler.schedule $
          handleInterrupts
            onInt
            et
  where
    onInt = lift . assertFailure . show . MkUnhandledProcessInterrupt

withTestLogC :: (e -> IO ()) -> (IO (e -> IO ()) -> TestTree) -> TestTree
withTestLogC doSchedule k = k (return doSchedule)

untilInterrupted :: Member t r => t (ResumeProcess v) -> Eff r ()
untilInterrupted pa = do
  r <- send pa
  case r of
    Interrupted _ -> return ()
    _ -> untilInterrupted pa

scheduleAndAssert ::
  forall r.
  (IoLogging r) =>
  IO (Eff (Processes r) () -> IO ()) ->
  ((String -> Bool -> Eff (Processes r) ()) -> Eff (Processes r) ()) ->
  IO ()
scheduleAndAssert schedulerFactory testCaseAction = withFrozenCallStack $ do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  resultVar <- newEmptyTMVarIO
  void
    ( applySchedulerFactory
        schedulerFactory
        ( testCaseAction
            (\title cond -> lift (atomically (putTMVar resultVar (title, cond))))
        )
    )
  (title, result) <- atomically (takeTMVar resultVar)
  assertBool title result

applySchedulerFactory ::
  forall r.
  (IoLogging r) =>
  IO (Eff (Processes r) () -> IO ()) ->
  Eff (Processes r) () ->
  IO ()
applySchedulerFactory factory procAction = do
  scheduler <- factory
  scheduler (procAction >> lift (threadDelay 20000))

assertShutdown ::
  (Member Logs r, HasCallStack, HasProcesses r q, Lifted IO r) =>
  ProcessId ->
  ShutdownReason ->
  Eff r ()
assertShutdown p r = do
  unlinkProcess p
  m <- monitor p
  sendShutdown p r
  logInfo (LABEL "awaitProcessDown" p) m
  logCallStack debugSeverity
  receiveSelectedMessage (selectProcessDown m)
    >>= lift . assertEqual "bad exit reason" (MkUnhandledProcessExit r) . MkUnhandledProcessExit . downReason

awaitProcessDown ::
  (Member Logs r, HasCallStack, HasProcesses r q) =>
  ProcessId ->
  Eff r ProcessDown
awaitProcessDown p = do
  m <- monitor p
  logInfo (LABEL "awaitProcessDown" p) m
  logCallStack debugSeverity
  receiveSelectedMessage (selectProcessDown m)

awaitProcessDownAny ::
  (Member Logs r, HasCallStack, HasProcesses r q) =>
  Eff r ProcessDown
awaitProcessDownAny = do
  logInfo (MSG "awaitProcessDownAny")
  logCallStack debugSeverity
  receiveMessage
