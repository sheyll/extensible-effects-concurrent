module Common
  ( module Common
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , module Test.Tasty.Runners
  , module Control.Eff.Extend
  , module Control.Monad
  , module GHC.Stack
  , module Control.Concurrent
  , module Control.Concurrent.STM
  , module Control.DeepSeq
  , module Control.Eff
  , module Control.Eff.Concurrent
  , module Control.Eff.Concurrent.Misc
  , module Data.Default
  , module Data.Foldable
  , module Data.Typeable
  , module Data.Text
  , module Data.Either
  , module Data.Maybe
  , module Data.Type.Pretty
  )
where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Eff
--import           Control.Eff.Log
import           Control.Eff.Concurrent
import           Control.Eff.Concurrent.Misc
import           Control.Eff.Concurrent.Process.ForkIOScheduler
                                               as Scheduler
import           Control.Eff.Extend
import           Control.Monad
import           Control.Lens
import           Data.Default
import           Data.Foldable
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as T
import           Data.Typeable           hiding ( cast )
import           GHC.Stack
import           Test.Tasty              hiding ( Timeout
                                                , defaultMain
                                                )
import qualified Test.Tasty                    as Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Runners
import           Data.Either                    ( fromRight
                                                , isLeft
                                                , isRight
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Type.Pretty

setTravisTestOptions :: TestTree -> TestTree
setTravisTestOptions =
  localOption (timeoutSeconds 60) . localOption (NumThreads 1)

timeoutSeconds :: Integer -> Tasty.Timeout
timeoutSeconds seconds =
  Tasty.Timeout (seconds * 1000000) (show seconds ++ "s")

runTestCase :: TestName -> Eff Effects () -> TestTree
runTestCase msg =
  testCase msg
    . runLift
    . withIoLogging (stdoutLogWriter renderMinimalisticWide) "unit-tests" local0 allLogMessages
    . Scheduler.schedule
    . handleInterrupts onInt
  where onInt = lift . assertFailure . show

-- | Render a 'LogMessage' human readable, for console logging
renderMinimalisticWide :: LogMessageRenderer T.Text
renderMinimalisticWide l =
  T.unwords $ filter
    (not . T.null)
    [ let s = l ^. lmSeverity . to (T.pack . show)
      in s <> T.replicate (max 0 (15 - T.length s)) " "
    , let p = fromMaybe " no proc " (l ^. lmProcessId)
      in p <> T.replicate (max 0 (55 - T.length p)) " "
    , let msg = l^.lmMessage
      in  msg <> T.replicate (max 0 (100 - T.length msg)) " "
    -- , fromMaybe "" (renderLogMessageSrcLoc l)
    ]


withTestLogC :: (e -> IO ()) -> (IO (e -> IO ()) -> TestTree) -> TestTree
withTestLogC doSchedule k = k (return doSchedule)

untilInterrupted :: Member t r => t (ResumeProcess v) -> Eff r ()
untilInterrupted pa = do
  r <- send pa
  case r of
    Interrupted _ -> return ()
    _             -> untilInterrupted pa

scheduleAndAssert
  :: forall r
   . (LogIo r)
  => IO (Eff (Processes r) () -> IO ())
  -> ((String -> Bool -> Eff (Processes r) ()) -> Eff (Processes r) ())
  -> IO ()
scheduleAndAssert schedulerFactory testCaseAction = withFrozenCallStack $ do
  resultVar <- newEmptyTMVarIO
  void
    (applySchedulerFactory
      schedulerFactory
      (testCaseAction
        (\title cond -> lift (atomically (putTMVar resultVar (title, cond))))
      )
    )
  (title, result) <- atomically (takeTMVar resultVar)
  assertBool title result

applySchedulerFactory
  :: forall r
   . (LogIo r)
  => IO (Eff (Processes r) () -> IO ())
  -> Eff (Processes r) ()
  -> IO ()
applySchedulerFactory factory procAction = do
  scheduler <- factory
  scheduler (procAction >> lift (threadDelay 20000))

assertShutdown
  :: (Member Logs r, HasCallStack, HasProcesses r q, Lifted IO r)
  => ProcessId
  -> Interrupt 'NoRecovery
  -> Eff r ()
assertShutdown p r = do
  unlinkProcess p
  m <- monitor p
  sendShutdown p r
  logInfo
    (  "awaitProcessDown: "
    <> pack (show p)
    <> " "
    <> pack (show m)
    )
  logCallStack debugSeverity
  receiveSelectedMessage (selectProcessDown m)
    >>= lift . assertEqual "bad exit reason" r . downReason

awaitProcessDown
  :: (Member Logs r, HasCallStack, HasProcesses r q)
  => ProcessId
  -> Eff r ProcessDown
awaitProcessDown p = do
  m <- monitor p
  logInfo
    (  "awaitProcessDown: "
    <> pack (show p)
    <> " "
    <> pack (show m)
    )
  logCallStack informationalSeverity
  receiveSelectedMessage (selectProcessDown m)

awaitProcessDownAny
  :: (Member Logs r, HasCallStack, HasProcesses r q)
  => Eff r ProcessDown
awaitProcessDownAny = do
  logInfo
    (  "awaitProcessDownAny: "
    <> " "
    <> pack (prettyCallStack callStack)
    )
  logCallStack informationalSeverity
  z <- receiveMessage
  return z


