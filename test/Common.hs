module Common where

import           Control.Concurrent.STM
import           Control.Eff.Concurrent.Process
import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.Log
import           Control.Eff.Lift
import           Control.Monad                  ( void
                                                , when
                                                )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Runners
import           GHC.Stack
import           Control.Lens

setTravisTestOptions :: TestTree -> TestTree
setTravisTestOptions =
  localOption (timeoutSeconds 300) . localOption (NumThreads 1)

timeoutSeconds :: Integer -> Timeout
timeoutSeconds seconds = Timeout (seconds * 1000000) (show seconds ++ "s")

withTestLogC
  :: (e -> LogChannel LogMessage -> IO ())
  -> (IO (e -> IO ()) -> TestTree)
  -> TestTree
withTestLogC doSchedule k = k
  (return
    (\e -> withAsyncLogChannel
      1000
      (multiMessageLogWriter
        (\writeWith ->
          writeWith
            (\m -> when (view lmSeverity m <= debugSeverity) (printLogMessage m)
            )
        )
      )
      (doSchedule e)
    )
  )

untilShutdown :: Member t r => t (ResumeProcess v) -> Eff r ()
untilShutdown pa = do
  r <- send pa
  case r of
    ShutdownRequested _ -> return ()
    _                   -> untilShutdown pa

scheduleAndAssert
  :: forall r
   . (SetMember Lift (Lift IO) r, Member (Logs LogMessage) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> (  (String -> Bool -> Eff (Process r ': r) ())
     -> Eff (Process r ': r) ()
     )
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
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> Eff (Process r ': r) ()
  -> IO ()
applySchedulerFactory factory procAction = do
  scheduler <- factory
  scheduler procAction
