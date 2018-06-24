module Common where

import Control.Concurrent.STM
import Control.Eff.Concurrent.Process
import Control.Eff
import Control.Eff.Log
import Control.Eff.Lift
import Control.Monad (void)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners

setTravisTestOptions :: TestTree -> TestTree
setTravisTestOptions = localOption (timeoutSeconds 300) . localOption (NumThreads 1)

timeoutSeconds :: Integer -> Timeout
timeoutSeconds seconds = Timeout (seconds * 1000000) (show seconds ++ "s")

withTestLogC
  :: (e -> LogChannel String -> IO ()) -> (IO (e -> IO ()) -> TestTree) -> TestTree
withTestLogC doSchedule k =
  withResource
  testLogC
  testLogJoin
  (\ logCFactory ->
       k (return
            (\e ->
                do logC <- logCFactory
                   doSchedule e logC)))

testLogC :: IO (LogChannel String)
testLogC =
  filterLogChannel (\m -> take (length logPrefix) m == logPrefix)
  <$>
  forkLogger 1 (putStrLn)  Nothing

testLogJoin :: LogChannel String -> IO ()
testLogJoin = joinLogChannel Nothing

tlog :: Member (Logs String) r => String -> Eff r ()
tlog = logMsg . (logPrefix ++)

logPrefix :: String
logPrefix = "[TEST] "

untilShutdown
  :: Member t r => t (ResumeProcess v) -> Eff r ()
untilShutdown pa = do
  r <- send pa
  case r of
    ShutdownRequested -> return ()
    _ -> untilShutdown pa

scheduleAndAssert :: forall r .
                    (SetMember Lift (Lift IO) r, Member (Logs String) r)
                  => IO (Eff (Process r ': r) () -> IO ())
                  -> ((String -> Bool -> Eff (Process r ': r) ()) -> Eff (Process r ': r) ())
                  -> IO ()
scheduleAndAssert schedulerFactory testCaseAction =
  do resultVar <- newEmptyTMVarIO
     void (applySchedulerFactory schedulerFactory
            (testCaseAction (\ title cond ->
                               do lift (atomically (putTMVar resultVar (title, cond))))))
     (title, result) <- atomically (takeTMVar resultVar)
     assertBool title result

applySchedulerFactory :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r)
                      => IO (Eff (Process r ': r) () -> IO ()) -> Eff (Process r ': r) () -> IO ()
applySchedulerFactory factory procAction =
  do scheduler <- factory
     scheduler procAction
