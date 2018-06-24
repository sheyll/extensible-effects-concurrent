module ForkIOScheduler where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.ForkIOScheduler as Scheduler
import Control.Monad (void, forever)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Dynamic

timeoutSeconds :: Integer -> Timeout
timeoutSeconds seconds = Timeout (seconds * 1000000) (show seconds ++ "s")

test_IOExceptionsIsolated :: TestTree
test_IOExceptionsIsolated =
  localOption
  (timeoutSeconds 30)
    $ testGroup "one process throws an IO exception, the other continues unimpaired"
       [ testCase ("process 2 exits with: "++ howToExit
                    ++ " - while process 1 is busy with: " ++ busyWith)
        $ do aVar <- newEmptyTMVarIO
             Scheduler.defaultMain
               $ do p1 <- spawn $ forever busyEffect
                    lift (threadDelay 1000)
                    void $ spawn $ do lift (threadDelay 1000)
                                      doExit
                    lift (threadDelay 100000)
                    wasStillRunningP1 <- sendShutdown forkIoScheduler p1
                    lift (atomically (putTMVar aVar wasStillRunningP1))

             wasStillRunningP1 <- atomically (takeTMVar aVar)
             assertBool "the other process was still running" wasStillRunningP1

       | (busyWith, busyEffect) <-
         [ ("receiving", void (send (ReceiveMessage @SchedulerIO)))
         , ("sending", void (send (SendMessage @SchedulerIO 44444 (toDyn "test message"))))
         , ("sending shutdown", void (send (SendShutdown @SchedulerIO 44444)))
         , ("selfpid-ing", void (send (SelfPid @SchedulerIO)))
         , ("spawn-ing", void (send (Spawn @SchedulerIO (void (send (ReceiveMessage @SchedulerIO))))))
         ]
       , (howToExit, doExit) <-
         [ ("throw async exception", void (lift (throw UserInterrupt)))
         , ("division by zero", void (return ((123::Int) `div` 0) >>= lift . putStrLn . show ))
         , ("call 'fail'", void (fail "test fail"))
         , ("call 'error'", void (error "test error"))
         ]
       ]

test_mainProcessSpawnsAChildAndReturns :: TestTree
test_mainProcessSpawnsAChildAndReturns =
  localOption
  (timeoutSeconds 30)
  (testCase "spawn a child and return"
   (Scheduler.defaultMain
     (void (spawn (void (receiveMessage forkIoScheduler))))))

test_mainProcessSpawnsAChildAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildAndExitsNormally =
  localOption
  (timeoutSeconds 30)
  (testCase "spawn a child and exit normally"
   (Scheduler.defaultMain
     (do void (spawn (void (receiveMessage forkIoScheduler)))
         void (exitNormally forkIoScheduler)
         fail "This should not happen!!"
     )))


test_mainProcessSpawnsAChildInABusySendLoopAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildInABusySendLoopAndExitsNormally =
  localOption
  (timeoutSeconds 300)
  (testCase "spawn a child with a busy send loop and exit normally"
   (Scheduler.defaultMain
     (do void (spawn (forever (void (sendMessage forkIoScheduler 1000 (toDyn "test")))))
         void (exitNormally forkIoScheduler)
         fail "This should not happen!!"
     )))


test_mainProcessSpawnsAChildBothReturn :: TestTree
test_mainProcessSpawnsAChildBothReturn =
  localOption
  (timeoutSeconds 30)
  (testCase "spawn a child and let it return and return"
   (Scheduler.defaultMain
     (do child <- spawn (void (receiveMessageAs @String forkIoScheduler))
         True <- sendMessage forkIoScheduler child (toDyn "test")
         return ()
     )))

test_mainProcessSpawnsAChildBothExitNormally :: TestTree
test_mainProcessSpawnsAChildBothExitNormally =
  localOption
  (timeoutSeconds 30)
  (testCase "spawn a child and let it exit and exit"
   (Scheduler.defaultMain
     (do child <- spawn
                 (do void (receiveMessageAs @String forkIoScheduler)
                     void (exitNormally forkIoScheduler)
                     error "This should not happen (child)!!"
                 )
         True <- sendMessage forkIoScheduler child (toDyn "test")
         void (exitNormally forkIoScheduler)
         error "This should not happen!!"
     )))
