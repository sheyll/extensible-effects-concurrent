module ForkIOScheduler where

import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.ForkIOScheduler as Scheduler
import Control.Monad (void, forever)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Dynamic

timeoutSeconds :: Integer -> Timeout
timeoutSeconds seconds = Timeout (seconds * 1000000) (show seconds ++ "s")

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
  (testCase "spawn a child and let it return and return"
   (Scheduler.defaultMain
     (do child <- spawn
                 (do void (receiveMessageAs @String forkIoScheduler)
                     exitNormally forkIoScheduler
                     error "This should not happen (child)!!"
                 )
         True <- sendMessage forkIoScheduler child (toDyn "test")
         void (exitNormally forkIoScheduler)
         error "This should not happen!!"
     )))
