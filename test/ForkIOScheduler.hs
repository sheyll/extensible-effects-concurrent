module ForkIOScheduler where

import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.ForkIOScheduler as Scheduler
import Control.Monad (void)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Dynamic

timeoutSeconds :: Integer -> Timeout
timeoutSeconds seconds = Timeout (seconds * 1000000) (show seconds ++ "s")

test_mainProcessSpawnsAChildAndReturns :: TestTree
test_mainProcessSpawnsAChildAndReturns =
  localOption
  (timeoutSeconds 2)
  (testCase "spawn a child and return"
   (Scheduler.defaultMain
     (void (spawn (void (receiveMessage forkIoScheduler))))))


test_mainProcessSpawnsAChildAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildAndExitsNormally =
  localOption
  (timeoutSeconds 2)
  (testCase "spawn a child and exit normally"
   (Scheduler.defaultMain
     (do void (spawn (void (receiveMessage forkIoScheduler)))
         void (exitNormally forkIoScheduler)
         fail "This should not happen!!"
     )))


test_mainProcessSpawnsAChildBothReturn :: TestTree
test_mainProcessSpawnsAChildBothReturn =
  localOption
  (timeoutSeconds 2)
  (testCase "spawn a child and let it return and return"
   (Scheduler.defaultMain
     (do child <- spawn (void (receiveMessageAs @String forkIoScheduler))
         True <- sendMessage forkIoScheduler child (toDyn "test")
         return ()
     )))

test_mainProcessSpawnsAChildBothExitNormally :: TestTree
test_mainProcessSpawnsAChildBothExitNormally =
  localOption
  (timeoutSeconds 2)
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
