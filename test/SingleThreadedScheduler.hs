module SingleThreadedScheduler where

import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.SingleThreadedScheduler as Scheduler
import Control.Monad (void)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Dynamic

timeoutSeconds :: Integer -> Timeout
timeoutSeconds seconds = Timeout (seconds * 1000000) (show seconds ++ "s")

test_mainProcessSpawnsAChildAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildAndExitsNormally =
  localOption
  (timeoutSeconds 2)
  (testCase "spawn a child and exit normally"
   (Scheduler.defaultMain
     (do void (spawn (void (receiveMessage singleThreadedIoScheduler)))
         void (exitNormally singleThreadedIoScheduler)
         fail "This should not happen!!"
     )))

test_mainProcessSpawnsAChildBothExitNormally :: TestTree
test_mainProcessSpawnsAChildBothExitNormally =
  localOption
  (timeoutSeconds 30)
  (testCase "spawn a child and let it exit and exit"
   (Scheduler.defaultMain
     (do child <- spawn
                 (do void (receiveMessageAs @String singleThreadedIoScheduler)
                     void (exitNormally singleThreadedIoScheduler)
                     error "This should not happen (child)!!"
                 )
         True <- sendMessage singleThreadedIoScheduler child (toDyn "test")
         void (exitNormally singleThreadedIoScheduler)
         error "This should not happen!!"
     )))
