module SingleThreadedScheduler
where

import           Control.Eff
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                               as Scheduler
import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Dynamic
import           Common

test_pureScheduler :: TestTree
test_pureScheduler = setTravisTestOptions $ testGroup
    "Pure Scheduler"
    [ testCase "two processes, each calculate and report back to main process"
      $   Right (42 :: Int)
      @=? Scheduler.schedulePure
              (do
                  adderChild <- spawn $ do
                      (from, arg1, arg2) <- receiveMessageAs SP
                      sendMessageAs SP from ((arg1 + arg2) :: Int)
                      forever $ void $ receiveMessage SP

                  multChild <- spawn $ do
                      (from, arg1, arg2) <- receiveMessageAs SP
                      sendMessageAs SP from ((arg1 * arg2) :: Int)

                  me <- self SP
                  sendMessageAs SP adderChild (me, 3 :: Int, 4 :: Int)
                  x <- receiveMessageAs @Int SP
                  sendMessageAs SP multChild (me, x, 6 :: Int)
                  receiveMessageAs @Int SP
              )
    ]

test_mainProcessSpawnsAChildAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildAndExitsNormally = setTravisTestOptions
    (testCase
        "spawn a child and exit normally"
        (Scheduler.defaultMain
            (do
                void (spawn (void (receiveMessage singleThreadedIoScheduler)))
                void (exitNormally singleThreadedIoScheduler)
                fail "This should not happen!!"
            )
        )
    )


test_mainProcessSpawnsAChildBothExitNormally :: TestTree
test_mainProcessSpawnsAChildBothExitNormally = setTravisTestOptions
    (testCase
        "spawn a child and let it exit and exit"
        (Scheduler.defaultMain
            (do
                child <- spawn
                    (do
                        void
                            (receiveMessageAs @String singleThreadedIoScheduler)
                        void (exitNormally singleThreadedIoScheduler)
                        error "This should not happen (child)!!"
                    )
                True <- sendMessageChecked singleThreadedIoScheduler
                                           child
                                           (toDyn "test")
                void (exitNormally singleThreadedIoScheduler)
                error "This should not happen!!"
            )
        )
    )
