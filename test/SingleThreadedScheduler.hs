module SingleThreadedScheduler
where

import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                               as Scheduler
import           Control.Monad                  ( void
                                                )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Dynamic
import           Common

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
