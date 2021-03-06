module SingleThreadedScheduler where

import Common
import Control.Eff.Concurrent.Process.SingleThreadedScheduler as Scheduler

test_pureScheduler :: TestTree
test_pureScheduler =
  setTravisTestOptions $
    testGroup
      "Pure Scheduler"
      [ testCase "two processes, each calculate and report back to main process" $
          Right (42 :: Int)
            @=? Scheduler.schedulePure
              ( do
                  adderChild <- spawn "test" $ do
                    (from, arg1, arg2) <- receiveMessage
                    sendMessage from ((arg1 + arg2) :: Int)
                    foreverCheap $ void $ receiveAnyMessage
                  multiplierChild <- spawn "test" $ do
                    (from, arg1, arg2) <- receiveMessage
                    sendMessage from ((arg1 * arg2) :: Int)
                  me <- self
                  sendMessage adderChild (me, 3 :: Int, 4 :: Int)
                  x <- receiveMessage @Int
                  sendMessage multiplierChild (me, x, 6 :: Int)
                  receiveMessage @Int
              )
      ]

test_mainProcessSpawnsAChildAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildAndExitsNormally =
  setTravisTestOptions
    ( testCase
        "spawn a child and exit normally"
        ( Scheduler.defaultMain
            ( do
                void (spawn "test" (void receiveAnyMessage))
                void exitNormally
                error "This should not happen!!"
            )
        )
    )

test_mainProcessSpawnsAChildBothExitNormally :: TestTree
test_mainProcessSpawnsAChildBothExitNormally =
  setTravisTestOptions
    ( testCase
        "spawn a child and let it exit and exit"
        ( Scheduler.defaultMain
            ( do
                child <-
                  spawn
                    "test"
                    ( do
                        void (receiveMessage @String)
                        void exitNormally
                        error "This should not happen (child)!!"
                    )
                sendMessage child ("test" :: String)
                void exitNormally
                error "This should not happen!!"
            )
        )
    )
