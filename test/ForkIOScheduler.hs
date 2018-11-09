module ForkIOScheduler where

import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Eff.Extend
import           Control.Eff.Lift
import           Control.Eff.Log
import           Control.Eff.Loop
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Process.ForkIOScheduler
                                               as Scheduler
import           Control.Monad                  ( void )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Dynamic
import           Common
import           Data.Default

test_IOExceptionsIsolated :: TestTree
test_IOExceptionsIsolated = setTravisTestOptions $ testGroup
  "one process throws an IO exception, the other continues unimpaired"
  [ testCase
        (  "process 2 exits with: "
        ++ howToExit
        ++ " - while process 1 is busy with: "
        ++ busyWith
        )
      $ do
          aVar <- newEmptyTMVarIO
          withAsyncLogChannel
            1000
            def
            (Scheduler.defaultMainWithLogChannel
              (do
                p1 <- spawn $ foreverCheap busyEffect
                lift (threadDelay 1000)
                void $ spawn $ do
                  lift (threadDelay 1000)
                  doExit
                lift (threadDelay 100000)
                wasStillRunningP1 <- sendShutdownChecked
                  forkIoScheduler
                  p1
                  (NotRecovered ExitNormally)
                lift (atomically (putTMVar aVar wasStillRunningP1))
              )
            )

          wasStillRunningP1 <- atomically (takeTMVar aVar)
          assertBool "the other process was still running" wasStillRunningP1
  | (busyWith , busyEffect) <-
    [ ( "receiving"
      , void (send (ReceiveSelectedMessage @SchedulerIO selectAnyMessageLazy))
      )
    , ( "sending"
      , void (send (SendMessage @SchedulerIO 44444 (toDyn "test message")))
      )
    , ( "sending shutdown"
      , void
        (send (SendShutdown @SchedulerIO 44444 (NotRecovered ExitNormally)))
      )
    , ("selfpid-ing", void (send (SelfPid @SchedulerIO)))
    , ( "spawn-ing"
      , void
        (send
          (Spawn @SchedulerIO
            (void
              (send (ReceiveSelectedMessage @SchedulerIO selectAnyMessageLazy))
            )
          )
        )
      )
    ]
  , (howToExit, doExit    ) <-
    [ ("throw async exception", void (lift (throw UserInterrupt)))
    , ("cancel process"       , void (lift (throw AsyncCancelled)))
    , ("division by zero"     , void ((lift . print) ((123 :: Int) `div` 0)))
    , ("call 'fail'"          , void (fail "test fail"))
    , ("call 'error'"         , void (error "test error"))
    ]
  ]

test_mainProcessSpawnsAChildAndReturns :: TestTree
test_mainProcessSpawnsAChildAndReturns = setTravisTestOptions
  (testCase
    "spawn a child and return"
    (withAsyncLogChannel
      1000
      def
      (Scheduler.defaultMainWithLogChannel
        (void (spawn (void (receiveAnyMessage forkIoScheduler))))
      )
    )
  )

test_mainProcessSpawnsAChildAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildAndExitsNormally = setTravisTestOptions
  (testCase
    "spawn a child and exit normally"
    (withAsyncLogChannel
      1000
      def
      (Scheduler.defaultMainWithLogChannel
        (do
          void (spawn (void (receiveAnyMessage forkIoScheduler)))
          void (exitNormally forkIoScheduler)
          fail "This should not happen!!"
        )
      )
    )
  )


test_mainProcessSpawnsAChildInABusySendLoopAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildInABusySendLoopAndExitsNormally =
  setTravisTestOptions
    (testCase
      "spawn a child with a busy send loop and exit normally"
      (withAsyncLogChannel
        1000
        def
        (Scheduler.defaultMainWithLogChannel
          (do
            void
              (spawn
                (foreverCheap
                  (void (sendMessage forkIoScheduler 1000 (toDyn "test")))
                )
              )
            void (exitNormally forkIoScheduler)
            fail "This should not happen!!"
          )
        )
      )
    )


test_mainProcessSpawnsAChildBothReturn :: TestTree
test_mainProcessSpawnsAChildBothReturn = setTravisTestOptions
  (testCase
    "spawn a child and let it return and return"
    (withAsyncLogChannel
      1000
      def
      (Scheduler.defaultMainWithLogChannel
        (do
          child <- spawn (void (receiveMessageAs @String forkIoScheduler))
          True  <- sendMessageChecked forkIoScheduler child (toDyn "test")
          return ()
        )
      )
    )
  )

test_mainProcessSpawnsAChildBothExitNormally :: TestTree
test_mainProcessSpawnsAChildBothExitNormally = setTravisTestOptions
  (testCase
    "spawn a child and let it exit and exit"
    (withAsyncLogChannel
      1000
      def
      (Scheduler.defaultMainWithLogChannel
        (do
          child <- spawn
            (do
              void (receiveMessageAs @String forkIoScheduler)
              void (exitNormally forkIoScheduler)
              error "This should not happen (child)!!"
            )
          True <- sendMessageChecked forkIoScheduler child (toDyn "test")
          void (exitNormally forkIoScheduler)
          error "This should not happen!!"
        )
      )
    )
  )
