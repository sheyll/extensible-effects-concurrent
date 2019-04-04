module ForkIOScheduler where

import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Eff.Extend
import           Control.Eff.Loop
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Process.Timer
import           Control.Eff.Concurrent.Process.ForkIOScheduler
                                               as Scheduler
import           Control.Monad                  ( void
                                                , replicateM_
                                                )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Dynamic
import           Common

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
          (Scheduler.defaultMain
              (do
                p1 <- spawn $ foreverCheap busyEffect
                lift (threadDelay 1000)
                void $ spawn $ do
                  lift (threadDelay 1000)
                  doExit
                lift (threadDelay 100000)

                me <- self
                spawn_ (lift (threadDelay 10000) >> sendMessage me ())
                eres <- receiveWithMonitor p1 (selectMessage @())
                case eres of
                  Left  _down -> lift (atomically (putTMVar aVar False))
                  Right ()    -> withMonitor p1 $ \ref -> do
                    sendShutdown p1 (NotRecovered (ProcessError "test 123"))
                    _down <- receiveSelectedMessage (selectProcessDown ref)
                    lift (atomically (putTMVar aVar True))
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
      , void (send (SendShutdown @SchedulerIO 44444 ExitNormally))
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
      (Scheduler.defaultMain
        (void (spawn (void receiveAnyMessage)))
      )
  )

test_mainProcessSpawnsAChildAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildAndExitsNormally = setTravisTestOptions
  (testCase
    "spawn a child and exit normally"
      (Scheduler.defaultMain
        (do
          void (spawn (void receiveAnyMessage))
          void exitNormally
          fail "This should not happen!!"
        )
    )
  )


test_mainProcessSpawnsAChildInABusySendLoopAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildInABusySendLoopAndExitsNormally =
  setTravisTestOptions
    (testCase
      "spawn a child with a busy send loop and exit normally"
        (Scheduler.defaultMain
          (do
            void (spawn (foreverCheap (void (sendMessage 1000 "test"))))
            void exitNormally
            fail "This should not happen!!"
          )
        )
      )


test_mainProcessSpawnsAChildBothReturn :: TestTree
test_mainProcessSpawnsAChildBothReturn = setTravisTestOptions
  (testCase
    "spawn a child and let it return and return"
    (Scheduler.defaultMain
        (do
          child <- spawn (void (receiveMessage @String))
          sendMessage child "test"
          return ()
        )
  ))

test_mainProcessSpawnsAChildBothExitNormally :: TestTree
test_mainProcessSpawnsAChildBothExitNormally = setTravisTestOptions
  (testCase
    "spawn a child and let it exit and exit"
      (Scheduler.defaultMain
           (do
              child <- spawn $ void $ provideInterrupts $ exitOnInterrupt
                (do
                  void (receiveMessage @String)
                  void exitNormally
                  error "This should not happen (child)!!"
                )
              sendMessage child "test"
              void exitNormally
              error "This should not happen!!"
            )
      )
    )

test_timer :: TestTree
test_timer =
  setTravisTestOptions
    $ testCase "flush via timer"
    $ Scheduler.defaultMain
    $ do
        let n = 100
            testMsg :: Float
            testMsg   = 123
            flushMsgs = do
              res <- receiveSelectedAfter (selectDynamicMessageLazy Just) 0
              case res of
                Left  _to -> return ()
                Right _   -> flushMsgs
        me <- self
        spawn_
          (do
            replicateM_ n $ sendMessage me "bad message"
            replicateM_ n $ sendMessage me (3123 :: Integer)
            sendMessage me testMsg
          )
        do
          res <- receiveAfter @Float 1000000
          lift (res @?= Just testMsg)
        flushMsgs
        res <- receiveSelectedAfter (selectDynamicMessageLazy Just) 10000
        case res of
          Left  _ -> return ()
          Right x -> lift (False @? "unexpected message in queue " ++ show x)
        lift (threadDelay 100)
