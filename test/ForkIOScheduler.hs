module ForkIOScheduler where

import Common
import Control.Concurrent.Async
import Control.Eff.Concurrent.Process.ForkIOScheduler as Scheduler
import Control.Exception

test_IOExceptionsIsolated :: TestTree
test_IOExceptionsIsolated =
  setTravisTestOptions $
    testGroup
      "one process throws an IO exception, the other continues unimpaired"
      [ testCase
          ( "process 2 exits with: "
              ++ howToExit
              ++ " - while process 1 is busy with: "
              ++ busyWith
          )
          $ do
            aVar <- newEmptyTMVarIO
            ( Scheduler.defaultMain
                ( do
                    p1 <- spawn "test" $ foreverCheap busyEffect
                    lift (threadDelay 1000)
                    void $ spawn "test" $ do
                      lift (threadDelay 1000)
                      doExit
                    lift (threadDelay 100000)
                    me <- self
                    spawn_ "test" (lift (threadDelay 10000) >> sendMessage me ())
                    resultOrError <- receiveWithMonitor p1 (selectMessage @())
                    case resultOrError of
                      Left _down -> lift (atomically (putTMVar aVar False))
                      Right () -> withMonitor p1 $ \ref -> do
                        sendShutdown p1 (interruptToExit (ErrorInterrupt "test 123"))
                        _down <- receiveSelectedMessage (selectProcessDown ref)
                        lift (atomically (putTMVar aVar True))
                )
              )
            wasStillRunningP1 <- atomically (takeTMVar aVar)
            assertBool "the other process was still running" wasStillRunningP1
        | (busyWith, busyEffect) <-
            [ ( "receiving",
                void (send (ReceiveSelectedMessage @BaseEffects selectAnyMessage))
              ),
              ( "sending",
                void (send (SendMessage @BaseEffects 44444 (toMessage ("test message" :: String))))
              ),
              ( "sending shutdown",
                void (send (SendShutdown @BaseEffects 44444 ExitNormally))
              ),
              ("selfpid-ing", void (send (SelfPid @BaseEffects))),
              ( "spawn-ing",
                void
                  ( send
                      ( Spawn @BaseEffects
                          "test"
                          ( void
                              (send (ReceiveSelectedMessage @BaseEffects selectAnyMessage))
                          )
                      )
                  )
              )
            ],
          (howToExit, doExit) <-
            [ ("throw async exception", void (lift (throw UserInterrupt))),
              ("cancel process", void (lift (throw AsyncCancelled))),
              ("division by zero", void ((lift . print) ((123 :: Int) `div` 0))),
              ("call 'error'", void (error "test error"))
            ]
      ]

test_mainProcessSpawnsAChildAndReturns :: TestTree
test_mainProcessSpawnsAChildAndReturns =
  setTravisTestOptions
    ( testCase
        "spawn a child and return"
        ( Scheduler.defaultMain
            (void (spawn "test" (void receiveAnyMessage)))
        )
    )

test_mainProcessSpawnsAChildAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildAndExitsNormally =
  setTravisTestOptions
    ( testCase
        "spawn a child and exit normally"
        ( Scheduler.defaultMain
            ( do
                void (spawn "test" (void receiveAnyMessage))
                void exitNormally
            )
        )
    )

test_mainProcessSpawnsAChildInABusySendLoopAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildInABusySendLoopAndExitsNormally =
  setTravisTestOptions
    ( testCase
        "spawn a child with a busy send loop and exit normally"
        ( Scheduler.defaultMain
            ( do
                void (spawn "test" (foreverCheap (void (sendMessage 1000 ("test" :: String)))))
                void exitNormally
                error "This should not happen!!"
            )
        )
    )

test_mainProcessSpawnsAChildBothReturn :: TestTree
test_mainProcessSpawnsAChildBothReturn =
  setTravisTestOptions
    ( testCase
        "spawn a child and let it return and return"
        ( Scheduler.defaultMain
            ( do
                child <- spawn "test" (void (receiveMessage @String))
                sendMessage child ("test" :: String)
                return ()
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
                  spawn "test" $ void $ provideInterrupts $
                    exitOnInterrupt
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

test_timer :: TestTree
test_timer =
  setTravisTestOptions
    $ testCase "flush via timer"
    $ Scheduler.defaultMain
    $ do
      let n = 100
          testMsg :: Float
          testMsg = 123
          flushMessagesLoop = do
            res <- receiveSelectedAfter (selectDynamicMessage Just) 0
            case res of
              Left _to -> return ()
              Right _ -> flushMessagesLoop
      me <- self
      spawn_
        "test-worker"
        ( do
            replicateM_ n $ sendMessage me ("bad message" :: String)
            replicateM_ n $ sendMessage me (3123 :: Integer)
            sendMessage me testMsg
        )
      do
        res <- receiveAfter @Float 1000000
        lift (res @?= Just testMsg)
      flushMessagesLoop
      res <- receiveSelectedAfter (selectDynamicMessage Just) 10000
      case res of
        Left _ -> return ()
        Right x -> lift (False @? "unexpected message in queue " ++ show x)
      lift (threadDelay 100)
