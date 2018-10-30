module ProcessBehaviourTestCases where

import           Data.List                      ( sort )
import           Data.Dynamic
import           Data.Foldable                  ( traverse_ )
import           Data.Dynamic                   ( fromDynamic )
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Client
import           Control.Eff.Concurrent.Api.Server
import qualified Control.Eff.Concurrent.Process.ForkIOScheduler
                                               as ForkIO
import qualified Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                               as SingleThreaded
import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.Log
import           Control.Eff.Loop
import           Control.Eff.Lift
import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common

test_forkIo :: TestTree
test_forkIo = setTravisTestOptions
  (withTestLogC ForkIO.schedule
                (\factory -> testGroup "ForkIOScheduler" [allTests factory])
  )

test_singleThreaded :: TestTree
test_singleThreaded = setTravisTestOptions
  (withTestLogC
    (\e logC ->
        -- void (runLift (logToChannel logC (SingleThreaded.schedule (return ()) e)))
      let runEff :: Eff '[Logs LogMessage, Lift IO] a -> IO a
          runEff = runLift . logToChannel logC
      in  void $ SingleThreaded.scheduleM runEff yield e
    )
    (\factory -> testGroup "SingleThreadedScheduler" [allTests factory])
  )


allTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> TestTree
allTests schedulerFactory = localOption
  (timeoutSeconds 300)
  (testGroup
    "Process"
    [ errorTests schedulerFactory
    , sendShutdownTests schedulerFactory
    , concurrencyTests schedulerFactory
    , exitTests schedulerFactory
    , pingPongTests schedulerFactory
    , yieldLoopTests schedulerFactory
    , selectiveReceiveTests schedulerFactory
    ]
  )


data ReturnToSender
  deriving Typeable

data instance Api ReturnToSender r where
  ReturnToSender :: ProcessId -> String -> Api ReturnToSender ('Synchronous Bool)
  StopReturnToSender :: Api ReturnToSender ('Synchronous ())

deriving instance Show (Api ReturnToSender x)

deriving instance Typeable (Api ReturnToSender x)

returnToSender
  :: forall q r
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Server ReturnToSender
  -> String
  -> Eff r Bool
returnToSender px toP msg = do
  me <- self px
  call px toP (ReturnToSender me msg)
  msgEcho <- receiveMessageAs @String px
  return (msgEcho == msg)

stopReturnToSender
  :: forall q r
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Server ReturnToSender
  -> Eff r ()
stopReturnToSender px toP = do
  me <- self px
  call px toP StopReturnToSender

returnToSenderServer
  :: forall q r
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Member (Logs LogMessage) q
     )
  => SchedulerProxy q
  -> Eff r (Server ReturnToSender)
returnToSenderServer px = asServer <$> spawn
  (serve px $ ApiHandler
    { _handleCall      = \m k -> case m of
      StopReturnToSender -> k () >> exitNormally px
      ReturnToSender fromP echoMsg ->
        sendMessageChecked px fromP (toDyn echoMsg)
          >>= (\res -> yieldProcess px >> k res)
    , _handleCast      = logWarning . show
    , _handleTerminate = logWarning . show
    }
  )

selectiveReceiveTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> TestTree
selectiveReceiveTests schedulerFactory = setTravisTestOptions
  (testGroup
    "selective receive tests"
    [ testCase "send 10 messages (from 1..10) and receive messages from 10 to 1"
    $ applySchedulerFactory schedulerFactory
    $ do
        let
          nMax = 10
          receiverLoop donePid = go nMax
           where
            go :: Int -> Eff (Process r ': r) ()
            go 0 = sendMessageAs SP donePid True
            go n = do
              void $ receiveMessageSuchThat
                SP
                (MessageSelector
                  (\m -> do
                    i <- fromDynamic m
                    if i == n then Just i else Nothing
                  )
                )
              go (n - 1)

          senderLoop receviverPid =
            traverse_ (sendMessageAs SP receviverPid) [1 .. nMax]

        me          <- self SP
        receiverPid <- spawn (receiverLoop me)
        spawn_ (senderLoop receiverPid)
        ok <- receiveMessageAs @Bool SP
        lift (ok @? "selective receive failed")
    , testCase "receive a message while waiting for a call reply"
    $ applySchedulerFactory schedulerFactory
    $ do
        srv <- returnToSenderServer SP
        ok  <- returnToSender SP srv "test"
        ()  <- stopReturnToSender SP srv
        lift (ok @? "selective receive failed")
    ]
  )


yieldLoopTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> TestTree
yieldLoopTests schedulerFactory
  = let maxN = 100000
    in
      setTravisTestOptions
        (testGroup
          "yield tests"
          [ testCase
            "yield many times (replicateM_)"
            (applySchedulerFactory schedulerFactory
                                   (replicateM_ maxN (yieldProcess SP))
            )
          , testCase
            "yield many times (forM_)"
            (applySchedulerFactory
              schedulerFactory
              (forM_ [1 :: Int .. maxN] (\_ -> yieldProcess SP))
            )
          , testCase
            "construct an effect with an exit first, followed by many yields"
            (applySchedulerFactory
              schedulerFactory
              (do
                void (exitNormally SP)
                replicateM_ 1000000000000 (yieldProcess SP)
              )
            )
          ]
        )


data Ping = Ping ProcessId
data Pong = Pong
  deriving (Eq, Show)

pingPongTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> TestTree
pingPongTests schedulerFactory = testGroup
  "Yield Tests"
  [ testCase "ping pong a message between two processes, both don't yield"
  $ applySchedulerFactory schedulerFactory
  $ do
      let pongProc = foreverCheap $ do
            Ping pinger <- receiveMessageAs SP
            sendMessageAs SP pinger Pong
          pingProc ponger parent = do
            me <- self SP
            sendMessageAs SP ponger (Ping me)
            Pong <- receiveMessageAs SP
            sendMessageAs SP parent True
      pongPid <- spawn pongProc
      me      <- self SP
      spawn_ (pingProc pongPid me)
      ok <- receiveMessageAs @Bool SP
      lift (ok @? "ping pong failed")
  , testCase "ping pong a message between two processes, with massive yielding"
  $ applySchedulerFactory schedulerFactory
  $ do
      yieldProcess SP
      let pongProc = foreverCheap $ do
            yieldProcess SP
            Ping pinger <- receiveMessageAs SP
            yieldProcess SP
            sendMessageAs SP pinger Pong
            yieldProcess SP
          pingProc ponger parent = do
            yieldProcess SP
            me <- self SP
            yieldProcess SP
            sendMessageAs SP ponger (Ping me)
            yieldProcess SP
            Pong <- receiveMessageAs SP
            yieldProcess SP
            sendMessageAs SP parent True
            yieldProcess SP
      yieldProcess SP
      pongPid <- spawn pongProc
      yieldProcess SP
      me <- self SP
      yieldProcess SP
      spawn_ (pingProc pongPid me)
      yieldProcess SP
      ok <- receiveMessageAs @Bool SP
      yieldProcess SP
      lift (ok @? "ping pong failed")
      yieldProcess SP
  , testCase
    "the first message is not delayed, not even in cooperative scheduling (because of yield)"
  $ applySchedulerFactory schedulerFactory
  $ do
      pongVar <- lift newEmptyMVar
      let pongProc = foreverCheap $ do
            Pong <- receiveMessageAs SP
            lift (putMVar pongVar Pong)
      ponger <- spawn pongProc
      sendMessageAs SP ponger Pong
      let waitLoop = do
            p <- lift (tryTakeMVar pongVar)
            case p of
              Nothing -> do
                yieldProcess SP
                waitLoop
              Just r -> return r
      p <- waitLoop
      lift (p == Pong @? "ping pong failed")
  ]

errorTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> TestTree
errorTests schedulerFactory
  = let
      px :: SchedulerProxy r
      px = SchedulerProxy
    in
      testGroup
        "causing and handling errors"
        [ testGroup
          "raiseError"
          [ testCase "unhandled raiseError"
          $ applySchedulerFactory schedulerFactory
          $ do
              void $ raiseError px "test error"
              error "This should not happen"
          , testCase "catch raiseError 1"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> catchRaisedError
              px
              (assertEff "error must be caught" . (== "test error 2"))
              (void (raiseError px "test error 2"))
          , testCase "catch raiseError from a long sub block"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> catchRaisedError
              px
              (assertEff "error must be caught" . (== "test error 3"))
              (do
                void (replicateM 100000 (void (self px)))
                void (raiseError px "test error 3")
              )
          ]
        , testGroup
          "exitWithError"
          [ testCase "unhandled exitWithError"
          $ applySchedulerFactory schedulerFactory
          $ do
              void $ exitWithError px "test error"
              error "This should not happen"
          , testCase "cannot catch exitWithError"
          $ applySchedulerFactory schedulerFactory
          $ do
              void $ ignoreProcessError px $ exitWithError px "test error 4"
              error "This should not happen"
          , testCase "multi process exitWithError"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me <- self px
              let n = 15
              traverse_
                (\(i :: Int) -> spawn $ if i `rem` 5 == 0
                  then do
                    void $ sendMessage px me (toDyn i)
                    void (exitWithError px (show i ++ " died"))
                    assertEff "this should not be reached" False
                  else
                    foreverCheap
                      (void (sendMessage px 888 (toDyn "test message to 888"))
                      )
                )
                [0, 5 .. n]
              oks <- replicateM
                (length [0, 5 .. n])
                (do
                  j <- receiveMessageAs px
                  return j
                )
              assertEff "" (sort oks == [0, 5 .. n])
          ]
        ]

concurrencyTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> TestTree
concurrencyTests schedulerFactory
  = let
      px :: SchedulerProxy r
      px = SchedulerProxy
      n  = 100
    in
      testGroup
        "concurrency tests"
        [ testCase
          "when main process exits the scheduler kills/cleans and returns"
        $ applySchedulerFactory schedulerFactory
        $ do
            me <- self px
            traverse_
              (const
                (spawn
                  (do
                    m <- receiveMessage px
                    void (sendMessage px me m)
                  )
                )
              )
              [1 .. n]
            lift (threadDelay 1000)
        , testCase "new processes are executed before the parent process"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do -- start massive amount of children that exit as soon as they are
               -- executed, this will only work smoothly when scheduler schedules
               -- the new child before the parent
            traverse_ (const (spawn (exitNormally px))) [1 .. n]
            assertEff "" True
        , testCase "two concurrent processes"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me     <- self px
            child1 <- spawn
              (do
                m <- receiveMessage px
                void (sendMessage px me m)
              )
            child2 <- spawn
              (foreverCheap (void (sendMessage px 888 (toDyn ""))))
            True <- sendMessageChecked px child1 (toDyn "test")
            i    <- receiveMessageAs px
            True <- sendShutdownChecked px child2
            assertEff "" (i == "test")
        , testCase "most processes send foreverCheap"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me <- self px
            traverse_
              (\(i :: Int) -> spawn $ do
                when (i `rem` 5 == 0) $ void $ sendMessage px me (toDyn i)
                foreverCheap
                  $ void (sendMessage px 888 (toDyn "test message to 888"))
              )
              [0 .. n]
            oks <- replicateM
              (length [0, 5 .. n])
              (do
                j <- receiveMessageAs px
                return j
              )
            assertEff "" (sort oks == [0, 5 .. n])
        , testCase "most processes self foreverCheap"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me <- self px
            traverse_
              (\(i :: Int) -> spawn $ do
                when (i `rem` 5 == 0) $ void $ sendMessage px me (toDyn i)
                foreverCheap $ void (self px)
              )
              [0 .. n]
            oks <- replicateM
              (length [0, 5 .. n])
              (do
                j <- receiveMessageAs px
                return j
              )
            assertEff "" (sort oks == [0, 5 .. n])
        , testCase "most processes sendShutdown foreverCheap"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me <- self px
            traverse_
              (\(i :: Int) -> spawn $ do
                when (i `rem` 5 == 0) $ void $ sendMessage px me (toDyn i)
                foreverCheap $ void (sendShutdown px 999)
              )
              [0 .. n]
            oks <- replicateM
              (length [0, 5 .. n])
              (do
                j <- receiveMessageAs px
                return j
              )
            assertEff "" (sort oks == [0, 5 .. n])
        , testCase "most processes spawn foreverCheap"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me <- self px
            traverse_
              (\(i :: Int) -> spawn $ do
                when (i `rem` 5 == 0) $ void $ sendMessage px me (toDyn i)
                parent <- self px
                foreverCheap
                  $ void
                      (spawn
                        (void
                          (sendMessage px parent (toDyn "test msg from child")
                          )
                        )
                      )
              )
              [0 .. n]
            oks <- replicateM
              (length [0, 5 .. n])
              (do
                j <- receiveMessageAs px
                return j
              )
            assertEff "" (sort oks == [0, 5 .. n])
        , testCase "most processes receive foreverCheap"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me <- self px
            traverse_
              (\(i :: Int) -> spawn $ do
                when (i `rem` 5 == 0) $ void $ sendMessage px me (toDyn i)
                foreverCheap $ void (receiveMessage px)
              )
              [0 .. n]
            oks <- replicateM
              (length [0, 5 .. n])
              (do
                j <- receiveMessageAs px
                return j
              )
            assertEff "" (sort oks == [0, 5 .. n])
        ]

exitTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> TestTree
exitTests schedulerFactory =
  let
    px :: SchedulerProxy r
    px = SchedulerProxy
  in
    testGroup "process exit tests"
      $ [ testGroup
          "async exceptions"
          [ testCase
                (  "a process dies immediately if a "
                ++ show e
                ++ " is thrown, while "
                ++ busyWith
                )
              $ do
                  tidVar           <- newEmptyTMVarIO
                  schedulerDoneVar <- newEmptyTMVarIO
                  void $ forkIO $ do
                    void
                      $ try @SomeException
                      $ void
                      $ applySchedulerFactory schedulerFactory
                      $ do
                          tid <- lift $ myThreadId
                          lift $ atomically $ putTMVar tidVar tid
                          foreverCheap busyEffect
                    atomically $ putTMVar schedulerDoneVar ()
                  tid <- atomically $ takeTMVar tidVar
                  threadDelay 1000
                  throwTo tid e
                  void $ atomically $ takeTMVar schedulerDoneVar
          | e <- [ThreadKilled, UserInterrupt, HeapOverflow, StackOverflow]
          , (busyWith, busyEffect) <-
            [ ("receiving", void (send (ReceiveMessage @r)))
            , ( "sending"
              , void (send (SendMessage @r 44444 (toDyn "test message")))
              )
            , ("sending shutdown", void (send (SendShutdown @r 44444)))
            , ("selfpid-ing"     , void (send (SelfPid @r)))
            , ( "spawn-ing"
              , void (send (Spawn @r (void (send (ReceiveMessage @r)))))
              )
            , ("sleeping", lift (threadDelay 100000))
            ]
          ]
        , testGroup
          "main thread exit not blocked by"
          [ testCase ("a child process, busy with " ++ busyWith)
            $ applySchedulerFactory schedulerFactory
            $ do
                void $ spawn $ foreverCheap busyEffect
                lift (threadDelay 10000)
          | (busyWith, busyEffect) <-
            [ ("receiving", void (send (ReceiveMessage @r)))
            , ( "sending"
              , void (send (SendMessage @r 44444 (toDyn "test message")))
              )
            , ("sending shutdown", void (send (SendShutdown @r 44444)))
            , ("selfpid-ing"     , void (send (SelfPid @r)))
            , ( "spawn-ing"
              , void (send (Spawn @r (void (send (ReceiveMessage @r)))))
              )
            ]
          ]
        , testGroup
          "one process exits, the other continues unimpaired"
          [ testCase
              (  "process 2 exits with: "
              ++ howToExit
              ++ " - while process 1 is busy with: "
              ++ busyWith
              )
            $ scheduleAndAssert schedulerFactory
            $ \assertEff -> do
                p1 <- spawn $ foreverCheap busyEffect
                lift (threadDelay 1000)
                void $ spawn $ do
                  lift (threadDelay 1000)
                  doExit
                lift (threadDelay 100000)
                wasStillRunningP1 <- sendShutdownChecked px p1
                assertEff "the other process was still running"
                          wasStillRunningP1
          | (busyWith , busyEffect) <-
            [ ("receiving", void (send (ReceiveMessage @r)))
            , ( "sending"
              , void (send (SendMessage @r 44444 (toDyn "test message")))
              )
            , ("sending shutdown", void (send (SendShutdown @r 44444)))
            , ("selfpid-ing"     , void (send (SelfPid @r)))
            , ( "spawn-ing"
              , void (send (Spawn @r (void (send (ReceiveMessage @r)))))
              )
            ]
          , (howToExit, doExit    ) <-
            [ ("normally"        , void (exitNormally px))
            , ("simply returning", return ())
            , ("raiseError", void (raiseError px "test error raised"))
            , ("exitWithError", void (exitWithError px "test error exit"))
            , ( "sendShutdown to self"
              , do
                me <- self px
                void (sendShutdown px me)
              )
            ]
          ]
        ]


sendShutdownTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> TestTree
sendShutdownTests schedulerFactory =
  let px :: SchedulerProxy r
      px = SchedulerProxy
  in  testGroup
        "sendShutdown"
        [ testCase "... self" $ applySchedulerFactory schedulerFactory $ do
          me <- self px
          void $ sendShutdown px me
          raiseError px "sendShutdown must not return"
        , testCase "... self low-level"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me <- self px
            r  <- send (SendShutdown @r me)
            assertEff
              "ShutdownRequested must be returned"
              (case r of
                ShutdownRequested -> True
                _                 -> False
              )
        , testGroup
          "... other process"
          [ testCase "while it is sending"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilShutdown (SendMessage @r 666 (toDyn "test"))
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
          , testCase "while it is receiving"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilShutdown (ReceiveMessage @r)
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
          , testCase "while it is self'ing"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilShutdown (SelfPid @r)
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
          , testCase "while it is spawning"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilShutdown (Spawn @r (return ()))
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
          , testCase "while it is sending shutdown messages"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilShutdown (SendShutdown @r 777)
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
          ]
        ]
