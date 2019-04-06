module ProcessBehaviourTestCases where

import           Data.List                      ( sort )
import           Data.Foldable                  ( traverse_ )
import qualified Data.Dynamic                  as Dynamic
import           Data.Typeable
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Process.Timer
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
import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import           Control.Applicative
import           Data.Void
import Control.Lens (view)

testInterruptReason :: InterruptReason
testInterruptReason = ProcessError "test interrupt"

test_forkIo :: TestTree
test_forkIo = setTravisTestOptions $ withTestLogC
  (\c ->
      runLift
    $ withSomeLogging @IO
    $ withAsyncLogging (100 :: Int) (ioLogWriter (\m -> when (view lmSeverity m < errorSeverity) (printLogMessage m)))
    $ ForkIO.schedule c)
  (\factory -> testGroup "ForkIOScheduler" [allTests factory])


test_singleThreaded :: TestTree
test_singleThreaded = setTravisTestOptions $ withTestLogC
  (\e ->
    let runEff
          :: Eff LoggingAndIo a
          -> IO a
        runEff =
            runLift
          . withLogging
              (ioLogWriter (\m -> when (view lmSeverity m < errorSeverity) (printLogMessage m)))
    in  void $ SingleThreaded.scheduleM runEff yield e
  )
  (\factory -> testGroup "SingleThreadedScheduler" [allTests factory])

allTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
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
    , linkingTests schedulerFactory
    , monitoringTests schedulerFactory
    , timerTests schedulerFactory
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
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => Server ReturnToSender
  -> String
  -> Eff r Bool
returnToSender toP msg = do
  me      <- self
  _       <- call toP (ReturnToSender me msg)
  msgEcho <- receiveMessage @String
  return (msgEcho == msg)

stopReturnToSender
  :: forall q r
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => Server ReturnToSender
  -> Eff r ()
stopReturnToSender toP = call toP StopReturnToSender

returnToSenderServer
  :: forall q
   . (HasCallStack, Member Logs q)
  => Eff (InterruptableProcess q) (Server ReturnToSender)
returnToSenderServer = spawnApiServer
  (handleCalls
    (\m k -> k $ case m of
      StopReturnToSender -> do
        return (Nothing, StopServer testInterruptReason)
      ReturnToSender fromP echoMsg -> do
        sendMessage fromP echoMsg
        yieldProcess
        return (Just True, AwaitNext)
    )
  )
  stopServerOnInterrupt

selectiveReceiveTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
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
            go :: Int -> Eff (InterruptableProcess r) ()
            go 0 = sendMessage donePid True
            go n = do
              void $ receiveSelectedMessage (filterMessage (== n))
              go (n - 1)

          senderLoop destination =
            traverse_ (sendMessage destination) [1 .. nMax]

        me          <- self
        receiverPid <- spawn (receiverLoop me)
        spawn_ (senderLoop receiverPid)
        ok <- receiveMessage @Bool
        lift (ok @? "selective receive failed")
    , testCase "receive a message while waiting for a call reply"
    $ applySchedulerFactory schedulerFactory
    $ do
        srv <- returnToSenderServer
        ok  <- returnToSender srv "test"
        ()  <- stopReturnToSender srv
        lift (ok @? "selective receive failed")
    , testCase "flush messages" $ applySchedulerFactory schedulerFactory $ do
      me <- self
      spawn_ $ replicateM_ 10 (sendMessage me True) >> sendMessage me ()
      spawn_
        $  replicateM_ 10 (sendMessage me (123.23411 :: Float))
        >> sendMessage me ()
      spawn_ $ replicateM_ 10 (sendMessage me "123") >> sendMessage me ()
      ()   <- receiveMessage
      ()   <- receiveMessage
      ()   <- receiveMessage
      -- replicateCheapM_ 40 yieldProcess
      messages <- flushMessages
      lift (length messages @?= 30)
    ]
  )


yieldLoopTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
yieldLoopTests schedulerFactory =
  let maxN = 100000
  in  setTravisTestOptions
        (testGroup
          "yield tests"
          [ testCase
            "yield many times (replicateM_)"
            (applySchedulerFactory schedulerFactory
                                   (replicateM_ maxN yieldProcess)
            )
          , testCase
            "yield many times (forM_)"
            (applySchedulerFactory
              schedulerFactory
              (forM_ [1 :: Int .. maxN] (\_ -> yieldProcess))
            )
          , testCase
            "construct an effect with an exit first, followed by many yields"
            (applySchedulerFactory
              schedulerFactory
              (do
                void exitNormally
                replicateM_ 1000000000000 yieldProcess
              )
            )
          ]
        )


data Ping = Ping ProcessId
  deriving (Eq, Show)

data Pong = Pong
  deriving (Eq, Show)

pingPongTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
pingPongTests schedulerFactory = testGroup
  "Yield Tests"
  [ testCase "ping pong a message between two processes, both don't yield"
  $ applySchedulerFactory schedulerFactory
  $ do
      let pongProc = foreverCheap $ do
            Ping pinger <- receiveMessage
            sendMessage pinger Pong
          pingProc ponger parent = do
            me <- self
            sendMessage ponger (Ping me)
            Pong <- receiveMessage
            sendMessage parent True
      pongPid <- spawn pongProc
      me      <- self
      spawn_ (pingProc pongPid me)
      ok <- receiveMessage @Bool
      lift (ok @? "ping pong failed")
  , testCase "ping pong a message between two processes, with massive yielding"
  $ applySchedulerFactory schedulerFactory
  $ do
      yieldProcess
      let pongProc = foreverCheap $ do
            yieldProcess
            Ping pinger <- receiveMessage
            yieldProcess
            sendMessage pinger Pong
            yieldProcess
          pingProc ponger parent = do
            yieldProcess
            me <- self
            yieldProcess
            sendMessage ponger (Ping me)
            yieldProcess
            Pong <- receiveMessage
            yieldProcess
            sendMessage parent True
            yieldProcess
      yieldProcess
      pongPid <- spawn pongProc
      yieldProcess
      me <- self
      yieldProcess
      spawn_ (pingProc pongPid me)
      yieldProcess
      ok <- receiveMessage @Bool
      yieldProcess
      lift (ok @? "ping pong failed")
      yieldProcess
  , testCase
    "the first message is not delayed, not even in cooperative scheduling (because of yield)"
  $ applySchedulerFactory schedulerFactory
  $ do
      pongVar <- lift newEmptyMVar
      let pongProc = foreverCheap $ do
            Pong <- receiveMessage
            lift (putMVar pongVar Pong)
      ponger <- spawn pongProc
      sendMessage ponger Pong
      let waitLoop = do
            p <- lift (tryTakeMVar pongVar)
            case p of
              Nothing -> do
                yieldProcess
                waitLoop
              Just r -> return r
      p <- waitLoop
      lift (p == Pong @? "ping pong failed")
  ]

errorTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
errorTests schedulerFactory = testGroup
  "causing and handling errors"
  [ testGroup
      "exitWithError"
      [ testCase "unhandled exitWithError"
      $ applySchedulerFactory schedulerFactory
      $ do
          void $ exitWithError "test error"
          error "This should not happen"
      , testCase "cannot catch exitWithError"
      $ applySchedulerFactory schedulerFactory
      $ do
          void $ exitWithError "test error 4"
          error "This should not happen"
      , testCase "multi process exitWithError"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me <- self
          let n = 15
          traverse_
            (\(i :: Int) -> spawn $ foreverCheap
              (void
                (  sendMessage (888888 + fromIntegral i) "test message"
                >> yieldProcess
                )
              )
            )
            [0 .. n]
          traverse_
            (\(i :: Int) -> spawn $ do
              void $ sendMessage me i
              void (exitWithError (show i ++ " died"))
              error "this should not be reached"
            )
            [0 .. n]
          oks <- replicateM (length [0 .. n]) receiveMessage
          assertEff "" (sort oks == [0 .. n])
      ]
  ]

concurrencyTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
concurrencyTests schedulerFactory =
  let n = 100
  in
    testGroup
      "concurrency tests"
      [ testCase
        "when main process exits the scheduler kills/cleans and returns"
      $ applySchedulerFactory schedulerFactory
      $ do
          me <- self
          traverse_
            (const
              (spawn
                (do
                  m <- receiveAnyMessage
                  void (sendMessage me m)
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
          traverse_ (const (spawn exitNormally)) [1 .. n]
          assertEff "" True
      , testCase "two concurrent processes"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me     <- self
          child1 <- spawn
            (do
              m <- receiveAnyMessage
              void (sendAnyMessage me m)
            )
          child2 <- spawn (foreverCheap (void (sendMessage 888888 "")))
          sendMessage child1 "test"
          i <- receiveMessage
          sendInterrupt child2 testInterruptReason
          assertEff "" (i == "test")
      , testCase "most processes send foreverCheap"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me <- self
          traverse_
            (\(i :: Int) -> spawn $ do
              when (i `rem` 5 == 0) $ void $ sendMessage me i
              foreverCheap $ void (sendMessage 888 "test message to 888")
            )
            [0 .. n]
          oks <- replicateM (length [0, 5 .. n]) receiveMessage
          assertEff "" (sort oks == [0, 5 .. n])
      , testCase "most processes self foreverCheap"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me <- self
          traverse_
            (\(i :: Int) -> spawn $ do
              when (i `rem` 5 == 0) $ void $ sendMessage me i
              foreverCheap $ void self
            )
            [0 .. n]
          oks <- replicateM (length [0, 5 .. n]) receiveMessage
          assertEff "" (sort oks == [0, 5 .. n])
      , testCase "most processes sendShutdown foreverCheap"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me <- self
          traverse_
            (\(i :: Int) -> spawn $ do
              when (i `rem` 5 == 0) $ void $ sendMessage me i
              foreverCheap $ void (sendShutdown 999 ExitNormally)
            )
            [0 .. n]
          oks <- replicateM (length [0, 5 .. n]) receiveMessage
          assertEff "" (sort oks == [0, 5 .. n])
      , testCase "most processes spawn foreverCheap"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me <- self
          traverse_
            (\(i :: Int) -> spawn $ do
              when (i `rem` 5 == 0) $ void $ sendMessage me i
              parent <- self
              foreverCheap $ void
                (spawn (void (sendMessage parent "test msg from child")))
            )
            [0 .. n]
          oks <- replicateM (length [0, 5 .. n]) receiveMessage
          assertEff "" (sort oks == [0, 5 .. n])
      , testCase "most processes receive foreverCheap"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me <- self
          traverse_
            (\(i :: Int) -> spawn $ do
              when (i `rem` 5 == 0) $ void $ sendMessage me i
              foreverCheap $ void receiveAnyMessage
            )
            [0 .. n]
          oks <- replicateM (length [0, 5 .. n]) receiveMessage
          assertEff "" (sort oks == [0, 5 .. n])
      ]

exitTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
exitTests schedulerFactory =
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
          [ ( "receiving"
            , void
              (send
                (ReceiveSelectedMessage @r (filterMessage (== "test message")))
              )
            )
          , ( "sending"
            , void (send (SendMessage @r 44444 (Dynamic.toDyn "test message")))
            )
          , ( "sending shutdown"
            , void (send (SendShutdown @r 44444 ExitNormally))
            )
          , ("selfpid-ing", void (send (SelfPid @r)))
          , ( "spawn-ing"
            , void
              (send
                (Spawn @r
                  (void (send (ReceiveSelectedMessage @r selectAnyMessageLazy)))
                )
              )
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
          [ ( "receiving"
            , void
              (send
                (ReceiveSelectedMessage @r (filterMessage (== "test message")))
              )
            )
          , ( "sending"
            , void (send (SendMessage @r 44444 (Dynamic.toDyn "test message")))
            )
          , ( "sending shutdown"
            , void (send (SendShutdown @r 44444 ExitNormally))
            )
          , ("selfpid-ing", void (send (SelfPid @r)))
          , ( "spawn-ing"
            , void
              (send
                (Spawn @r
                  (void (send (ReceiveSelectedMessage @r selectAnyMessageLazy)))
                )
              )
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
              wasRunningP1 <- isProcessAlive p1
              sendShutdown p1 ExitNormally
              lift (threadDelay 100000)
              stillRunningP1 <- isProcessAlive p1
              assertEff "the other process did not die still running"
                        (not stillRunningP1 && wasRunningP1)
        | (busyWith , busyEffect) <-
          [ ( "receiving"
            , void
              (send
                (ReceiveSelectedMessage @r (filterMessage (== "test message")))
              )
            )
          , ( "sending"
            , void (send (SendMessage @r 44444 (Dynamic.toDyn "test message")))
            )
          , ( "sending shutdown"
            , void (send (SendShutdown @r 44444 ExitNormally))
            )
          , ("selfpid-ing", void (send (SelfPid @r)))
          , ( "spawn-ing"
            , void
              (send
                (Spawn @r
                  (void (send (ReceiveSelectedMessage @r selectAnyMessageLazy)))
                )
              )
            )
          ]
        , (howToExit, doExit    ) <-
          [ ("normally"        , void exitNormally)
          , ("simply returning", return ())
          , ("raiseError", void (interrupt (ProcessError "test error raised")))
          , ("exitWithError"   , void (exitWithError "test error exit"))
          , ( "sendShutdown to self"
            , do
              me <- self
              void (sendShutdown me ExitNormally)
            )
          ]
        ]
      ]


sendShutdownTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
sendShutdownTests schedulerFactory = testGroup
  "sendShutdown"
  [ testCase "... self" $ applySchedulerFactory schedulerFactory $ do
    me <- self
    void $ send (SendShutdown @r me ExitNormally)
    interrupt (ProcessError "sendShutdown must not return")
  , testCase "sendInterrupt to self"
  $ scheduleAndAssert schedulerFactory
  $ \assertEff -> do
      me <- self
      r  <- send (SendInterrupt @r me (ProcessError "123"))
      assertEff
        "Interrupted must be returned"
        (case r of
          Interrupted (ProcessError "123") -> True
          _ -> False
        )
  , testGroup
    "... other process"
    [ testCase "while it is sending"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          (do
            untilInterrupted (SendMessage @r 666666 (Dynamic.toDyn "test"))
            void (sendMessage me "OK")
          )
        void (sendInterrupt other testInterruptReason)
        a <- receiveMessage
        assertEff "" (a == "OK")
    , testCase "while it is receiving"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          (do
            untilInterrupted (ReceiveSelectedMessage @r selectAnyMessageLazy)
            void (sendMessage me "OK")
          )
        void (sendInterrupt other testInterruptReason)
        a <- receiveMessage
        assertEff "" (a == "OK")
    , testCase "while it is self'ing"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          (do
            untilInterrupted (SelfPid @r)
            void (sendMessage me "OK")
          )
        void (sendInterrupt other (ProcessError "testError"))
        a <- receiveMessage
        assertEff "" (a == "OK")
    , testCase "while it is spawning"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          (do
            untilInterrupted (Spawn @r (return ()))
            void (sendMessage me "OK")
          )
        void (sendInterrupt other testInterruptReason)
        a <- receiveMessage
        assertEff "" (a == "OK")
    , testCase "while it is sending shutdown messages"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          (do
            untilInterrupted (SendShutdown @r 777 ExitNormally)
            void (sendMessage me "OK")
          )
        void (sendInterrupt other testInterruptReason)
        a <- receiveMessage
        assertEff "" (a == "OK")
    , testCase "handleInterrupt handles my own interrupts"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff ->
        handleInterrupts (\e -> return (ProcessError "test" == e))
                         (interrupt (ProcessError "test") >> return False)
          >>= assertEff "exception handler not invoked"
    ]
  ]

linkingTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
linkingTests schedulerFactory = setTravisTestOptions
  (testGroup
    "process linking tests"
    [ testCase "link process with it self"
    $ applySchedulerFactory schedulerFactory
    $ do
        me <- self
        handleInterrupts
          (\er -> lift (False @? ("unexpected interrupt: " ++ show er)))
          (do
            linkProcess me
            lift (threadDelay 10000)
          )
    , testCase "link with not running process"
    $ applySchedulerFactory schedulerFactory
    $ do
        let testPid = 234234234
        handleInterrupts
          (lift . (@?= LinkedProcessCrashed testPid))
          (do
            linkProcess testPid
            void (receiveMessage @Void)
          )
    , testCase "linked process exit message is NormalExit"
    $ applySchedulerFactory schedulerFactory
    $ do
        foo <- spawn (void (receiveMessage @Void))
        handleInterrupts
          (lift . (\e -> e /= LinkedProcessCrashed foo @? show e))
          (do
            linkProcess foo
            sendShutdown foo ExitNormally
            lift (threadDelay 1000)
          )
    , testCase "linked process exit message is Crash"
    $ applySchedulerFactory schedulerFactory
    $ do
        foo <- spawn (void (receiveMessage @Void))
        handleInterrupts
          (lift . (@?= LinkedProcessCrashed foo))
          (do
            linkProcess foo
            sendShutdown foo Killed
            void (receiveMessage @Void)
          )
    , testCase "link multiple times"
    $ applySchedulerFactory schedulerFactory
    $ do
        foo <- spawn (void (receiveMessage @Void))
        handleInterrupts
          (lift . (@?= LinkedProcessCrashed foo))
          (do
            linkProcess foo
            linkProcess foo
            linkProcess foo
            linkProcess foo
            linkProcess foo
            linkProcess foo
            linkProcess foo
            sendShutdown foo Killed
            void (receiveMessage @Void)
          )
    , testCase "unlink multiple times"
    $ applySchedulerFactory schedulerFactory
    $ do
        foo <- spawn (void (receiveMessage @Void))
        handleInterrupts
          (lift . (False @?) . show)
          (do
            spawn_ (void receiveAnyMessage)
            linkProcess foo
            linkProcess foo
            linkProcess foo
            unlinkProcess foo
            unlinkProcess foo
            unlinkProcess foo
            unlinkProcess foo
            withMonitor foo $ \ref -> do
              sendShutdown foo Killed
              void (receiveSelectedMessage (selectProcessDown ref))
          )
    , testCase "spawnLink" $ applySchedulerFactory schedulerFactory $ do
      let foo = void (receiveMessage @Void)
      handleInterrupts (\er -> lift (isBecauseDown Nothing er @? show er)) $ do
        x <- spawnLink foo
        sendShutdown x Killed
        void (receiveMessage @Void)
    , testCase "ignore normal exit"
    $ applySchedulerFactory schedulerFactory
    $ do
        mainProc <- self
        let linkingServer = void $ exitOnInterrupt $ do
              logNotice "linker"
              foreverCheap $ do
                x <- receiveMessage
                linkProcess x
                sendMessage mainProc True
        linker <- spawnLink linkingServer
        logNotice "mainProc"
        do
          x <- spawnLink (logNotice "x 1" >> void (receiveMessage @Void))
          withMonitor x $ \xRef -> do
            sendMessage linker x
            void $ receiveSelectedMessage (filterMessage id)
            sendShutdown x ExitNormally
            void (receiveSelectedMessage (selectProcessDown xRef))
        do
          x <- spawnLink (logNotice "x 2" >> void (receiveMessage @Void))
          withMonitor x $ \xRef -> do
            sendMessage linker x
            void $ receiveSelectedMessage (filterMessage id)
            sendShutdown x ExitNormally
            void (receiveSelectedMessage (selectProcessDown xRef))
        handleInterrupts (lift . (LinkedProcessCrashed linker @=?)) $ do
          sendShutdown linker Killed
          void (receiveMessage @Void)
    , testCase "unlink" $ applySchedulerFactory schedulerFactory $ do
      let
        foo1 = void receiveAnyMessage
        foo2 foo1Pid = do
          linkProcess foo1Pid
          (r1, barPid) <- receiveMessage
          lift ("unlink foo1" @=? r1)
          unlinkProcess foo1Pid
          sendMessage barPid ("unlinked foo1", foo1Pid)
          receiveMessage >>= lift . (@?= "the end")
          exitWithError "foo two"
        bar foo2Pid parentPid = do
          linkProcess foo2Pid
          me <- self
          sendMessage foo2Pid ("unlink foo1", me)
          (r1, foo1Pid) <- receiveMessage
          lift ("unlinked foo1" @=? r1)
          handleInterrupts
            (const (return ()))
            (do
              linkProcess foo1Pid
              sendShutdown foo1Pid Killed
              void (receiveMessage @Void)
            )
          handleInterrupts
            (\er -> void
              (sendMessage parentPid (LinkedProcessCrashed foo2Pid == er))
            )
            (do
              sendMessage foo2Pid "the end"
              void receiveAnyMessage
            )
      foo1Pid <- spawn foo1
      foo2Pid <- spawn (foo2 foo1Pid)
      me      <- self
      barPid  <- spawn (bar foo2Pid me)
      handleInterrupts
        (\er -> lift (LinkedProcessCrashed barPid @?= er))
        (do
          res <- receiveMessage @Bool
          lift (threadDelay 100000)
          lift (res @?= True)
        )
    ]
  )

monitoringTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
monitoringTests schedulerFactory = setTravisTestOptions
  (testGroup
    "process monitoring tests"
    [ testCase "monitored process not running"
    $ applySchedulerFactory schedulerFactory
    $ do
        let badPid = 132123
        ref <- monitor badPid
        pd  <- receiveSelectedMessage (selectProcessDown ref)
        lift (downReason pd @?= SomeExitReason (ProcessNotRunning badPid))
        lift (threadDelay 10000)
    , testCase "monitored process exit normally"
    $ applySchedulerFactory schedulerFactory
    $ do
        target <- spawn (receiveMessage >>= exitBecause)
        ref    <- monitor target
        sendMessage target ExitNormally
        pd <- receiveSelectedMessage (selectProcessDown ref)
        lift (downReason pd @?= SomeExitReason ExitNormally)
        lift (threadDelay 10000)
    , testCase "multiple monitors some demonitored"
    $ applySchedulerFactory schedulerFactory
    $ do
        target <- spawn (receiveMessage >>= exitBecause)
        ref1   <- monitor target
        ref2   <- monitor target
        ref3   <- monitor target
        ref4   <- monitor target
        ref5   <- monitor target
        demonitor ref3
        demonitor ref5
        sendMessage target ExitNormally
        pd1 <- receiveSelectedMessage (selectProcessDown ref1)
        lift (downReason pd1 @?= SomeExitReason ExitNormally)
        pd2 <- receiveSelectedMessage (selectProcessDown ref2)
        lift (downReason pd2 @?= SomeExitReason ExitNormally)
        pd4 <- receiveSelectedMessage (selectProcessDown ref4)
        lift (downReason pd4 @?= SomeExitReason ExitNormally)
        lift (threadDelay 10000)
    , testCase "monitored process killed"
    $ applySchedulerFactory schedulerFactory
    $ do
        target <- spawn (receiveMessage >>= exitBecause)
        ref    <- monitor target
        sendMessage target Killed
        pd <- receiveSelectedMessage (selectProcessDown ref)
        lift (downReason pd @?= SomeExitReason Killed)
        lift (threadDelay 10000)
    , testCase "demonitored process killed"
    $ applySchedulerFactory schedulerFactory
    $ do
        target <- spawn (receiveMessage >>= exitBecause)
        ref    <- monitor target
        demonitor ref
        sendMessage target Killed
        me <- self
        spawn_ (lift (threadDelay 10000) >> sendMessage me ())
        pd <- receiveSelectedMessage

          (Right <$> selectProcessDown ref <|> Left <$> selectMessage @())
        lift (pd @?= Left ())
        lift (threadDelay 10000)
    ]
  )

timerTests
  :: forall r
   . (Lifted IO r, LogsTo IO r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
timerTests schedulerFactory = setTravisTestOptions
  (testGroup
    "process timer tests"
    [ testCase "receiveAfter into timeout"
    $ applySchedulerFactory schedulerFactory
    $ do
        pd <- receiveAfter @Void 1000
        lift (pd @?= Nothing)
        lift (threadDelay 10000)
    , testCase "receiveAfter no timeout"
    $ applySchedulerFactory schedulerFactory
    $ do
        me    <- self
        other <- spawn
          (do
            r <- receiveMessage @()
            lift (r @?= ())
            sendMessage me (123 :: Int)
          )
        pd1 <- receiveAfter @() 10000
        lift (pd1 @?= Nothing)
        sendMessage other ()
        pd2 <- receiveAfter @Int 10000
        lift (pd2 @?= Just 123)
        lift (threadDelay 10000)
    , testCase "many receiveAfters"
    $ applySchedulerFactory schedulerFactory
    $ do
        let n = 5
            testMsg :: Float
            testMsg = 123
        me    <- self
        other <- spawn
          (do
            replicateM_ n $ sendMessage me "bad message"
            r <- receiveMessage @()
            lift (r @?= ())
            replicateM_ n $ sendMessage me testMsg
          )
        receiveAfter @Float 100 >>= lift . (@?= Nothing)
        sendMessage other ()
        replicateM_
          n
          (do
            res <- receiveAfter @Float 10000
            lift (res @?= Just testMsg)
          )

        lift (threadDelay 100)
    ]
  )
