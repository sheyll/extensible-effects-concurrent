{-# LANGUAGE NumericUnderscores #-}
module ProcessBehaviourTestCases where

import           Common
import           Control.Exception
import qualified Control.Eff.Concurrent.Protocol.CallbackServer
                                               as Callback
import           Control.Eff.Concurrent.Protocol.EffectfulServer
import qualified Control.Eff.Concurrent.Process.ForkIOScheduler
                                               as ForkIO
import qualified Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                               as SingleThreaded
import           Control.Applicative
import           Control.Lens                   ( view )
import           Data.List                      ( sort )
import           Data.Foldable                  ( traverse_ )
import           Data.Maybe
import           Data.Void
import           GHC.Generics                   ( Generic )
import           Data.String                    ( fromString )


testInterruptReason :: Interrupt 'Recoverable
testInterruptReason = ErrorInterrupt "test interrupt"

test_forkIo :: TestTree
test_forkIo = setTravisTestOptions $ withTestLogC
  (\c ->
    runLift
      $ withLogging
          (filteringLogWriter (lmSeverityIsAtLeast errorSeverity)
                              consoleLogWriter
          )
      $ withAsyncLogWriter (100 :: Int)
      $ ForkIO.schedule c
  )
  (\factory -> testGroup "ForkIOScheduler" [allTests factory])


test_singleThreaded :: TestTree
test_singleThreaded = setTravisTestOptions $ withTestLogC
  (\e ->
    let runEff :: Eff LoggingAndIo a -> IO a
        runEff = runLift . withLogging
          (mkLogWriterIO
            (\m -> when (view lmSeverity m < errorSeverity) (printLogMessage m))
          )
    in  void $ SingleThreaded.scheduleM runEff yield e
  )
  (\factory -> testGroup "SingleThreadedScheduler" [allTests factory])

allTests
  :: forall r
   . (LogIo r, Typeable r)
  => IO (Eff (Processes r) () -> IO ())
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
    , processDetailsTests schedulerFactory
    ]
  )

data ReturnToSender
  deriving Typeable

type instance ToPretty ReturnToSender = PutStr "ReturnToSender"

instance HasPdu ReturnToSender where
 data instance Pdu ReturnToSender r where
   ReturnToSender :: ProcessId -> String -> Pdu ReturnToSender ('Synchronous Bool)
   StopReturnToSender :: Pdu ReturnToSender ('Synchronous ())

instance NFData (Pdu ReturnToSender r) where
  rnf (ReturnToSender p s) = rnf p `seq` rnf s
  rnf StopReturnToSender = ()

deriving instance Show (Pdu ReturnToSender x)

deriving instance Typeable (Pdu ReturnToSender x)

returnToSender
  :: forall q r
   . (HasCallStack, HasProcesses r q)
  => Endpoint ReturnToSender
  -> String
  -> Eff r Bool
returnToSender toP msg = do
  me      <- self
  _       <- call toP (ReturnToSender me msg)
  msgEcho <- receiveMessage @String
  return (msgEcho == msg)

stopReturnToSender
  :: forall q r
   . (HasCallStack, HasProcesses r q)
  => Endpoint ReturnToSender
  -> Eff r ()
stopReturnToSender toP = call toP StopReturnToSender

returnToSenderServer
  :: forall q
   . (HasCallStack, LogIo q, Typeable q)
  => Eff (Processes q) (Endpoint ReturnToSender)
returnToSenderServer = Callback.startLink @ReturnToSender $ Callback.onEvent
  (\evt -> case evt of
    OnCall rt msg -> case msg of
      StopReturnToSender           -> interrupt testInterruptReason
      ReturnToSender fromP echoMsg -> do
        sendMessage fromP echoMsg
        yieldProcess
        sendReply rt True
    OnInterrupt i -> interrupt i
    other         -> interrupt (ErrorInterrupt (show other))
  )
  "return-to-sender"

selectiveReceiveTests
  :: forall r
   . (LogIo r, Typeable r)
  => IO (Eff (Processes r) () -> IO ())
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
            go :: Int -> Eff (Processes r) ()
            go 0 = sendMessage donePid True
            go n = do
              void $ receiveSelectedMessage (filterMessage (== n))
              go (n - 1)

          senderLoop destination =
            traverse_ (sendMessage destination) [1 .. nMax]

        me           <- self
        receiverPid2 <- spawn "reciever loop" (receiverLoop me)
        spawn_ "sender loop" (senderLoop receiverPid2)
        ok <- receiveMessage @Bool
        lift (ok @? "selective receive failed")
    , testCase "receive a message while waiting for a call reply"
    $ applySchedulerFactory schedulerFactory
    $ do
        srv <- returnToSenderServer
        ok  <- returnToSender srv "test"
        ()  <- stopReturnToSender srv
        lift (ok @? "selective receive failed")
    , testCase "when sending multiple messages, it is possible to receive them selectively in any order"
    $ applySchedulerFactory schedulerFactory
    $ do
        me <- self
        let messages :: [Int]
            messages = [1 .. 100]
        mRefs <- traverse
                  (\i ->
                    spawn (fromString ("sender-" ++ show i))
                          (yieldProcess >> sendMessage me i >> logInfo ("sent: " <> pack (show i)))
                    >>= monitor)
                  messages
        traverse_
                  (\i ->
                    receiveSelectedMessage (filterMessage (== i))
                    >>= logInfo . ("received: " <> ) . pack . show)
                  messages
        traverse_
                  (\(mref, i) ->
                    logInfo ("waiting for " <> pack (show mref) <> " of " <> pack (show i))
                      >> receiveSelectedMessage (selectProcessDown mref)
                      >>= logInfo . (("down: " <> pack (show i) <> " ") <> ) . pack . show)
                  (mRefs `zip` messages)

    , testCase "flush messages" $ applySchedulerFactory schedulerFactory $ do
      me <- self
      spawn_ "sender-bool"
        $  replicateM_ 10 (sendMessage me True)
        >> sendMessage me ()
      spawn_ "sender-float"
        $  replicateM_ 10 (sendMessage me (123.23411 :: Float))
        >> sendMessage me ()
      spawn_ "sender-string"
        $  replicateM_ 10 (sendMessage me ("123" :: String))
        >> sendMessage me ()
      ()       <- receiveMessage
      ()       <- receiveMessage
      ()       <- receiveMessage
      -- replicateCheapM_ 40 yieldProcess
      messages <- flushMessages
      lift (length messages @?= 30)
    ]
  )


yieldLoopTests
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
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


newtype Ping = Ping ProcessId
  deriving (Eq, Show, Typeable, NFData)

data Pong = Pong
  deriving (Eq, Show, Generic)

instance NFData Pong

pingPongTests
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
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
      pongPid <- spawn "pong" pongProc
      me      <- self
      spawn_ "ping" (pingProc pongPid me)
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
      pongPid <- spawn "pong" pongProc
      yieldProcess
      me <- self
      yieldProcess
      spawn_ "ping" (pingProc pongPid me)
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
      ponger <- spawn "pong" pongProc
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
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
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
            (\(i :: Int) -> spawn "test" $ foreverCheap
              (void
                (  sendMessage (888888 + fromIntegral i)
                               ("test message" :: String)
                >> yieldProcess
                )
              )
            )
            [0 .. n]
          traverse_
            (\(i :: Int) -> spawn "test" $ do
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
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
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
                "reciever"
                (do
                  m <- receiveAnyMessage
                  void (sendAnyMessage me m)
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
          traverse_ (const (spawn "lemming" exitNormally)) [1 .. n]
          assertEff "" True
      , testCase "two concurrent processes"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me     <- self
          child1 <- spawn
            "reciever"
            (do
              m <- receiveAnyMessage
              void (sendAnyMessage me m)
            )
          child2 <- spawn
            "sender"
            (foreverCheap (void (sendMessage 888888 ("" :: String))))
          sendMessage child1 ("test" :: String)
          i <- receiveMessage
          sendInterrupt child2 testInterruptReason
          assertEff "" (i == ("test" :: String))
      , testCase "most processes send foreverCheap"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me <- self
          traverse_
            (\(i :: Int) -> spawn "sender" $ do
              when (i `rem` 5 == 0) $ void $ sendMessage me i
              foreverCheap
                $ void (sendMessage 888 ("test message to 888" :: String))
            )
            [0 .. n]
          oks <- replicateM (length [0, 5 .. n]) receiveMessage
          assertEff "" (sort oks == [0, 5 .. n])
      , testCase "most processes self foreverCheap"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me <- self
          traverse_
            (\(i :: Int) -> spawn "sender" $ do
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
            (\(i :: Int) -> spawn "killer" $ do
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
            (\(i :: Int) -> spawn "sender 0" $ do
              when (i `rem` 5 == 0) $ void $ sendMessage me i
              parent <- self
              foreverCheap $ void
                (spawn
                  "sender"
                  (void (sendMessage parent ("test msg from child" :: String))
                  )
                )
            )
            [0 .. n]
          oks <- replicateM (length [0, 5 .. n]) receiveMessage
          assertEff "" (sort oks == [0, 5 .. n])
      , testCase "most processes receive foreverCheap"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff -> do
          me <- self
          traverse_
            (\(i :: Int) -> spawn "sender" $ do
              when (i `rem` 5 == 0) $ void $ sendMessage me i
              foreverCheap $ void receiveAnyMessage
            )
            [0 .. n]
          oks <- replicateM (length [0, 5 .. n]) receiveMessage
          assertEff "" (sort oks == [0, 5 .. n])
      ]

exitTests
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
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
                (ReceiveSelectedMessage @r
                  (filterMessage (== ("test message" :: String)))
                )
              )
            )
          , ( "sending"
            , void
              (send
                (SendMessage @r 44444
                                (toStrictDynamic ("test message" :: String))
                )
              )
            )
          , ( "sending shutdown"
            , void (send (SendShutdown @r 44444 ExitNormally))
            )
          , ("selfpid-ing", void (send (SelfPid @r)))
          , ( "spawn-ing"
            , void
              (send
                (Spawn @r
                  "reciever"
                  (void (send (ReceiveSelectedMessage @r selectAnyMessage)))
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
              void $ spawn "busyloop" $ foreverCheap busyEffect
              lift (threadDelay 10000)
        | (busyWith, busyEffect) <-
          [ ( "receiving"
            , void
              (send
                (ReceiveSelectedMessage @r
                  (filterMessage (== ("test message" :: String)))
                )
              )
            )
          , ( "sending"
            , void
              (send
                (SendMessage @r 44444
                                (toStrictDynamic ("test message" :: String))
                )
              )
            )
          , ( "sending shutdown"
            , void (send (SendShutdown @r 44444 ExitNormally))
            )
          , ("selfpid-ing", void (send (SelfPid @r)))
          , ( "spawn-ing"
            , void
              (send
                (Spawn @r
                  "reciever"
                  (void (send (ReceiveSelectedMessage @r selectAnyMessage)))
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
              p1 <- spawn "busyloop" $ foreverCheap busyEffect
              lift (threadDelay 1000)
              void $ spawn "sleep loop" $ do
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
                (ReceiveSelectedMessage @r
                  (filterMessage (== ("test message" :: String)))
                )
              )
            )
          , ( "sending"
            , void
              (send
                (SendMessage @r 44444
                                (toStrictDynamic ("test message" :: String))
                )
              )
            )
          , ( "sending shutdown"
            , void (send (SendShutdown @r 44444 ExitNormally))
            )
          , ("selfpid-ing", void (send (SelfPid @r)))
          , ( "spawn-ing"
            , void
              (send
                (Spawn @r
                  "receiver"
                  (void (send (ReceiveSelectedMessage @r selectAnyMessage)))
                )
              )
            )
          ]
        , (howToExit, doExit    ) <-
          [ ("normally"        , void exitNormally)
          , ("simply returning", return ())
          , ( "raiseError"
            , void (interrupt (ErrorInterrupt "test error raised"))
            )
          , ("exitWithError", void (exitWithError "test error exit"))
          , ( "sendShutdown to self"
            , do
              me <- self
              void (sendShutdown me ExitNormally)
            )
          ]
        ]
      ]


sendShutdownTests
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
sendShutdownTests schedulerFactory = testGroup
  "sendShutdown"
  [ testCase "... self" $ applySchedulerFactory schedulerFactory $ do
    me <- self
    void $ send (SendShutdown @r me ExitNormally)
    interrupt (ErrorInterrupt "sendShutdown must not return")
  , testCase "sendInterrupt to self"
  $ scheduleAndAssert schedulerFactory
  $ \assertEff -> do
      me <- self
      r  <- send (SendInterrupt @r me (ErrorInterrupt "123"))
      assertEff
        "Interrupted must be returned"
        (case r of
          Interrupted (ErrorInterrupt "123") -> True
          _ -> False
        )
  , testGroup
    "... other process"
    [ testCase "while it is sending"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          "interrupted"
          (do
            untilInterrupted
              (SendMessage @r 666666 (toStrictDynamic ("test" :: String)))
            void (sendMessage me ("OK" :: String))
          )
        void (sendInterrupt other testInterruptReason)
        a <- receiveMessage
        assertEff "" (a == ("OK" :: String))
    , testCase "while it is receiving"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          "interrupted"
          (do
            untilInterrupted (ReceiveSelectedMessage @r selectAnyMessage)
            void (sendMessage me ("OK" :: String))
          )
        void (sendInterrupt other testInterruptReason)
        a <- receiveMessage
        assertEff "" (a == ("OK" :: String))
    , testCase "while it is self'ing"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          "interrupted"
          (do
            untilInterrupted (SelfPid @r)
            void (sendMessage me ("OK" :: String))
          )
        void (sendInterrupt other (ErrorInterrupt "testError"))
        (a :: String) <- receiveMessage
        assertEff "" (a == "OK")
    , testCase "while it is spawning"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          "interrupted"
          (do
            untilInterrupted (Spawn @r "returner" (return ()))
            void (sendMessage me ("OK" :: String))
          )
        void (sendInterrupt other testInterruptReason)
        a <- receiveMessage
        assertEff "" (a == ("OK" :: String))
    , testCase "while it is sending shutdown messages"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff -> do
        me    <- self
        other <- spawn
          "interrupted"
          (do
            untilInterrupted (SendShutdown @r 777 ExitNormally)
            void (sendMessage me ("OK" :: String))
          )
        void (sendInterrupt other testInterruptReason)
        a <- receiveMessage
        assertEff "" (a == ("OK" :: String))
    , testCase "handleInterrupt handles my own interrupts"
    $ scheduleAndAssert schedulerFactory
    $ \assertEff ->
        handleInterrupts (\e -> return (ErrorInterrupt "test" == e))
                         (interrupt (ErrorInterrupt "test") >> return False)
          >>= assertEff "exception handler not invoked"
    ]
  ]

linkingTests
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
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
        foo <- spawn "reciever" (void (receiveMessage @Void))
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
        foo <- spawn "reciever" (void (receiveMessage @Void))
        handleInterrupts
          (lift . (@?= LinkedProcessCrashed foo))
          (do
            linkProcess foo
            self >>= sendShutdown foo . ExitProcessCancelled . Just
            void (receiveMessage @Void)
          )
    , testCase "link multiple times"
    $ applySchedulerFactory schedulerFactory
    $ do
        foo <- spawn "reciever" (void (receiveMessage @Void))
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
            self >>= sendShutdown foo . ExitProcessCancelled . Just
            void (receiveMessage @Void)
          )
    , testCase "unlink multiple times"
    $ applySchedulerFactory schedulerFactory
    $ do
        foo <- spawn "reciever" (void (receiveMessage @Void))
        handleInterrupts
          (lift . (False @?) . show)
          (do
            spawn_ "reciever" (void receiveAnyMessage)
            linkProcess foo
            linkProcess foo
            linkProcess foo
            unlinkProcess foo
            unlinkProcess foo
            unlinkProcess foo
            unlinkProcess foo
            withMonitor foo $ \ref -> do
              self >>= sendShutdown foo . ExitProcessCancelled . Just
              void (receiveSelectedMessage (selectProcessDown ref))
          )
    , testCase "spawnLink" $ applySchedulerFactory schedulerFactory $ do
      let foo = void (receiveMessage @Void)
      handleInterrupts
          (\er -> lift (isProcessDownInterrupt Nothing er @? show er))
        $ do
            x <- spawnLink "foo" foo
            self >>= sendShutdown x . ExitProcessCancelled . Just
            void (receiveMessage @Void)
    , testCase "spawnLink and child exits by returning from spawn"
    $ applySchedulerFactory schedulerFactory
    $ do
        me <- self
        u  <- spawn "unlinker" $ do
          logCritical "unlinked child started"
          l <- spawnLink "linked" $ do
            logCritical "linked child started"
            () <- receiveMessage
            logCritical "linked child done"
          sendMessage me l
          x <- receiveAnyMessage
          logCritical' ("got: " <> show x)
        l <- receiveMessage
        _ <- monitor l
        sendMessage l ()
        pL@(ProcessDown _ _ _) <- receiveMessage
        logCritical' ("linked process down: " <> show pL)
        _   <- monitor u
        mpU <- receiveAfter (TimeoutMicros 1000)
        case mpU of
          Just (pU@(ProcessDown _ _ _)) ->
            error ("unlinked process down: " <> show pU)
          Nothing -> logInfo "passed"
    , testCase "spawnLink and child exits via exitWithError"
    $ applySchedulerFactory schedulerFactory
    $ do
        me <- self
        u  <- spawn "unlinker" $ do
          logCritical "unlinked child started"
          l <- spawnLink "linker" $ do
            logCritical "linked child started"
            () <- receiveMessage
            logCritical "linked child done"
            exitWithError "linked process test error"
          sendMessage me l
          x <- receiveAnyMessage
          logCritical' ("got: " <> show x)
        l <- receiveMessage
        _ <- monitor l
        sendMessage l ()
        pL@(ProcessDown _ _ _) <- receiveMessage
        logCritical' ("linked process down: " <> show pL)
        _   <- monitor u
        mpU <- receiveAfter (TimeoutMicros 1000)
        case mpU of
          Just (pU@(ProcessDown _ _ _)) ->
            logInfo' ("unlinked process down: " <> show pU)
          Nothing -> error "linked process not exited!"
    , testCase "ignore normal exit"
    $ applySchedulerFactory schedulerFactory
    $ do
        mainProc <- self
        let linkingServer = void $ exitOnInterrupt $ do
              logNotice "linker"
              foreverCheap $ do
                x <- receiveMessage
                case x of
                  Right p -> do
                    linkProcess p
                    sendMessage mainProc True
                  Left e ->
                    exitBecause e
        linker <- spawnLink "link-server" linkingServer
        logNotice "mainProc"
        do
          x <- spawnLink "x1" (logNotice "x 1" >> void (receiveMessage @Void))
          withMonitor x $ \xRef -> do
            sendMessage linker (Right x :: Either (Interrupt 'NoRecovery) ProcessId)
            void $ receiveSelectedMessage (filterMessage id)
            sendShutdown x ExitNormally
            void (receiveSelectedMessage (selectProcessDown xRef))
        do
          x <- spawnLink "x2" (logNotice "x 2" >> void (receiveMessage @Void))
          withMonitor x $ \xRef -> do
            sendMessage linker (Right x :: Either (Interrupt 'NoRecovery) ProcessId)
            void $ receiveSelectedMessage (filterMessage id)
            sendShutdown x ExitNormally
            void (receiveSelectedMessage (selectProcessDown xRef))
        handleInterrupts (lift . (LinkedProcessCrashed linker @=?)) $ do
          me <- self
          sendMessage linker (Left (ExitProcessCancelled (Just me)) :: Either (Interrupt 'NoRecovery) ProcessId)
          void (receiveMessage @Void)
    , testCase "unlink" $ applySchedulerFactory schedulerFactory $ do
      let
        foo1 = void receiveAnyMessage
        foo2 foo1Pid = do
          linkProcess foo1Pid
          (r1, barPid) <- receiveMessage
          lift (("unlink foo1" :: String) @=? r1)
          unlinkProcess foo1Pid
          sendMessage barPid ("unlinked foo1" :: String, foo1Pid)
          receiveMessage >>= lift . (@?= ("the end" :: String))
          exitWithError "foo two"
        bar foo2Pid parentPid = do
          linkProcess foo2Pid
          me <- self
          sendMessage foo2Pid ("unlink foo1" :: String, me)
          (r1, foo1Pid) <- receiveMessage
          lift (("unlinked foo1" :: String) @=? r1)
          handleInterrupts
            (const (return ()))
            (do
              linkProcess foo1Pid
              self >>= sendShutdown foo1Pid . ExitProcessCancelled . Just
              void (receiveMessage @Void)
            )
          handleInterrupts
            (\er -> void
              (sendMessage parentPid (LinkedProcessCrashed foo2Pid == er))
            )
            (do
              sendMessage foo2Pid ("the end" :: String)
              void receiveAnyMessage
            )
      foo1Pid <- spawn "foo1" foo1
      foo2Pid <- spawn "foo2" (foo2 foo1Pid)
      me      <- self
      barPid  <- spawn "bar" (bar foo2Pid me)
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
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
monitoringTests schedulerFactory = setTravisTestOptions
  (testGroup
    "process monitoring tests"
    [ testCase "monitored process not running"
    $ applySchedulerFactory schedulerFactory
    $ do
        let badPid = 132123
        ref <- monitor badPid
        pd  <- receiveSelectedMessage (selectProcessDown ref)
        lift (downReason pd @?= ExitOtherProcessNotRunning badPid)
        lift (threadDelay 10000)
    , testCase
      "monitor twice, once when it is running and one, when the monitored process is not running (variant 1)"
    $ applySchedulerFactory schedulerFactory
    $ do
        target <- spawn "reciever-exit-value" (receiveMessage >>= exitBecause)
        me     <- self
        spawn_ "monitor 1" $ do
          ref1 <- monitor target
          receiveSelectedMessage (selectProcessDown ref1)
            >>= sendMessage me
            .   Just
        lift (threadDelay 10000)
        sendMessage target ExitNormally
        pd1 <- receiveMessage
        lift (downReason <$> pd1 @?= Just ExitNormally)
        ref <- monitor target
        pd2 <- receiveSelectedMessage (selectProcessDown ref)
        lift (downReason pd2 @?= ExitOtherProcessNotRunning target)
        lift (threadDelay 10000)
    , testCase
      "spawn, shutdown and monitor many times in a tight loop"
    $ applySchedulerFactory schedulerFactory
    $ do
        tests <- replicateM 60 $ spawn "spawn, shutdown and monitor test" $ do
          target <- spawn "target" (receiveMessage >>= exitBecause)
          replicateM_ 102 $ spawn_ "monitor" $ do
            ref <- monitor target
            logInfo ("monitoring now" <> pack (show ref))
            void $ receiveSelectedMessage (selectProcessDown ref)
          sendMessage target ExitNormally
          ref <- monitor target
          logInfo ("monitoring now" <> pack (show ref))
          void (receiveSelectedMessage (selectProcessDown ref))
        traverse_ awaitProcessDown tests
    , testCase "monitored process exit normally"
    $ applySchedulerFactory schedulerFactory
    $ do
        target <- spawn "reciever-exit-value" (receiveMessage >>= exitBecause)
        ref    <- monitor target
        sendMessage target ExitNormally
        pd <- receiveSelectedMessage (selectProcessDown ref)
        lift (downReason pd @?= ExitNormally)
        lift (threadDelay 10000)
    , testCase "multiple monitors some demonitored"
    $ applySchedulerFactory schedulerFactory
    $ do
        target <- spawn "reciever-exit-value" (receiveMessage >>= exitBecause)
        ref1   <- monitor target
        ref2   <- monitor target
        ref3   <- monitor target
        ref4   <- monitor target
        ref5   <- monitor target
        demonitor ref3
        demonitor ref5
        sendMessage target ExitNormally
        pd1 <- receiveSelectedMessage (selectProcessDown ref1)
        lift (downReason pd1 @?= ExitNormally)
        pd2 <- receiveSelectedMessage (selectProcessDown ref2)
        lift (downReason pd2 @?= ExitNormally)
        pd4 <- receiveSelectedMessage (selectProcessDown ref4)
        lift (downReason pd4 @?= ExitNormally)
        lift (threadDelay 10000)
    , testCase "monitored process killed"
    $ applySchedulerFactory schedulerFactory
    $ do
        target <- spawn "reciever-exit-value" (receiveMessage >>= exitBecause)
        ref    <- monitor target
        self >>= sendMessage target . ExitProcessCancelled . Just
        pd <- receiveSelectedMessage (selectProcessDown ref)
        me <- self
        lift (downReason pd @?= ExitProcessCancelled (Just me))
        lift (threadDelay 10000)
    , testCase "demonitored process killed"
    $ applySchedulerFactory schedulerFactory
    $ do
        target <- spawn "reciever-exit-value" (receiveMessage >>= exitBecause)
        ref    <- monitor target
        demonitor ref
        self >>= sendMessage target . ExitProcessCancelled . Just
        me <- self
        spawn_ "wait-and-send" (lift (threadDelay 10000) >> sendMessage me ())
        pd <- receiveSelectedMessage

          (Right <$> selectProcessDown ref <|> Left <$> selectMessage @())
        lift (pd @?= Left ())
        lift (threadDelay 10000)
    ]
  )

timerTests
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
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
          "reciever"
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
          "test"
          (do
            replicateM_ n $ sendMessage me ("bad message" :: String)
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

processDetailsTests
  :: forall r . (LogIo r) => IO (Eff (Processes r) () -> IO ()) -> TestTree
processDetailsTests schedulerFactory = setTravisTestOptions
  (testGroup
    "process info tests"
    [ testCase
      "no infos for a non-existant process"
      (applySchedulerFactory
        schedulerFactory
        (do
          let nonExistentPid = ProcessId 123
          me    <- self
          pInfo <- getProcessState nonExistentPid
          lift (assertEqual "" (nonExistentPid /= me) (isNothing pInfo))
        )
      )
    , testCase
      "no infos for a dead process"
      (applySchedulerFactory
        schedulerFactory
        (do
          deadProc <- spawn "dead" (return ())
          mr       <- monitor deadProc
          void $ receiveSelectedMessage (selectProcessDown mr)
          pInfo <- getProcessState deadProc
          lift
            (assertBool "no process state of a dead process expected"
                        (isNothing pInfo)
            )
        )
      )
    , testGroup
      "process title tests"
      [ testCase
          "getProcessState returns the title passed to spawn"
          (applySchedulerFactory
            schedulerFactory
            (do
              let expectedTitle = "expected title"
              p <- spawn expectedTitle (void receiveAnyMessage)
              (actualTitle, _, _) <- fromJust <$> getProcessState p
              lift
                (assertEqual "unexpected process title"
                             expectedTitle
                             actualTitle
                )
            )
          )
      ]
    , testGroup
      "process details tests"
      [ testCase
          "update"
          (applySchedulerFactory
            schedulerFactory
            (do
              let expected1 = "test details 1"
                  expected2 = "test details 2"
              updateProcessDetails expected1
              (_, actual1, _) <- fromJust <$> (self >>= getProcessState)
              lift (assertEqual "1" expected1 actual1)
              updateProcessDetails expected2
              (_, actual2, _) <- fromJust <$> (self >>= getProcessState)
              lift (assertEqual "2" expected2 actual2)
            )
          )
      ]
    ]
  )
