module ProcessBehaviourTestCases where

import           Data.List                      ( sort )
import           Data.Foldable                  ( traverse_ )
import           Data.Dynamic                   ( toDyn )
import           Data.Typeable
import           Data.Default
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
import           Data.Void
import           Debug.Trace

testInterruptReason :: InterruptReason
testInterruptReason = ProcessError "test interrupt"

test_forkIo :: TestTree
test_forkIo = setTravisTestOptions $ withTestLogC
  (\c lc -> handleLoggingAndIO_ (ForkIO.schedule c) lc)
  (\factory -> testGroup "ForkIOScheduler" [allTests factory])


test_singleThreaded :: TestTree
test_singleThreaded = setTravisTestOptions $ withTestLogC
  (\e logC ->
        -- void (runLift (logToChannel logC (SingleThreaded.schedule (return ()) e)))
    let runEff
          :: Eff '[Logs LogMessage, LogWriterReader LogMessage IO, Lift IO] a
          -> IO a
        runEff = flip handleLoggingAndIO logC
    in  void $ SingleThreaded.scheduleM runEff yield e
  )
  (\factory -> testGroup "SingleThreadedScheduler" [allTests factory])

allTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
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
  => SchedulerProxy q
  -> Server ReturnToSender
  -> String
  -> Eff r Bool
returnToSender px toP msg = do
  me      <- self px
  _       <- call px toP (ReturnToSender me msg)
  msgEcho <- receiveMessage @String px
  return (msgEcho == msg)

stopReturnToSender
  :: forall q r
   . (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => SchedulerProxy q
  -> Server ReturnToSender
  -> Eff r ()
stopReturnToSender px toP = call px toP StopReturnToSender

returnToSenderServer
  :: forall q r
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Member (Logs LogMessage) q
     , Member Interrupts r
     )
  => SchedulerProxy q
  -> Eff r (Server ReturnToSender)
returnToSenderServer px = asServer <$> spawn
  (serve px $ def
    { _callCallback = Just
                        (\m k -> case m of
                          StopReturnToSender -> do
                            k ()
                            return (StopApiServer testInterruptReason)
                          ReturnToSender fromP echoMsg -> do
                            sendMessage px fromP (toDyn echoMsg)
                            yieldProcess px
                            k True
                            return HandleNextRequest
                        )
    }
  )

selectiveReceiveTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
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
            go 0 = sendMessage SP donePid True
            go n = do
              void $ receiveSelectedMessage SP (filterMessage (== n))
              go (n - 1)

          senderLoop receviverPid =
            traverse_ (sendMessage SP receviverPid) [1 .. nMax]

        me          <- self SP
        receiverPid <- spawn (receiverLoop me)
        spawn_ (senderLoop receiverPid)
        ok <- receiveMessage @Bool SP
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
  => IO (Eff (InterruptableProcess r) () -> IO ())
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
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
pingPongTests schedulerFactory = testGroup
  "Yield Tests"
  [ testCase "ping pong a message between two processes, both don't yield"
  $ applySchedulerFactory schedulerFactory
  $ do
      let pongProc = foreverCheap $ do
            Ping pinger <- receiveMessage SP
            sendMessage SP pinger Pong
          pingProc ponger parent = do
            me <- self SP
            sendMessage SP ponger (Ping me)
            Pong <- receiveMessage SP
            sendMessage SP parent True
      pongPid <- spawn pongProc
      me      <- self SP
      spawn_ (pingProc pongPid me)
      ok <- receiveMessage @Bool SP
      lift (ok @? "ping pong failed")
  , testCase "ping pong a message between two processes, with massive yielding"
  $ applySchedulerFactory schedulerFactory
  $ do
      yieldProcess SP
      let pongProc = foreverCheap $ do
            yieldProcess SP
            Ping pinger <- receiveMessage SP
            yieldProcess SP
            sendMessage SP pinger Pong
            yieldProcess SP
          pingProc ponger parent = do
            yieldProcess SP
            me <- self SP
            yieldProcess SP
            sendMessage SP ponger (Ping me)
            yieldProcess SP
            Pong <- receiveMessage SP
            yieldProcess SP
            sendMessage SP parent True
            yieldProcess SP
      yieldProcess SP
      pongPid <- spawn pongProc
      yieldProcess SP
      me <- self SP
      yieldProcess SP
      spawn_ (pingProc pongPid me)
      yieldProcess SP
      ok <- receiveMessage @Bool SP
      yieldProcess SP
      lift (ok @? "ping pong failed")
      yieldProcess SP
  , testCase
    "the first message is not delayed, not even in cooperative scheduling (because of yield)"
  $ applySchedulerFactory schedulerFactory
  $ do
      pongVar <- lift newEmptyMVar
      let pongProc = foreverCheap $ do
            Pong <- receiveMessage SP
            lift (putMVar pongVar Pong)
      ponger <- spawn pongProc
      sendMessage SP ponger Pong
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
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
errorTests schedulerFactory
  = let
      px :: SchedulerProxy r
      px = SchedulerProxy
    in
      testGroup
        "causing and handling errors"
        [ testGroup
            "exitWithError"
            [ testCase "unhandled exitWithError"
            $ applySchedulerFactory schedulerFactory
            $ do
                void $ exitWithError px "test error"
                error "This should not happen"
            , testCase "cannot catch exitWithError"
            $ applySchedulerFactory schedulerFactory
            $ do
                void $ exitWithError px "test error 4"
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
                      error "this should not be reached"
                    else
                      foreverCheap
                        (void
                          (sendMessage px 888 (toDyn "test message to 888"))
                        )
                  )
                  [0, 5 .. n]
                oks <- replicateM (length [0, 5 .. n]) (receiveMessage px)
                assertEff "" (sort oks == [0, 5 .. n])
            ]
        ]

concurrencyTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
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
                    m <- receiveAnyMessage px
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
                m <- receiveAnyMessage px
                void (sendMessage px me m)
              )
            child2 <- spawn
              (foreverCheap (void (sendMessage px 888 (toDyn ""))))
            sendMessage px child1 (toDyn "test")
            traceM "now receiveMessage"
            i <- receiveMessage px
            traceM ("receiveMessage returned " ++ i)
            sendInterrupt px child2 testInterruptReason
            traceM "sendInterrupt"
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
            oks <- replicateM (length [0, 5 .. n]) (receiveMessage px)
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
            oks <- replicateM (length [0, 5 .. n]) (receiveMessage px)
            assertEff "" (sort oks == [0, 5 .. n])
        , testCase "most processes sendShutdown foreverCheap"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me <- self px
            traverse_
              (\(i :: Int) -> spawn $ do
                when (i `rem` 5 == 0) $ void $ sendMessage px me (toDyn i)
                foreverCheap $ void (sendShutdown px 999 ExitNormally)
              )
              [0 .. n]
            oks <- replicateM (length [0, 5 .. n]) (receiveMessage px)
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
            oks <- replicateM (length [0, 5 .. n]) (receiveMessage px)
            assertEff "" (sort oks == [0, 5 .. n])
        , testCase "most processes receive foreverCheap"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me <- self px
            traverse_
              (\(i :: Int) -> spawn $ do
                when (i `rem` 5 == 0) $ void $ sendMessage px me (toDyn i)
                foreverCheap $ void (receiveAnyMessage px)
              )
              [0 .. n]
            oks <- replicateM (length [0, 5 .. n]) (receiveMessage px)
            assertEff "" (sort oks == [0, 5 .. n])
        ]

exitTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
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
            [ ( "receiving"
              , void
                (send
                  (ReceiveSelectedMessage @r (filterMessage (== "test message"))
                  )
                )
              )
            , ( "sending"
              , void (send (SendMessage @r 44444 (toDyn "test message")))
              )
            , ( "sending shutdown"
              , void (send (SendShutdown @r 44444 ExitNormally))
              )
            , ("selfpid-ing", void (send (SelfPid @r)))
            , ( "spawn-ing"
              , void
                (send
                  (Spawn @r
                    (void
                      (send (ReceiveSelectedMessage @r selectAnyMessageLazy))
                    )
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
                  (ReceiveSelectedMessage @r (filterMessage (== "test message"))
                  )
                )
              )
            , ( "sending"
              , void (send (SendMessage @r 44444 (toDyn "test message")))
              )
            , ( "sending shutdown"
              , void (send (SendShutdown @r 44444 ExitNormally))
              )
            , ("selfpid-ing", void (send (SelfPid @r)))
            , ( "spawn-ing"
              , void
                (send
                  (Spawn @r
                    (void
                      (send (ReceiveSelectedMessage @r selectAnyMessageLazy))
                    )
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
                wasRunningP1 <- isProcessAlive SP p1
                traceM
                  ("XXXXXXXXXXXXXXXXXXXXXX wasRunningP1: " ++ show wasRunningP1)
                sendShutdown px p1 ExitNormally
                lift (threadDelay 100000)
                stillRunningP1 <- isProcessAlive SP p1
                traceM
                  (  "XXXXXXXXXXXXXXXXXXXXXX wasStillRunningP1: "
                  ++ show stillRunningP1
                  )
                assertEff "the other process did not die still running"
                          (not stillRunningP1 && wasRunningP1)
          | (busyWith , busyEffect) <-
            [ ( "receiving"
              , void
                (send
                  (ReceiveSelectedMessage @r (filterMessage (== "test message"))
                  )
                )
              )
            , ( "sending"
              , void (send (SendMessage @r 44444 (toDyn "test message")))
              )
            , ( "sending shutdown"
              , void (send (SendShutdown @r 44444 ExitNormally))
              )
            , ("selfpid-ing", void (send (SelfPid @r)))
            , ( "spawn-ing"
              , void
                (send
                  (Spawn @r
                    (void
                      (send (ReceiveSelectedMessage @r selectAnyMessageLazy))
                    )
                  )
                )
              )
            ]
          , (howToExit, doExit    ) <-
            [ ("normally"        , void (exitNormally px))
            , ("simply returning", return ())
            , ( "raiseError"
              , void (interrupt (ProcessError "test error raised"))
              )
            , ("exitWithError", void (exitWithError px "test error exit"))
            , ( "sendShutdown to self"
              , do
                me <- self px
                void (sendShutdown px me ExitNormally)
              )
            ]
          ]
        ]


sendShutdownTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
sendShutdownTests schedulerFactory
  = let
      px :: SchedulerProxy r
      px = SchedulerProxy
    in
      testGroup
        "sendShutdown"
        [ testCase "... self" $ applySchedulerFactory schedulerFactory $ do
          me <- self px
          void $ send (SendShutdown @r me ExitNormally)
          interrupt (ProcessError "sendShutdown must not return")
        , testCase "sendInterrupt to self"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff -> do
            me <- self px
            r  <- send (SendInterrupt @r me (ProcessError "123"))
            traceShowM
              (show me ++ ": returned from SendShutdow to self " ++ show r)
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
              me    <- self px
              other <- spawn
                (do
                  untilInterrupted (SendMessage @r 666 (toDyn "test"))
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendInterrupt px other testInterruptReason)
              a <- receiveMessage px
              assertEff "" (a == "OK")
          , testCase "while it is receiving"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilInterrupted
                    (ReceiveSelectedMessage @r selectAnyMessageLazy)
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendInterrupt px other testInterruptReason)
              a <- receiveMessage px
              assertEff "" (a == "OK")
          , testCase "while it is self'ing"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilInterrupted (SelfPid @r)
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendInterrupt px other (ProcessError "testError"))
              a <- receiveMessage px
              assertEff "" (a == "OK")
          , testCase "while it is spawning"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilInterrupted (Spawn @r (return ()))
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendInterrupt px other testInterruptReason)
              a <- receiveMessage px
              assertEff "" (a == "OK")
          , testCase "while it is sending shutdown messages"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilInterrupted (SendShutdown @r 777 ExitNormally)
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendInterrupt px other testInterruptReason)
              a <- receiveMessage px
              assertEff "" (a == "OK")
          , testCase "handleInterrupt handles my own interrupts"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff ->
              handleInterrupts
                  (\e ->
                    assertEff "" (ProcessError "test" == e) >> return True
                  )
                  (interrupt (ProcessError "test") >> return False)
                >>= assertEff "exception handler not invoked"
          ]
        ]

linkingTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (InterruptableProcess r) () -> IO ())
  -> TestTree
linkingTests schedulerFactory = setTravisTestOptions
  (testGroup
    "process linking tests"
    [ testCase "link process with it self"
    $ applySchedulerFactory schedulerFactory
    $ do
        me <- self SP
        handleInterrupts
          (\er -> lift (False @? ("unexpected interrupt: " ++ show er)))
          (do linkProcess SP me
              lift (threadDelay 10000))
    , testCase "link with not running process"
    $ applySchedulerFactory schedulerFactory
    $ do
        let testPid = 234234234
        handleInterrupts
          (lift . (@=? LinkedProcessCrashed testPid))
          (do
            linkProcess SP testPid
            void (receiveMessage @Void SP)
          )
    , testCase "linked process exit message is NormalExit"
    $ applySchedulerFactory schedulerFactory
    $ do
        foo <- spawn (void (receiveMessage @Void SP))
        handleInterrupts
          (lift . (\e -> e /= LinkedProcessCrashed foo @? show e))
          (do
            linkProcess SP foo
            sendShutdown SP foo ExitNormally
            lift (threadDelay 1000)
          )
    , testCase "linked process exit message is Crash"
    $ applySchedulerFactory schedulerFactory
    $ do
        foo <- spawn (void (receiveMessage @Void SP))
        handleInterrupts
          (lift . (@=? LinkedProcessCrashed foo))
          (do
            linkProcess SP foo
            sendShutdown SP foo Killed
            void (receiveMessage @Void SP)
          )
    , testCase "spawnLink" $ applySchedulerFactory schedulerFactory $ do
      let foo = void (receiveMessage @Void SP)
      handleInterrupts (\er -> lift (isBecauseDown Nothing er @? show er)) $ do
        x <- spawnLink foo
        sendShutdown SP x Killed
        void (receiveMessage @Void SP)
    , testCase "ignore normal exit"
    $ applySchedulerFactory schedulerFactory
    $ do
        mainProc <- self SP
        let linkingServer = void $ exitOnInterrupt SP $ do
              logNotice "linker"
              foreverCheap $ do
                x <- receiveMessage SP
                linkProcess SP x
                sendMessage SP mainProc True
        linker <- spawnLink linkingServer
        logNotice "mainProc"
        do
          x <- spawnLink (logNotice "x 1" >> void (receiveMessage @Void SP))
          handleInterrupts (lift . (LinkedProcessCrashed x @=?)) $ do
            sendMessage SP linker x
            void $ receiveSelectedMessage SP (filterMessage id)
            sendShutdown SP x Killed
            void (receiveMessage @Void SP)
        do
          x <- spawn (logNotice "x 2" >> void (receiveMessage @Void SP))
          handleInterrupts (lift . (LinkedProcessCrashed x @=?)) $ do
            sendMessage SP linker x
            void $ receiveSelectedMessage SP (filterMessage id)
            sendShutdown SP x ExitNormally
            void (receiveMessage @Void SP)
        do
          handleInterrupts (lift . (LinkedProcessCrashed linker @=?)) $ do
            sendShutdown SP linker Killed
            void (receiveMessage @Void SP)
    , testCase "unlink" $ applySchedulerFactory schedulerFactory $ do
      let
        foo1 = void (receiveAnyMessage SP)
        foo2 foo1Pid = do
          logCritical "link foo1 <-> foo2"
          linkProcess SP foo1Pid
          ("unlink foo1", barPid) <- receiveMessage SP
          logCritical "unlink foo1 <-> foo2"
          unlinkProcess SP foo1Pid
          sendMessage SP barPid ("unlinked foo1", foo1Pid)
          "the end" <- receiveMessage SP
          logCritical "foo2 done"
          exitWithError SP "foo two"
        bar foo2Pid parentPid = do
          logCritical "link bar <-> foo2"
          linkProcess SP foo2Pid
          me <- self SP
          sendMessage SP foo2Pid ("unlink foo1", me)
          ("unlinked foo1", foo1Pid) <- receiveMessage SP
          handleInterrupts
            (\er -> logCritical ("foo1 down: " ++ show er))
            (do
              linkProcess SP foo1Pid
              logCritical "kill foo1"
              sendShutdown SP foo1Pid Killed
              void (receiveMessage @Void SP)
            )
          handleInterrupts
            (\er ->
              logCritical ("foo2 down " ++ show er)
                >> void
                     (sendMessage SP
                                  parentPid
                                  (LinkedProcessCrashed foo2Pid == er)
                     )
            )
            (do
              sendMessage SP foo2Pid "the end"
              void (receiveAnyMessage SP)
            )
      foo1Pid <- spawn foo1
      foo2Pid <- spawn (foo2 foo1Pid)
      me      <- self SP
      barPid  <- spawn (bar foo2Pid me)
      handleInterrupts
        (\er -> do
          logCritical ("Got ER: " ++ show er)
          lift (LinkedProcessCrashed barPid @?= er)
        )
        (do
          res <- receiveMessage @Bool SP
          lift (threadDelay 100000)
          lift (res @?= True)
        )
    ]
  )
