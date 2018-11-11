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
import           Debug.Trace

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
   . (HasCallStack, SetMember Process (Process q) r)
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
   . (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Server ReturnToSender
  -> Eff r ()
stopReturnToSender px toP = call px toP StopReturnToSender

returnToSenderServer
  :: forall q r
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Member (Logs LogMessage) q
     )
  => SchedulerProxy q
  -> Eff r (Server ReturnToSender)
returnToSenderServer px = asServer <$> spawn
  (serve px $ def
    { _callCallback = Just
                        (\m k -> case m of
                          StopReturnToSender -> do
                            k ()
                            return (StopApiServer ExitNormally)
                          ReturnToSender fromP echoMsg -> do
                            ok <- sendMessage px fromP (toDyn echoMsg)
                            yieldProcess px
                            k ok
                            return HandleNextRequest
                        )
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
              void $ receiveSelectedMessage SP (filterMessage (== n))
              go (n - 1)

          senderLoop receviverPid =
            traverse_ (sendMessageAs SP receviverPid) [1 .. nMax]

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
            Ping pinger <- receiveMessage SP
            sendMessageAs SP pinger Pong
          pingProc ponger parent = do
            me <- self SP
            sendMessageAs SP ponger (Ping me)
            Pong <- receiveMessage SP
            sendMessageAs SP parent True
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
            sendMessageAs SP pinger Pong
            yieldProcess SP
          pingProc ponger parent = do
            yieldProcess SP
            me <- self SP
            yieldProcess SP
            sendMessageAs SP ponger (Ping me)
            yieldProcess SP
            Pong <- receiveMessage SP
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
            "exitWithError"
            [ testCase "unhandled exitWithError"
            $ applySchedulerFactory schedulerFactory
            $ do
                void $ exitWithError px "test error"
                error "This should not happen"
            , testCase "cannot catch exitWithError"
            $ applySchedulerFactory schedulerFactory
            $ do
                void $ ignoreInterrupts px $ exitWithError px "test error 4"
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
            True <- sendMessage px child1 (toDyn "test")
            traceM "now receiveMessage"
            i <- receiveMessage px
            traceM ("receiveMessage returned " ++ i)
            True <- sendInterrupt px child2 ExitNormally
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
                foreverCheap
                  $ void (sendShutdown px 999 (NotRecovered ExitNormally))
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
              , void (send (SendShutdown @r 44444 (NotRecovered ExitNormally)))
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
              , void (send (SendShutdown @r 44444 (NotRecovered ExitNormally)))
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
                wasStillRunningP1 <- sendShutdown
                  px
                  p1
                  (NotRecovered ExitNormally)
                assertEff "the other process was still running"
                          wasStillRunningP1
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
              , void (send (SendShutdown @r 44444 (NotRecovered ExitNormally)))
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
            , ("raiseError", void (raiseError px "test error raised"))
            , ("exitWithError", void (exitWithError px "test error exit"))
            , ( "sendShutdown to self"
              , do
                me <- self px
                void (sendShutdown px me (NotRecovered ExitNormally))
              )
            ]
          ]
        ]


sendShutdownTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
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
          void $ send (SendShutdown @r me (NotRecovered ExitNormally))
          raiseError px "sendShutdown must not return"
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
              void (sendInterrupt px other ExitNormally)
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
              void (sendInterrupt px other ExitNormally)
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
              void (sendInterrupt px other ExitNormally)
              a <- receiveMessage px
              assertEff "" (a == "OK")
          , testCase "while it is sending shutdown messages"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff -> do
              me    <- self px
              other <- spawn
                (do
                  untilInterrupted
                    (SendShutdown @r 777 (NotRecovered ExitNormally))
                  void (sendMessage px me (toDyn "OK"))
                )
              void (sendInterrupt px other ExitNormally)
              a <- receiveMessage px
              assertEff "" (a == "OK")
          , testCase "handleInterrupt handles my own interrupts"
          $ scheduleAndAssert schedulerFactory
          $ \assertEff ->
              SP
                  (\e ->
                    assertEff "" (ProcessError "test" == e) >> return True
                  )
                  (interruptProcess SP (ProcessError "test") >> return False)
                >>= assertEff "exception handler not invoked"
          ]
        ]


linkingTests
  :: forall r
   . (Member (Logs LogMessage) r, SetMember Lift (Lift IO) r)
  => IO (Eff (Process r ': r) () -> IO ())
  -> TestTree
linkingTests schedulerFactory = setTravisTestOptions
  (testGroup
    "process linking tests"
    [
       -- link from process bar to process foo, then exit foo normally and handle the
       -- resulting interrupt in bar. Bar will send the exit reason to the
       -- waiting main process, which will then place an assertion on the
       -- correctnes of the exit reason.
      testCase "link processes, shutdown normal no recovery"
    $ applySchedulerFactory schedulerFactory
    $ do
        let
          foo = void (receiveAnyMessage SP)
          bar fooPid parentPid = handleInterrupts
            SP
            (\er -> do
              logCritical
                ("4--------------------------------------------------------------"
                )
              void (sendMessageAs SP parentPid er)
              logCritical
                ("4--------------------------------------------------------------"
                )
            )
            (do
              linkProcess SP fooPid
              logCritical
                ("1--------------------------------------------------------------"
                )
              sendShutdown SP fooPid (NotRecovered ExitNormally)
              logCritical
                ("2--------------------------------------------------------------"
                )
              void (receiveAnyMessage SP)
              logCritical
                ("3--------------------------------------------------------------"
                )
            )

        fooPid <- spawn foo
        me     <- self SP
        spawn_ (bar fooPid me)
        er <- receiveMessage @(ExitReason 'Recoverable) SP
        lift (er @=? LinkedProcessCrashed fooPid NormalExit NoRecovery)
    , testCase "link process with it self"
    $ applySchedulerFactory schedulerFactory
    $ do
        me <- self SP
        handleInterrupts
          SP
          (\er -> lift (False @? ("unexpected interrupt: " ++ show er)))
          (linkProcess SP me)
    ]
  )
