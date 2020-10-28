{-# LANGUAGE NoOverloadedStrings #-}

module BrokerTests
  ( test_Broker,
  )
where

import Common
import Control.Eff.Concurrent.Protocol.Broker as Broker
import Control.Eff.Concurrent.Protocol.EffectfulServer (Event (..))
import qualified Control.Eff.Concurrent.Protocol.Observer.Queue as OQ
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import Control.Lens
import Data.String

test_Broker :: HasCallStack => TestTree
test_Broker =
  setTravisTestOptions $
    testGroup
      "Broker"
      ( let startTestBroker = startTestBrokerWith ExitWhenRequested (TimeoutMicros 500000)
            startTestBrokerWith e t = Broker.startLink (MkBrokerConfig t (Stateful.Init . TestServerArgs e))
            spawnTestChild broker i = Broker.spawnChild broker i >>= either (lift . assertFailure . show) pure
         in [ runTestCase "The broker starts and is shut down" $ do
                outerSelf <- self
                testWorker <-
                  spawn (fromString "test-worker") $ do
                    broker <- startTestBroker
                    sendMessage outerSelf broker
                    () <- receiveMessage
                    sendMessage outerSelf ()
                    () <- receiveMessage
                    Broker.stopBroker broker
                unlinkProcess testWorker
                broker <- receiveMessage :: Eff Effects (Endpoint (Broker.Broker (Stateful.Stateful TestProtocol)))
                brokerAliveAfter1 <- isBrokerAlive broker
                logInfo (LABEL "still alive 1" brokerAliveAfter1)
                lift (brokerAliveAfter1 @=? True)
                sendMessage testWorker ()
                () <- receiveMessage
                brokerAliveAfter2 <- isBrokerAlive broker
                logInfo (LABEL "still alive 2" brokerAliveAfter2)
                lift (brokerAliveAfter2 @=? True)
                sendMessage testWorker ()
                testWorkerMonitorRef <- monitor testWorker
                d1 <- receiveSelectedMessage (selectProcessDown testWorkerMonitorRef)
                logInfo (LABEL "got test worker down" d1)
                testBrokerMonitorRef <- monitorBroker broker
                d2 <- receiveSelectedMessage (selectProcessDown testBrokerMonitorRef)
                logInfo (LABEL "got broker down" d2)
                brokerAliveAfterOwnerExited <- isBrokerAlive broker
                logInfo (LABEL "still alive after owner exited" brokerAliveAfterOwnerExited)
                lift (brokerAliveAfterOwnerExited @=? False),
              testGroup
                "Diagnostics"
                [ runTestCase "When only time passes the diagnostics do not change" $ do
                    broker <- startTestBroker
                    info1 <- Broker.getDiagnosticInfo broker
                    lift (threadDelay 10000)
                    info2 <- Broker.getDiagnosticInfo broker
                    lift (assertEqual "diagnostics should not differ: " info1 info2),
                  runTestCase "When a child is started the diagnostics change" $ do
                    broker <- startTestBroker
                    info1 <- Broker.getDiagnosticInfo broker
                    logInfo (LABEL "got diagnostics" info1)
                    let childId = 1
                    _child <- fromRight (error "failed to spawn child") <$> Broker.spawnChild broker childId
                    info2 <- Broker.getDiagnosticInfo broker
                    logInfo (LABEL "got diagnostics" info2)
                    lift $ assertBool ("diagnostics should differ: " ++ show (info1, info2)) (info1 /= info2)
                ],
              let childId = 1
               in testGroup
                    "Startup and shutdown"
                    [ runTestCase "When a broker is shut down, all children are shutdown" $ do
                        broker <- startTestBroker
                        child <- spawnTestChild broker childId
                        let childPid = _fromEndpoint child
                        brokerMon <- monitorBroker broker
                        childMon <- monitor childPid
                        isProcessAlive childPid >>= lift . assertBool "child process not running"
                        isBrokerAlive broker >>= lift . assertBool "broker process not running"
                        call child (TestGetStringLength "123") >>= lift . assertEqual "child not working" 3
                        stopBroker broker
                        d1@(ProcessDown mon1 er1 _) <-
                          fromMaybe (error "receive timeout 1") <$> receiveAfter (TimeoutMicros 1000000)
                        logInfo (LABEL "got process down" d1)
                        d2@(ProcessDown mon2 er2 _) <-
                          fromMaybe (error "receive timeout 2") <$> receiveAfter (TimeoutMicros 1000000)
                        logInfo (LABEL "got process down" d2)
                        case if mon1 == brokerMon && mon2 == childMon
                          then Right (er1, er2)
                          else
                            if mon1 == childMon && mon2 == brokerMon
                              then Right (er2, er1)
                              else
                                Left
                                  ( packLogMsg "unexpected monitor down: first: "
                                      <> toLogMsg (mon1, er1)
                                      <> packLogMsg ", and then: "
                                      <> toLogMsg (mon2, er2)
                                      <> packLogMsg ", brokerMon: "
                                      <> toLogMsg brokerMon
                                      <> packLogMsg ", childMon: "
                                      <> toLogMsg childMon
                                  ) of
                          Right (brokerER, childER) -> do
                            lift (assertEqual "bad broker exit reason" ExitNormally brokerER)
                            lift (assertEqual "bad child exit reason" ExitNormally childER)
                          Left x -> lift (assertFailure (show x)),
                      runTestCase
                        "When a broker is shut down, children that won't shutdown, are killed after some time"
                        $ do
                          broker <- startTestBrokerWith IgnoreNormalExitRequest (TimeoutMicros 10000)
                          child <- spawnTestChild broker childId
                          let childPid = _fromEndpoint child
                          brokerMon <- monitorBroker broker
                          childMon <- monitor childPid
                          isProcessAlive childPid >>= lift . assertBool "child process not running"
                          isBrokerAlive broker >>= lift . assertBool "broker process not running"
                          call child (TestGetStringLength "123") >>= lift . assertEqual "child not working" 3
                          stopBroker broker
                          d1@(ProcessDown mon1 er1 _) <-
                            fromMaybe (error "receive timeout 1") <$> receiveAfter (TimeoutMicros 1000000)
                          logInfo (LABEL "got process down" d1)
                          d2@(ProcessDown mon2 er2 _) <-
                            fromMaybe (error "receive timeout 2") <$> receiveAfter (TimeoutMicros 1000000)
                          logInfo (LABEL "got process down" d2)
                          case if mon1 == brokerMon && mon2 == childMon
                            then Right er1
                            else
                              if mon1 == childMon && mon2 == brokerMon
                                then Right er2
                                else
                                  Left
                                    ( "unexpected monitor down: first: " <> show (mon1, er1) <> ", and then: "
                                        <> show (mon2, er2)
                                        <> ", brokerMon: "
                                        <> show brokerMon
                                        <> ", childMon: "
                                        <> show childMon
                                    ) of
                            Right brokerER -> do
                              lift (assertEqual "bad broker exit reason" ExitNormally brokerER)
                            Left x -> lift (assertFailure x)
                    ],
              let i = 123
               in testGroup
                    "Spawning and Using Children"
                    [ runTestCase
                        "When a broker is requested to start two children with the same id, an already started error is returned"
                        $ do
                          broker <- startTestBroker
                          c <- spawnTestChild broker i
                          x <- Broker.spawnChild broker i
                          case x of
                            Left (AlreadyStarted i' c') ->
                              lift $ do
                                assertEqual "bad pid returned" c c'
                                assertEqual "bad child id returned" 123 i'
                            _ -> lift (assertFailure "AlreadyStarted expected!"),
                      runTestCase "When a child is started it can be lookup up" $ do
                        broker <- startTestBroker
                        c <- spawnTestChild broker i
                        c' <- Broker.lookupChild broker i >>= maybe (lift (assertFailure "child not found")) pure
                        lift (assertEqual "lookupChild returned wrong child" c c'),
                      runTestCase "When several children are started they can be lookup up and don't crash" $ do
                        broker <- startTestBroker
                        c <- spawnTestChild broker i
                        someOtherChild <- spawnTestChild broker (i + 1)
                        c' <- Broker.lookupChild broker i >>= maybe (lift (assertFailure "child not found")) pure
                        lift (assertEqual "lookupChild returned wrong child" c c')
                        childStillRunning <- isProcessAlive (_fromEndpoint c)
                        lift (assertBool "child not running" childStillRunning)
                        someOtherChildStillRunning <- isProcessAlive (_fromEndpoint someOtherChild)
                        lift (assertBool "someOtherChild not running" someOtherChildStillRunning),
                      let startTestBrokerAndChild = do
                            broker <- startTestBroker
                            c <- spawnTestChild broker i
                            cm <- monitor (_fromEndpoint c)
                            return (broker, cm)
                       in testGroup
                            "Stopping children"
                            [ runTestCase "When a child is started it can be stopped" $ do
                                (broker, cm) <- startTestBrokerAndChild
                                Broker.stopChild broker i >>= lift . assertBool "child not found"
                                (ProcessDown _ r _) <- receiveSelectedMessage (selectProcessDown cm)
                                lift (assertEqual "bad exit reason" ExitNormally r),
                              runTestCase
                                "When a child is stopped but doesn't exit voluntarily, it is kill after some time"
                                $ do
                                  broker <- startTestBrokerWith IgnoreNormalExitRequest (TimeoutMicros 5000)
                                  c <- spawnTestChild broker i
                                  cm <- monitor (_fromEndpoint c)
                                  Broker.stopChild broker i >>= lift . assertBool "child not found"
                                  (ProcessDown _ r _) <- receiveSelectedMessage (selectProcessDown cm)
                                  case r of
                                    ExitUnhandledError _ -> return ()
                                    _ -> lift (assertFailure ("bad exit reason: " ++ show r)),
                              runTestCase "When a stopChild is called with an unallocated ID, False is returned" $ do
                                (broker, _) <- startTestBrokerAndChild
                                Broker.stopChild broker (i + 1) >>= lift . assertBool "child not found" . not,
                              runTestCase "When a child is stopped, lookup won't find it" $ do
                                (broker, _) <- startTestBrokerAndChild
                                Broker.stopChild broker i >>= lift . assertBool "child not found"
                                x <- Broker.lookupChild broker i
                                lift (assertEqual "lookup should not find a child" Nothing x),
                              runTestCase "When a child is stopped, the child id can be reused" $ do
                                (broker, _) <- startTestBrokerAndChild
                                Broker.stopChild broker i >>= lift . assertBool "child not found"
                                Broker.spawnChild broker i >>= lift . assertBool "id could not be reused" . isRight
                            ],
                      let startTestBrokerAndChild = do
                            broker <- startTestBroker
                            c <- spawnTestChild broker i
                            cm <- monitor (_fromEndpoint c)
                            return (broker, c, cm)
                       in testGroup
                            "Child exit handling"
                            [ runTestCase "When a child exits normally, lookupChild will not find it" $ do
                                (broker, c, cm) <- startTestBrokerAndChild
                                cast c (TestInterruptWith NormalExitRequested)
                                (ProcessDown _ r _) <- receiveSelectedMessage (selectProcessDown cm)
                                lift (assertEqual "bad exit reason" ExitNormally r)
                                x <- Broker.lookupChild broker i
                                lift (assertEqual "lookup should not find a child" Nothing x),
                              runTestCase "When a child exits with an error, lookupChild will not find it" $ do
                                (broker, c, cm) <- startTestBrokerAndChild
                                cast c (TestInterruptWith (ErrorInterrupt (fromString "test error reason")))
                                (ProcessDown _ _ _) <- receiveSelectedMessage (selectProcessDown cm)
                                x <- Broker.lookupChild broker i
                                lift (assertEqual "lookup should not find a child" Nothing x),
                              runTestCase "When a child is interrupted from another process and dies, lookupChild will not find it" $ do
                                (broker, c, cm) <- startTestBrokerAndChild
                                sendInterrupt (_fromEndpoint c) NormalExitRequested
                                (ProcessDown _ _ _) <- receiveSelectedMessage (selectProcessDown cm)
                                x <- Broker.lookupChild broker i
                                lift (assertEqual "lookup should not find a child" Nothing x),
                              runTestCase "When a child is shutdown from another process and dies, lookupChild will not find it" $ do
                                (broker, c, cm) <- startTestBrokerAndChild
                                self >>= sendShutdown (_fromEndpoint c) . ExitProcessCancelled . Just
                                (ProcessDown _ _ _) <- receiveSelectedMessage (selectProcessDown cm)
                                x <- Broker.lookupChild broker i
                                lift (assertEqual "lookup should not find a child" Nothing x)
                            ],
                      testGroup
                        "broker events"
                        [ runTestCase "when a child starts the observer is notified" $ do
                            broker <- startTestBroker
                            OQ.observe @(Broker.ChildEvent (Stateful.Stateful TestProtocol)) (100 :: Int) broker $ do
                              c <- Broker.spawnChild @(Stateful.Stateful TestProtocol) broker i >>= either (lift . assertFailure . show) pure
                              e <- OQ.await @(Broker.ChildEvent (Stateful.Stateful TestProtocol))
                              case e of
                                Broker.OnChildSpawned broker' i' c' -> do
                                  lift (assertEqual "wrong endpoint" broker broker')
                                  lift (assertEqual "wrong child" c c')
                                  lift (assertEqual "wrong child-id" i i')
                                _ ->
                                  lift (assertFailure ("unexpected event: " ++ show (toLogMsg e))),
                          runTestCase "when a child stops the observer is notified" $ do
                            broker <- startTestBroker
                            c <- Broker.spawnChild @(Stateful.Stateful TestProtocol) broker i >>= either (lift . assertFailure . show) pure
                            OQ.observe @(Broker.ChildEvent (Stateful.Stateful TestProtocol)) (100 :: Int) broker $ do
                              Broker.stopChild broker i >>= lift . assertBool "child not found"
                              OQ.await @(Broker.ChildEvent (Stateful.Stateful TestProtocol))
                                >>= \case
                                  Broker.OnChildDown broker' i' c' e -> do
                                    lift (assertEqual "wrong endpoint" broker broker')
                                    lift (assertEqual "wrong child" c c')
                                    lift (assertEqual "wrong child-id" i i')
                                    lift (assertEqual "wrong exit reason" ExitNormally e)
                                  e ->
                                    lift (assertFailure ("unexpected event: " ++ show (toLogMsg e))),
                          runTestCase "when a child crashes the observer is notified" $ do
                            broker <- startTestBroker
                            OQ.observe @(Broker.ChildEvent (Stateful.Stateful TestProtocol)) (100 :: Int) broker $ do
                              c <- Broker.spawnChild @(Stateful.Stateful TestProtocol) broker i >>= either (lift . assertFailure . show) pure
                              let expectedError = ExitUnhandledError (fromString "test error")
                              sendShutdown (_fromEndpoint c) expectedError
                              OQ.await @(Broker.ChildEvent (Stateful.Stateful TestProtocol))
                                >>= \case
                                  Broker.OnChildSpawned broker' i' c' -> do
                                    lift (assertEqual "wrong endpoint" broker broker')
                                    lift (assertEqual "wrong child" c c')
                                    lift (assertEqual "wrong child-id" i i')
                                  e ->
                                    lift (assertFailure ("unexpected event: " ++ show (toLogMsg e)))
                              OQ.await @(Broker.ChildEvent (Stateful.Stateful TestProtocol))
                                >>= \case
                                  Broker.OnChildDown broker' i' c' e -> do
                                    lift (assertEqual "wrong endpoint" broker broker')
                                    lift (assertEqual "wrong child" c c')
                                    lift (assertEqual "wrong child-id" i i')
                                    lift (assertEqual "wrong exit reason" expectedError e)
                                  e ->
                                    lift (assertFailure ("unexpected event: " ++ show (toLogMsg e))),
                          runTestCase "when a child does not stop when requested and is killed, the oberser is notified correspondingly" $ do
                            broker <- startTestBrokerWith IgnoreNormalExitRequest (TimeoutMicros 5000)
                            c <- spawnTestChild broker i
                            OQ.observe @(Broker.ChildEvent (Stateful.Stateful TestProtocol)) (100 :: Int) broker $ do
                              cm <- monitor (_fromEndpoint c)
                              Broker.stopChild broker i >>= lift . assertBool "child not found"
                              (ProcessDown _ expectedError _) <- receiveSelectedMessage (selectProcessDown cm)
                              OQ.await @(Broker.ChildEvent (Stateful.Stateful TestProtocol))
                                >>= \case
                                  Broker.OnChildDown broker' i' c' e -> do
                                    lift (assertEqual "wrong endpoint" broker broker')
                                    lift (assertEqual "wrong child" c c')
                                    lift (assertEqual "wrong child-id" i i')
                                    lift (assertEqual "wrong exit reason" expectedError e)
                                  e ->
                                    lift (assertFailure ("unexpected event: " ++ show (toLogMsg e))),
                          runTestCase "when the broker stops, an OnBrokerShuttingDown is emitted before any child is stopped" $ do
                            broker <- startTestBroker
                            unlinkProcess (broker ^. fromEndpoint)
                            void
                              ( Broker.spawnChild @(Stateful.Stateful TestProtocol) broker i
                                  >>= either (lift . assertFailure . show) pure
                              )
                            OQ.observe @(Broker.ChildEvent (Stateful.Stateful TestProtocol)) (1000 :: Int) broker $ do
                              Broker.stopBroker broker
                              OQ.await @(Broker.ChildEvent (Stateful.Stateful TestProtocol))
                                >>= \case
                                  Broker.OnBrokerShuttingDown _ -> logNotice "received OnBrokerShuttingDown"
                                  e ->
                                    lift (assertFailure ("unexpected event: " ++ show (toLogMsg e)))
                              OQ.await @(Broker.ChildEvent (Stateful.Stateful TestProtocol))
                                >>= \case
                                  Broker.OnChildDown _ _ _ e -> lift (assertEqual "wrong exit reason" ExitNormally e)
                                  e ->
                                    lift (assertFailure ("unexpected event: " ++ show (toLogMsg e)))
                        ]
                    ]
            ]
      )

data TestProtocol deriving (Typeable)

instance ToTypeLogMsg TestProtocol where
  toTypeLogMsg _ = fromString "TestProtocol"

instance HasPdu TestProtocol where
  data Pdu TestProtocol x where
    TestGetStringLength :: String -> Pdu TestProtocol ('Synchronous Int)
    TestInterruptWith :: InterruptReason -> Pdu TestProtocol 'Asynchronous
    deriving (Typeable)

instance NFData (Pdu TestProtocol x) where
  rnf (TestGetStringLength x) = rnf x
  rnf (TestInterruptWith x) = rnf x

instance ToLogMsg (Pdu TestProtocol r) where
  toLogMsg (TestGetStringLength s) = packLogMsg "TestGetStringLength " <> toLogMsg s
  toLogMsg (TestInterruptWith s) = packLogMsg "TestInterruptWith " <> toLogMsg s

data TestProtocolServerMode
  = IgnoreNormalExitRequest
  | ExitWhenRequested
  deriving (Eq)

instance Stateful.Server TestProtocol Effects where
  newtype Model TestProtocol = TestProtocolModel () deriving (Default)
  update _me (TestServerArgs testMode tId) evt =
    case evt of
      OnCast (TestInterruptWith i) -> do
        logInfo tId (LABEL "stopping with" i)
        interrupt i
      OnCall rt (TestGetStringLength str) -> do
        logInfo tId (LABEL "calculating length of" str)
        sendReply rt (length str)
      OnInterrupt x -> do
        logNotice tId (LABEL "interrupt" x)
        if testMode == IgnoreNormalExitRequest
          then logNotice tId "ignoring normal exit request"
          else do
            logNotice tId "exitting normally"
            exitBecause (interruptToExit x)
      _ ->
        logDebug tId (LABEL "got some info" evt)
  data StartArgument TestProtocol = TestServerArgs TestProtocolServerMode Int

type instance ChildId TestProtocol = Int

instance ToLogMsg (StartArgument TestProtocol) where
  toLogMsg (TestServerArgs mode x) =
    packLogMsg "TestProtocol-" <> packLogMsg modeMsg <> packLogMsg "-" <> toLogMsg x
    where
      modeMsg =
        case mode of
          IgnoreNormalExitRequest -> "IgnoreNormalExitRequest"
          ExitWhenRequested -> "ExitWhenRequested"
