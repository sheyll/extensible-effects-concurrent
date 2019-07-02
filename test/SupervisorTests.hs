module SupervisorTests
  ( test_Supervisor
  ) where

import Common
import Control.Eff.Concurrent.Protocol.EffectfulServer (Event(..))
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import Control.Eff.Concurrent.Protocol.Supervisor as Sup
import qualified Control.Eff.Concurrent.Protocol.Observer.Queue as OQ
import Control.Lens

test_Supervisor :: HasCallStack => TestTree
test_Supervisor =
  setTravisTestOptions $
  testGroup
    "Supervisor"
    (let startTestSup = startTestSupWith ExitWhenRequested (TimeoutMicros 500000)
         startTestSupWith e t = Sup.startLink (MkSupConfig t (Stateful.Init . TestServerArgs e))
         spawnTestChild sup i = Sup.spawnChild sup i >>= either (lift . assertFailure . show) pure
      in [ runTestCase "The supervisor starts and is shut down" $ do
             outerSelf <- self
             testWorker <-
               spawn "test-worker" $ do
                 sup <- startTestSup
                 sendMessage outerSelf sup
                 () <- receiveMessage
                 sendMessage outerSelf ()
                 () <- receiveMessage
                 Sup.stopSupervisor sup
             unlinkProcess testWorker
             sup <- receiveMessage :: Eff Effects  (Endpoint (Sup.Sup (Stateful.Stateful TestProtocol)))
             supAliveAfter1 <- isSupervisorAlive sup
             logInfo ("still alive 1: " <> pack (show supAliveAfter1))
             lift (supAliveAfter1 @=? True)
             sendMessage testWorker ()
             () <- receiveMessage
             supAliveAfter2 <- isSupervisorAlive sup
             logInfo ("still alive 2: " <> pack (show supAliveAfter2))
             lift (supAliveAfter2 @=? True)
             sendMessage testWorker ()
             testWorkerMonitorRef <- monitor testWorker
             d1 <- receiveSelectedMessage (selectProcessDown testWorkerMonitorRef)
             logInfo ("got test worker down: " <> pack (show d1))
             testSupervisorMonitorRef <- monitorSupervisor sup
             d2 <- receiveSelectedMessage (selectProcessDown testSupervisorMonitorRef)
             logInfo ("got supervisor down: " <> pack (show d2))
             supAliveAfterOwnerExited <- isSupervisorAlive sup
             logInfo ("still alive after owner exited: " <> pack (show supAliveAfterOwnerExited))
             lift (supAliveAfterOwnerExited @=? False)
         , testGroup
             "Diagnostics"
             [ runTestCase "When only time passes the diagnostics do not change" $ do
                 sup <- startTestSup
                 info1 <- Sup.getDiagnosticInfo sup
                 lift (threadDelay 10000)
                 info2 <- Sup.getDiagnosticInfo sup
                 lift (assertEqual "diagnostics should not differ: " info1 info2)
             , runTestCase "When a child is started the diagnostics change" $ do
                 sup <- startTestSup
                 info1 <- Sup.getDiagnosticInfo sup
                 logInfo ("got diagnostics: " <> info1)
                 let childId = 1
                 _child <- fromRight (error "failed to spawn child") <$> Sup.spawnChild sup childId
                 info2 <- Sup.getDiagnosticInfo sup
                 logInfo ("got diagnostics: " <> info2)
                 lift $ assertBool ("diagnostics should differ: " ++ show (info1, info2)) (info1 /= info2)
             ]
         , let childId = 1
            in testGroup
                 "Startup and shutdown"
                 [ runTestCase "When a supervisor is shut down, all children are shutdown" $ do
                     sup <- startTestSup
                     child <- spawnTestChild sup childId
                     let childPid = _fromEndpoint child
                     supMon <- monitorSupervisor sup
                     childMon <- monitor childPid
                     isProcessAlive childPid >>= lift . assertBool "child process not running"
                     isSupervisorAlive sup >>= lift . assertBool "supervisor process not running"
                     call child (TestGetStringLength "123") >>= lift . assertEqual "child not working" 3
                     stopSupervisor sup
                     d1@(ProcessDown mon1 er1 _) <-
                       fromMaybe (error "receive timeout 1") <$> receiveAfter (TimeoutMicros 1000000)
                     logInfo ("got process down: " <> pack (show d1))
                     d2@(ProcessDown mon2 er2 _) <-
                       fromMaybe (error "receive timeout 2") <$> receiveAfter (TimeoutMicros 1000000)
                     logInfo ("got process down: " <> pack (show d2))
                     case if mon1 == supMon && mon2 == childMon
                            then Right (er1, er2)
                            else if mon1 == childMon && mon2 == supMon
                                   then Right (er2, er1)
                                   else Left
                                          ("unexpected monitor down: first: " <> show (mon1, er1) <> ", and then: " <>
                                           show (mon2, er2) <>
                                           ", supMon: " <>
                                           show supMon <>
                                           ", childMon: " <>
                                           show childMon) of
                       Right (supER, childER) -> do
                         lift (assertEqual "bad supervisor exit reason" ExitNormally supER)
                         lift (assertEqual "bad child exit reason" ExitNormally childER)
                       Left x -> lift (assertFailure x)
                 , runTestCase
                     "When a supervisor is shut down, children that won't shutdown, are killed after some time" $ do
                     sup <- startTestSupWith IgnoreNormalExitRequest (TimeoutMicros 10000)
                     child <- spawnTestChild sup childId
                     let childPid = _fromEndpoint child
                     supMon <- monitorSupervisor sup
                     childMon <- monitor childPid
                     isProcessAlive childPid >>= lift . assertBool "child process not running"
                     isSupervisorAlive sup >>= lift . assertBool "supervisor process not running"
                     call child (TestGetStringLength "123") >>= lift . assertEqual "child not working" 3
                     stopSupervisor sup
                     d1@(ProcessDown mon1 er1 _) <-
                       fromMaybe (error "receive timeout 1") <$> receiveAfter (TimeoutMicros 1000000)
                     logInfo ("got process down: " <> pack (show d1))
                     d2@(ProcessDown mon2 er2 _) <-
                       fromMaybe (error "receive timeout 2") <$> receiveAfter (TimeoutMicros 1000000)
                     logInfo ("got process down: " <> pack (show d2))
                     case if mon1 == supMon && mon2 == childMon
                            then Right er1
                            else if mon1 == childMon && mon2 == supMon
                                   then Right er2
                                   else Left
                                          ("unexpected monitor down: first: " <> show (mon1, er1) <> ", and then: " <>
                                           show (mon2, er2) <>
                                           ", supMon: " <>
                                           show supMon <>
                                           ", childMon: " <>
                                           show childMon) of
                       Right supER -> do
                         lift (assertEqual "bad supervisor exit reason" ExitNormally supER)
                       Left x -> lift (assertFailure x)
                 ]
         , let i = 123
            in testGroup
                 "Spawning and Using Children"
                 [ runTestCase
                     "When a supervisor is requested to start two children with the same id, an already started error is returned" $ do
                     sup <- startTestSup
                     c <- spawnTestChild sup i
                     x <- Sup.spawnChild sup i
                     case x of
                       Left (AlreadyStarted i' c') ->
                         lift $ do
                           assertEqual "bad pid returned" c c'
                           assertEqual "bad child id returned" 123 i'
                       _ -> lift (assertFailure "AlreadyStarted expected!")
                 , runTestCase "When a child is started it can be lookup up" $ do
                     sup <- startTestSup
                     c <- spawnTestChild sup i
                     c' <- Sup.lookupChild sup i >>= maybe (lift (assertFailure "child not found")) pure
                     lift (assertEqual "lookupChild returned wrong child" c c')
                 , runTestCase "When several children are started they can be lookup up and don't crash" $ do
                     sup <- startTestSup
                     c <- spawnTestChild sup i
                     someOtherChild <- spawnTestChild sup (i + 1)
                     c' <- Sup.lookupChild sup i >>= maybe (lift (assertFailure "child not found")) pure
                     lift (assertEqual "lookupChild returned wrong child" c c')
                     childStillRunning <- isProcessAlive (_fromEndpoint c)
                     lift (assertBool "child not running" childStillRunning)
                     someOtherChildStillRunning <- isProcessAlive (_fromEndpoint someOtherChild)
                     lift (assertBool "someOtherChild not running" someOtherChildStillRunning)
                 , let startTestSupAndChild = do
                         sup <- startTestSup
                         c <- spawnTestChild sup i
                         cm <- monitor (_fromEndpoint c)
                         return (sup, cm)
                    in testGroup
                         "Stopping children"
                         [ runTestCase "When a child is started it can be stopped" $ do
                             (sup, cm) <- startTestSupAndChild
                             Sup.stopChild sup i >>= lift . assertBool "child not found"
                             (ProcessDown _ r _) <- receiveSelectedMessage (selectProcessDown cm)
                             lift (assertEqual "bad exit reason" ExitNormally r)
                         , runTestCase
                             "When a child is stopped but doesn't exit voluntarily, it is kill after some time" $ do
                             sup <- startTestSupWith IgnoreNormalExitRequest (TimeoutMicros 5000)
                             c <- spawnTestChild sup i
                             cm <- monitor (_fromEndpoint c)
                             Sup.stopChild sup i >>= lift . assertBool "child not found"
                             (ProcessDown _ r _) <- receiveSelectedMessage (selectProcessDown cm)
                             case r of
                               ExitUnhandledError _ -> return ()
                               _ -> lift (assertFailure ("bad exit reason: " ++ show r))
                         , runTestCase "When a stopChild is called with an unallocated ID, False is returned" $ do
                             (sup, _) <- startTestSupAndChild
                             Sup.stopChild sup (i + 1) >>= lift . assertBool "child not found" . not
                         , runTestCase "When a child is stopped, lookup won't find it" $ do
                             (sup, _) <- startTestSupAndChild
                             Sup.stopChild sup i >>= lift . assertBool "child not found"
                             x <- Sup.lookupChild sup i
                             lift (assertEqual "lookup should not find a child" Nothing x)
                         , runTestCase "When a child is stopped, the child id can be reused" $ do
                             (sup, _) <- startTestSupAndChild
                             Sup.stopChild sup i >>= lift . assertBool "child not found"
                             Sup.spawnChild sup i >>= lift . assertBool "id could not be reused" . isRight
                         ]
                 , let startTestSupAndChild = do
                         sup <- startTestSup
                         c <- spawnTestChild sup i
                         cm <- monitor (_fromEndpoint c)
                         return (sup, c, cm)
                    in testGroup
                         "Child exit handling"
                         [ runTestCase "When a child exits normally, lookupChild will not find it" $ do
                             (sup, c, cm) <- startTestSupAndChild
                             cast c (TestInterruptWith NormalExitRequested)
                             (ProcessDown _ r _) <- receiveSelectedMessage (selectProcessDown cm)
                             lift (assertEqual "bad exit reason" ExitNormally r)
                             x <- Sup.lookupChild sup i
                             lift (assertEqual "lookup should not find a child" Nothing x)
                         , runTestCase "When a child exits with an error, lookupChild will not find it" $ do
                             (sup, c, cm) <- startTestSupAndChild
                             cast c (TestInterruptWith (ErrorInterrupt "test error reason"))
                             (ProcessDown _ _ _) <- receiveSelectedMessage (selectProcessDown cm)
                             x <- Sup.lookupChild sup i
                             lift (assertEqual "lookup should not find a child" Nothing x)
                         , runTestCase "When a child is interrupted from another process and dies, lookupChild will not find it" $ do
                             (sup, c, cm) <- startTestSupAndChild
                             sendInterrupt (_fromEndpoint c) NormalExitRequested
                             (ProcessDown _ _ _) <- receiveSelectedMessage (selectProcessDown cm)
                             x <- Sup.lookupChild sup i
                             lift (assertEqual "lookup should not find a child" Nothing x)
                         , runTestCase "When a child is shutdown from another process and dies, lookupChild will not find it" $ do
                             (sup, c, cm) <- startTestSupAndChild
                             self >>= sendShutdown (_fromEndpoint c) . ExitProcessCancelled . Just
                             (ProcessDown _ _ _) <- receiveSelectedMessage (selectProcessDown cm)
                             x <- Sup.lookupChild sup i
                             lift (assertEqual "lookup should not find a child" Nothing x)
                         ]
                 , testGroup "supervisor events"
                     [ runTestCase "when a child starts the observer is notified" $ do
                         sup <- startTestSup
                         OQ.observe @(Sup.ChildEvent (Stateful.Stateful TestProtocol)) (100 :: Int) sup $ do
                           c <- Sup.spawnChild @(Stateful.Stateful TestProtocol) sup i >>= either (lift . assertFailure . show) pure
                           e <- OQ.await @(Sup.ChildEvent (Stateful.Stateful TestProtocol))
                           case e of
                            Sup.OnChildSpawned i' c' -> do
                              lift (assertEqual "lookupChild returned wrong child" c c')
                              lift (assertEqual "lookupChild returned wrong child-id" i i')
                            _ ->
                              lift (assertFailure ("unexpected event: " ++ show e))
                    , runTestCase "when a child stops the observer is notified" $ do
                         sup <- startTestSup
                         c <- Sup.spawnChild @(Stateful.Stateful TestProtocol) sup i >>= either (lift . assertFailure . show) pure
                         OQ.observe @(Sup.ChildEvent (Stateful.Stateful TestProtocol)) (100 :: Int) sup $ do
                           Sup.stopChild sup i >>= lift . assertBool "child not found"
                           OQ.await @(Sup.ChildEvent (Stateful.Stateful TestProtocol))
                            >>= \case
                                    Sup.OnChildDown i' c' e -> do
                                      lift (assertEqual "lookupChild returned wrong child" c c')
                                      lift (assertEqual "lookupChild returned wrong child-id" i i')
                                      lift (assertEqual "bad exit reason" ExitNormally e)
                                    e ->
                                      lift (assertFailure ("unexpected event: " ++ show e))

                    , runTestCase "when a child crashes the observer is notified" $ do
                         sup <- startTestSup
                         OQ.observe @(Sup.ChildEvent (Stateful.Stateful TestProtocol)) (100 :: Int) sup $ do
                           c <- Sup.spawnChild @(Stateful.Stateful TestProtocol) sup i >>= either (lift . assertFailure . show) pure
                           let expectedError = ExitUnhandledError "test error"
                           sendShutdown (_fromEndpoint c) expectedError
                           OQ.await @(Sup.ChildEvent (Stateful.Stateful TestProtocol))
                            >>= \case
                                    Sup.OnChildSpawned i' c' -> do
                                      lift (assertEqual "lookupChild returned wrong child" c c')
                                      lift (assertEqual "lookupChild returned wrong child-id" i i')
                                    e ->
                                      lift (assertFailure ("unexpected event: " ++ show e))
                           OQ.await @(Sup.ChildEvent (Stateful.Stateful TestProtocol))
                            >>= \case
                                    Sup.OnChildDown i' c' e -> do
                                      lift (assertEqual "lookupChild returned wrong child" c c')
                                      lift (assertEqual "lookupChild returned wrong child-id" i i')
                                      lift (assertEqual "bad exit reason" expectedError e)
                                    e ->
                                      lift (assertFailure ("unexpected event: " ++ show e))
                    , runTestCase "when a child does not stop when requested and is killed, the oberser is notified correspondingly" $ do
                         sup <- startTestSupWith IgnoreNormalExitRequest (TimeoutMicros 5000)
                         c <- spawnTestChild sup i
                         OQ.observe @(Sup.ChildEvent (Stateful.Stateful TestProtocol)) (100 :: Int) sup $ do
                           cm <- monitor (_fromEndpoint c)
                           Sup.stopChild sup i >>= lift . assertBool "child not found"
                           (ProcessDown _ expectedError _) <- receiveSelectedMessage (selectProcessDown cm)
                           OQ.await @(Sup.ChildEvent (Stateful.Stateful TestProtocol))
                            >>= \case
                                    Sup.OnChildDown i' c' e -> do
                                      lift (assertEqual "lookupChild returned wrong child" c c')
                                      lift (assertEqual "lookupChild returned wrong child-id" i i')
                                      lift (assertEqual "bad exit reason" expectedError e)
                                    e ->
                                      lift (assertFailure ("unexpected event: " ++ show e))

                    , runTestCase "when the supervisor stops, an OnSupervisorShuttingDown is emitted before any child is stopped" $ do
                         sup <- startTestSup
                         unlinkProcess (sup ^. fromEndpoint)
                         void (Sup.spawnChild @(Stateful.Stateful TestProtocol) sup i
                                >>= either (lift . assertFailure . show) pure)
                         OQ.observe @(Sup.ChildEvent (Stateful.Stateful TestProtocol)) (1000 :: Int) sup $ do
                           Sup.stopSupervisor sup
                           OQ.await @(Sup.ChildEvent (Stateful.Stateful TestProtocol))
                            >>= \case
                                    Sup.OnSupervisorShuttingDown -> logNotice "received OnSupervisorShuttingDown"
                                    e ->
                                      lift (assertFailure ("unexpected event: " ++ show e))
                           OQ.await @(Sup.ChildEvent (Stateful.Stateful TestProtocol))
                            >>= \case
                                  Sup.OnChildDown _ _ e -> lift (assertEqual "bad exit reason" ExitNormally e)
                                  e ->
                                    lift (assertFailure ("unexpected event: " ++ show e))
                    ]
                 ]
         ])

data TestProtocol
  deriving (Typeable)

type instance ToPretty TestProtocol = PutStr "test"

instance HasPdu TestProtocol where
  data instance Pdu TestProtocol x where
    TestGetStringLength :: String -> Pdu TestProtocol ('Synchronous Int)
    TestInterruptWith :: Interrupt 'Recoverable -> Pdu TestProtocol 'Asynchronous
      deriving Typeable

instance NFData (Pdu TestProtocol x) where
  rnf (TestGetStringLength x) = rnf x
  rnf (TestInterruptWith x) = rnf x

instance Show (Pdu TestProtocol r) where
  show (TestGetStringLength s) = "TestGetStringLength " ++ show s
  show (TestInterruptWith s) = "TestInterruptWith " ++ show s

data TestProtocolServerMode
  = IgnoreNormalExitRequest
  | ExitWhenRequested
  deriving Eq

instance Stateful.Server TestProtocol Effects where
  newtype instance Model TestProtocol = TestProtocolModel () deriving Default
  update _me (TestServerArgs testMode tId) evt =
    case evt of
      OnCast (TestInterruptWith i) -> do
        logInfo (pack (show tId) <> ": stopping with: " <> pack (show i))
        interrupt i
      OnCall rt (TestGetStringLength str) -> do
        logInfo (pack (show tId) <> ": calculating length of: " <> pack str)
        sendReply rt (length str)
      OnInterrupt x -> do
        logNotice (pack (show tId) <> ": " <> pack (show x))
        if testMode == IgnoreNormalExitRequest
          then
            logNotice $ pack (show tId) <> ": ignoring normal exit request"
          else do
            logNotice $ pack (show tId) <> ": exitting normally"
            exitBecause (interruptToExit x)
      _ ->
        logDebug (pack (show tId) <> ": got some info: " <> pack (show evt))
  data instance StartArgument TestProtocol Effects = TestServerArgs TestProtocolServerMode Int

type instance ChildId TestProtocol = Int

