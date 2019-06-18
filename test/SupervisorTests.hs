module SupervisorTests
  ( test_Supervisor
  ) where

import Common
import Control.Concurrent (threadDelay)
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent
import Control.Eff.Concurrent.Protocol.StatefulServer as Server
import Control.Eff.Concurrent.Protocol.Supervisor as Sup
import Control.Eff.Concurrent.Process.Timer
import Data.Either (fromRight, isLeft, isRight)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Type.Pretty
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.HUnit

test_Supervisor :: HasCallStack => TestTree
test_Supervisor =
  setTravisTestOptions $
  testGroup
    "Supervisor"
    (let startTestSup = startTestSupWith ExitWhenRequested (TimeoutMicros 500000)
         startTestSupWith e t = Sup.startSupervisor (MkSupConfig t (TestServerArgs e))
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
             sup <- receiveMessage :: Eff Effects  (Endpoint (Sup.Sup TestProtocol))
             supAliveAfter1 <- isSupervisorAlive sup
             logInfo ("still alive 1: " <> pack (show supAliveAfter1))
             lift (supAliveAfter1 @=? True)
             sendMessage testWorker ()
             () <- receiveMessage
             supAliveAfter2 <- isSupervisorAlive sup
             logInfo ("still alive 2: " <> pack (show supAliveAfter2))
             lift (supAliveAfter2 @=? True)
             sendMessage testWorker ()
             _ <- monitor testWorker
             d1@(ProcessDown _ _) <- receiveMessage
             logInfo ("got test worker down: " <> pack (show d1))
             _ <- monitorSupervisor sup
             d2@(ProcessDown _ _) <- receiveMessage
             logInfo ("got supervisor down: " <> pack (show d2))
             supAliveAfterOwnerExited <- isSupervisorAlive sup
             logInfo ("still alive after owner exited: " <> pack (show supAliveAfterOwnerExited))
             lift (supAliveAfterOwnerExited @=? False)
         , testGroup
             "Diagnostics"
             [ runTestCase "When only time passes the diagnostics do not change" $ do
                 sup <- startTestSup
                 diag1 <- Sup.getDiagnosticInfo sup
                 lift (threadDelay 10000)
                 diag2 <- Sup.getDiagnosticInfo sup
                 lift (assertEqual "diagnostics should not differ: " diag1 diag2)
             , runTestCase "When a child is started the diagnostics change" $ do
                 sup <- startTestSup
                 diag1 <- Sup.getDiagnosticInfo sup
                 logInfo ("got diagnostics: " <> diag1)
                 let childId = 1
                 _child <- fromRight (error "failed to spawn child") <$> Sup.spawnChild sup childId
                 diag2 <- Sup.getDiagnosticInfo sup
                 logInfo ("got diagnostics: " <> diag2)
                 lift $ assertBool ("diagnostics should differ: " ++ show (diag1, diag2)) (diag1 /= diag2)
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
                     d1@(ProcessDown mon1 er1) <-
                       fromMaybe (error "receive timeout 1") <$> receiveAfter (TimeoutMicros 1000000)
                     logInfo ("got process down: " <> pack (show d1))
                     d2@(ProcessDown mon2 er2) <-
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
                         lift (assertEqual "bad supervisor exit reason" (SomeExitReason ExitNormally) supER)
                         lift (assertEqual "bad child exit reason" (SomeExitReason ExitNormally) childER)
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
                     d1@(ProcessDown mon1 er1) <-
                       fromMaybe (error "receive timeout 1") <$> receiveAfter (TimeoutMicros 1000000)
                     logInfo ("got process down: " <> pack (show d1))
                     d2@(ProcessDown mon2 er2) <-
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
                         lift (assertEqual "bad supervisor exit reason" (SomeExitReason ExitNormally) supER)
                         lift (assertBool "bad child exit reason" (isLeft (fromSomeExitReason childER)))
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
                             (ProcessDown _ r) <- receiveSelectedMessage (selectProcessDown cm)
                             lift (assertEqual "bad exit reason" (SomeExitReason ExitNormally) r)
                         , runTestCase
                             "When a child is stopped but doesn't exit voluntarily, it is kill after some time" $ do
                             sup <- startTestSupWith IgnoreNormalExitRequest (TimeoutMicros 5000)
                             c <- spawnTestChild sup i
                             cm <- monitor (_fromEndpoint c)
                             Sup.stopChild sup i >>= lift . assertBool "child not found"
                             (ProcessDown _ r) <- receiveSelectedMessage (selectProcessDown cm)
                             case r of
                               SomeExitReason (ExitUnhandledError _) -> return ()
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
                             (ProcessDown _ r) <- receiveSelectedMessage (selectProcessDown cm)
                             lift (assertEqual "bad exit reason" (SomeExitReason ExitNormally) r)
                             x <- Sup.lookupChild sup i
                             lift (assertEqual "lookup should not find a child" Nothing x)
                         , runTestCase "When a child exits with an error, lookupChild will not find it" $ do
                             (sup, c, cm) <- startTestSupAndChild
                             cast c (TestInterruptWith (ErrorInterrupt "test error reason"))
                             (ProcessDown _ _) <- receiveSelectedMessage (selectProcessDown cm)
                             x <- Sup.lookupChild sup i
                             lift (assertEqual "lookup should not find a child" Nothing x)
                         , runTestCase "When a child is interrupted from another process and dies, lookupChild will not find it" $ do
                             (sup, c, cm) <- startTestSupAndChild
                             sendInterrupt (_fromEndpoint c) NormalExitRequested
                             (ProcessDown _ _) <- receiveSelectedMessage (selectProcessDown cm)
                             x <- Sup.lookupChild sup i
                             lift (assertEqual "lookup should not find a child" Nothing x)
                         , runTestCase "When a child is shutdown from another process and dies, lookupChild will not find it" $ do
                             (sup, c, cm) <- startTestSupAndChild
                             sendShutdown (_fromEndpoint c) ExitProcessCancelled
                             (ProcessDown _ _) <- receiveSelectedMessage (selectProcessDown cm)
                             x <- Sup.lookupChild sup i
                             lift (assertEqual "lookup should not find a child" Nothing x)
                         ]
                 ]
         ])

data TestProtocol
  deriving (Typeable)

type instance ToPretty TestProtocol = PutStr "test"

instance Typeable x => HasPdu TestProtocol x where
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

instance Server TestProtocol Effects where
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
