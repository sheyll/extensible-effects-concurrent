module SupervisorTests
  ( test_Supervisor
  ) where

import Common
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent
import Control.Eff.Concurrent.Api.Supervisor as Sup
import Control.Eff.Concurrent.Process.ForkIOScheduler as Scheduler
import Control.Eff.Concurrent.Process.Timer
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text ( pack)
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.HUnit

test_Supervisor :: HasCallStack => TestTree
test_Supervisor =
  setTravisTestOptions $
  testGroup
    "Supervisor"
    [ runTestCase "The supervisor starts, returns diagnostic info, and is shut down" $ do
        outerSelf <- self
        testWorker <-
          spawn $ do
            sup <- Sup.startLink spawnTestApiProcess
            logInfo ("started: " <> pack (show sup))
            sendMessage outerSelf (_fromServer sup)
            () <- receiveMessage
            diag <- Sup.getDiagnosticInfo sup
            sendMessage outerSelf diag
            () <- receiveMessage
            sendInterrupt (_fromServer sup) NormalExitRequested
        unlinkProcess testWorker
        sup <- receiveMessage
        supAliveAfterStart <- isProcessAlive sup
        logInfo ("still alive after start: " <> pack (show supAliveAfterStart))
        lift (supAliveAfterStart @=? True)
        sendMessage testWorker ()
        diag <- receiveMessage
        logInfo ("got diagnostics: " <> diag)
        supAliveAfterDiag <- isProcessAlive sup
        logInfo ("still alive after diag: " <> pack (show supAliveAfterDiag))
        lift (supAliveAfterDiag @=? True)
        sendMessage testWorker ()
        _ <- monitor testWorker
        d1@(ProcessDown _ _) <- receiveMessage
        logInfo ("got test worker down: " <> pack (show d1))
        _ <- monitor sup
        d2@(ProcessDown _ _) <- receiveMessage
        logInfo ("got supervisor down: " <> pack (show d2))
        supAliveAfterOwnerExited <- isProcessAlive sup
        logInfo ("still alive after owner exited: " <> pack (show supAliveAfterOwnerExited))
        lift (supAliveAfterOwnerExited @=? False)
    , runTestCase "When a supervisor starts a child and is shut down, the child then exits, too" $ do
        sup <- Sup.startLink spawnTestApiProcess
        logInfo ("started: " <> pack (show sup))
        diag1 <- Sup.getDiagnosticInfo sup
        logInfo ("got diagnostics: " <> diag1)
        let childId = 1
        child <- fromRight (error "failed to spawn child") <$> Sup.spawnChild sup childId
        diag2 <- Sup.getDiagnosticInfo sup
        logInfo ("got diagnostics: " <> diag2)
        lift $ assertBool ("diagnostics should differ: " ++ show (diag1, diag2)) (diag1 /= diag2)
        let supPid = _fromServer sup
            childPid = _fromServer child
        supMon <- monitor supPid
        childMon <- monitor childPid
        isProcessAlive childPid >>= lift . assertBool "child process not running"
        isProcessAlive supPid >>= lift . assertBool "supervisor process not running"
        call child (TestGetStringLength "123") >>= lift . assertEqual "child not working" 3
        sendShutdown supPid ExitNormally
         -- TODO discuss: Add stopSupervisor API, hide the supervisor pid in a newtype?
         -- TODO discuss: Let the linked processes die even when the exit is "ExitNormally"?
         -- TODO discuss: Add another process to monitor the supervisor process, to kill the children when the supervisor dies?
        d1@(ProcessDown mon1 er1) <- fromMaybe (error "receive timeout 1") <$> receiveAfter (TimeoutMicros 1000000)
        logInfo ("got process down: " <> pack (show d1))
        d2@(ProcessDown mon2 er2) <- fromMaybe (error "receive timeout 2") <$> receiveAfter (TimeoutMicros 1000000)
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
            lift (assertEqual "bad supervisor exit reason" supER (SomeExitReason ExitNormally))
            lift (assertEqual "bad child exit reason" childER (SomeExitReason (LinkedProcessCrashed supPid)))
          Left x -> lift (assertFailure x)
    ]

runTestCase :: TestName -> Eff InterruptableProcEff () -> TestTree
runTestCase msg =
  testCase msg .
  runLift . withTraceLogging "supervisor-test" local0 allLogMessages . Scheduler.schedule . handleInterrupts onInt
 where
  onInt = lift . assertFailure . show

spawnTestApiProcess :: Sup.SpawnFun Int InterruptableProcEff (Server TestApi)
spawnTestApiProcess tId =
  spawnApiServer (handleCalls onCall ^: handleAnyMessages onInfo) (InterruptCallback onInterrupt)
  where
    onCall :: Api TestApi ('Synchronous r) -> (Eff InterruptableProcEff (Maybe r, CallbackResult 'Recoverable) -> x) -> x
    onCall (TestGetStringLength str) runMe =
      runMe $ do
        logInfo (pack (show tId) <> ": calculating length of: " <> pack str)
        pure (Just (length str), AwaitNext)
    onInfo :: StrictDynamic -> Eff InterruptableProcEff (CallbackResult 'Recoverable)
    onInfo sd = do
      logDebug (pack (show tId) <> ": got some info: " <> pack (show sd))
      pure AwaitNext
    onInterrupt x = do
      logWarning (pack (show tId) <> ": interrupted: " <> pack (show x))
      pure AwaitNext

data TestApi
  deriving (Typeable)

data instance  Api TestApi x where
        TestGetStringLength :: String -> Api TestApi ('Synchronous Int)
    deriving Typeable

instance NFData (Api TestApi x) where
  rnf (TestGetStringLength x) = rnf x