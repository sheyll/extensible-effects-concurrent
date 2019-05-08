module SupervisorTests
  ( test_Supervisor
  ) where

import Common
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent
import Control.Eff.Concurrent.Api.Supervisor as Sup
import Control.Eff.Concurrent.Process.ForkIOScheduler as Scheduler
import Control.Monad
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text, pack)

test_Supervisor :: TestTree
test_Supervisor =
  setTravisTestOptions $
  testGroup
    "Supervisor"
    [ runTestCase "The supervisor starts, returns diagnostic info, and is shut down" $ do
        outerSelf <- self

        testWorker <- spawn $ do
          sup <- Sup.startLink spawnTestApiProcess
          logInfo ("started: " <> pack (show sup))
          sendMessage outerSelf (_fromServer sup)
          () <- receiveMessage
          diag <- Sup.getDiagnosticInfo sup
          sendMessage outerSelf diag
          () <- receiveMessage
          sendShutdown (_fromServer sup) ExitNormally

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
    ]

runTestCase :: TestName -> Eff InterruptableProcEff () -> TestTree
runTestCase msg = testCase msg . runLift . withTraceLogging "supervisor-test" local0 allLogMessages . Scheduler.schedule

spawnTestApiProcess :: Sup.SpawnFun Int InterruptableProcEff (Server TestApi)
spawnTestApiProcess tId = spawnApiServer (handleCalls onCall ^: handleAnyMessages onInfo) (InterruptCallback onInterrupt)
 where
  onCall :: Api TestApi ('Synchronous r) -> (Eff InterruptableProcEff (Maybe r, CallbackResult) -> x) -> x
  onCall (TestGetStringLength str) runMe = runMe $ do
    logInfo (pack (show tId) <> ": calculating length of: " <> pack str)
    pure (Just (length str), AwaitNext)
  onInfo :: StrictDynamic -> Eff InterruptableProcEff CallbackResult
  onInfo sd = do
    logDebug (pack (show tId) <> ": got some info: " <> pack (show sd))
    pure AwaitNext
  onInterrupt x = do
    logWarning (pack (show tId) <> ": interrupted: " <> pack (show  x))
    pure AwaitNext

data TestApi
  deriving (Typeable)

data instance  Api TestApi x where
        TestGetStringLength :: String -> Api TestApi ('Synchronous Int)
    deriving Typeable

instance NFData (Api TestApi x) where
  rnf (TestGetStringLength x) = rnf x