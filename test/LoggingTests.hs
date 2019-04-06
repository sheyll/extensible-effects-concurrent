module LoggingTests where

import           Control.Eff
import           Control.Eff.Log
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import           Control.Lens       ((.~))


test_Logging :: TestTree
test_Logging = setTravisTestOptions $ testGroup "Logging"
  [ basics
  , strictness
  ]


basics :: HasCallStack => TestTree
basics =
  testCase "basic logging works" $
      pureLogs demo @?= (lmSrcLoc .~ Nothing) <$> [infoMessage "jo", debugMessage "oh"]
 where

    demo :: ('[Logs] <:: e) => Eff e ()
    demo = do
      logInfo "jo"
      logDebug "oh"

    pureLogs :: Eff '[Logs, LogWriterReader CaptureLogs, CapturedLogsWriter] a -> [LogMessage]
    pureLogs =
        snd
      . run
      . runCapturedLogsWriter
      . withLogging listLogWriter
      . censorLogs @CaptureLogs (lmSrcLoc .~ Nothing)

strictness :: HasCallStack => TestTree
strictness =
  testCase "messages failing the predicate are not deeply evaluated"
    $ runLift
    $ withLogging consoleLogWriter
    $ excludeLogMessages (lmSeverityIs errorSeverity)
    $ do logDebug "test"
         logMsg (errorMessage ("test" ++ error "TEST FAILED: this log statement should not have been evaluated deeply"))

