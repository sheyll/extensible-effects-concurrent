module LoggingTests where

import           Control.Eff
import           Control.Eff.Log
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import Control.Lens ((.~))

demo :: ('[Logs] <:: e) => Eff e ()
demo = do
  logInfo "jo"
  logDebug "oh"

pureLogs :: Eff '[Logs, LogWriterReader CaptureLogs, CapturedLogsWriter] a -> [LogMessage]
pureLogs =
    snd
  . run
  . runCapturedLogsWriter
  . runLogWriterReader listLogWriter
  . runLogs @CaptureLogs
  . censorLogs @CaptureLogs (lmSrcLoc .~ Nothing)

test_Logging :: TestTree
test_Logging = setTravisTestOptions $ testGroup "Logging"
  [ testCase "basic logging works" $
      pureLogs demo @?= (lmSrcLoc .~ Nothing) <$> [infoMessage "jo", debugMessage "oh"]
  ]