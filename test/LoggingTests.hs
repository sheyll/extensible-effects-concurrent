module LoggingTests where

import           Control.Eff
import           Control.Eff.Log
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import           Control.Concurrent.STM
import           Control.DeepSeq

demo :: ('[Logs] <:: e) => Eff e ()
demo = do
  logInfo "jo"
  logDebug "oh"

pureLogs :: Eff '[Logs, CapturedLogsWriter] a -> [LogMessage]
pureLogs = snd . run . runCapturedLogsWriter . runLogs . logTo captureLogMessages

test_Logging = setTravisTestOptions $ testGroup "Logging"
  [ testCase "basic logging works" $
      pureLogs demo @?= [infoMessage "jo", infoMessage "oh"]
  ]