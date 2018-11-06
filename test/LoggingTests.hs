module LoggingTests where

import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Log
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import           Control.Concurrent.STM
import           Control.DeepSeq

demo :: ([Logs String, Logs OtherLogMsg] <:: e) => Eff e ()
demo = do
  logMsg "jo"
  logMsg (OtherLogMsg "test 123")


test_loggingInterception :: TestTree
test_loggingInterception = setTravisTestOptions $ testGroup
  "Intercept Logging"
  [ testCase "Convert Log Message Type" $ do
      otherLogsQueue  <- newTQueueIO @OtherLogMsg
      stringLogsQueue <- newTQueueIO @String
      runLift
        (writeLogs
          (multiMessageLogWriter ($ (atomically . writeTQueue stringLogsQueue)))
          (writeLogs
            (multiMessageLogWriter ($ (atomically . writeTQueue otherLogsQueue))
            )
            (mapLogMessages @String reverse  demo)
          )
        )

      otherLogs <- atomically (flushTQueue otherLogsQueue)
      strLogs   <- atomically (flushTQueue stringLogsQueue)
      assertEqual "string logs" ["oj"]                   strLogs
      assertEqual "other logs"  [OtherLogMsg "test 123"] otherLogs
  ]

newtype OtherLogMsg = OtherLogMsg String deriving (Eq, NFData, Show)
