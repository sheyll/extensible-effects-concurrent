module LoggingTests where

import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Log
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Monad.IO.Class

demo
  :: (Member (Logs String) e, Member (Logs OtherLogMsg) e, MonadIO (Eff e))
  => Eff e ()
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
        (handleLogsWith
          (atomically . writeTQueue stringLogsQueue)
          (handleLogsWith (atomically . writeTQueue otherLogsQueue)
                          (interceptLogging reverseStringLogs demo)
          )
        )
      otherLogs <- atomically (flushTQueue otherLogsQueue)
      strLogs   <- atomically (flushTQueue stringLogsQueue)
      assertEqual "string logs" ["oj"]                   strLogs
      assertEqual "other logs"  [OtherLogMsg "test 123"] otherLogs
  ]

newtype OtherLogMsg = OtherLogMsg String deriving (Eq, NFData, Show)

reverseStringLogs
  :: ('[Logs String, Lift IO] <:: e, MonadIO (Eff e)) => String -> Eff e ()
reverseStringLogs = logMsg . reverse
