module LoggingTests where

import           Control.Eff
import           Control.Eff.Log
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import           Control.DeepSeq
import qualified Data.Sequence                 as Seq


demo :: Member (Logs String) e => Eff e ()
demo = do
  logMsg "jo"
  logMsg "test 123"


test_loggingInterception :: TestTree
test_loggingInterception = setTravisTestOptions $ testGroup
  "Intercept Logging"
  [ testCase "Convert Log Message Type" $ do
      let (((), strLogs), otherLogs) = run
            (captureLogs @OtherLogMsg
              (captureLogs @String (interceptLogging toOtherLogMsg demo))
            )
      assertEqual "string logs must be empty" Seq.empty strLogs
      assertEqual "other logs should contain all logging"
                  (Seq.fromList [OtherLogMsg "jo", OtherLogMsg "test 123"])
                  otherLogs
  ]

newtype OtherLogMsg = OtherLogMsg String deriving (Eq, NFData, Show)

toOtherLogMsg :: ('[Logs String, Logs OtherLogMsg] <:: e) => String -> Eff e ()
toOtherLogMsg = logMsg . OtherLogMsg

demo2 :: Member (Logs LogMessage) e => Eff e ()
demo2 = relogAsDebugMessages demo
