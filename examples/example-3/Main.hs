module Main where

import           Control.Eff
import           Control.Eff.Log
import           Control.Lens

main :: IO ()
main =
  runLift
  $  runLogWriterReader (noOpLogWriter @IO)
  $  runLogs
  $  withLogFileAppender  "extensible-effects-concurrent-example-3.log"
  $  addLogWriter (filteringLogWriter testPred (mappingLogWriter (lmMessage %~ ("TRACED "++)) debugTraceLogWriter))
  $  setThreadIdAndTimestamp
  $  do
        logEmergency "test emergencySeverity 1"
        logCritical "test criticalSeverity 2"
        logAlert "test alertSeverity 3"
        logError "test errorSeverity 4"
        logWarning "test warningSeverity 5"
        logInfo "test informationalSeverity 6"
        logDebug "test debugSeverity 7"


testPred :: LogPredicate
testPred = view (lmSeverity . to (<= errorSeverity))

