{-# LANGUAGE NoOverloadedStrings #-}

module Main where

import Control.Eff
import Control.Eff.Log
import Control.Eff.LogWriter.DebugTrace
import Control.Eff.LogWriter.File
import Control.Eff.LogWriter.Rich
import Control.Lens

main :: IO ()
main =
  runLift
    $ withFileLogging "extensible-effects-concurrent-example-3.log" "test-app" local0 allLogEvents renderConsoleMinimalisticWide
    $ addLogWriter (filteringLogWriter severeMessages (mappingLogWriter (logEventMessage %~ (packLogMsg "traced: " <>)) (debugTraceLogWriter renderRFC5424)))
    $ modifyLogWriter (richLogWriter "example-3" local0)
    $ addLogWriter (filteringLogWriter severeMessages (mappingLogWriter (logEventMessage %~ (packLogMsg "traced without timestamp: " <>)) (debugTraceLogWriter renderRFC5424)))
    $ do
      logEmergency "test emergencySeverity 1"
      logCritical "test criticalSeverity 2"
      logAlert "test alertSeverity 3"
      logError "test errorSeverity 4"
      logWarning "test warningSeverity 5"
      logInfo "test informationalSeverity 6"
      logDebug "test debugSeverity 7"

severeMessages :: LogPredicate
severeMessages = view (logEventSeverity . to (<= errorSeverity))
