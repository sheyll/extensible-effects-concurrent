-- | Examples for Logging.
module Control.Eff.Log.Examples
  ( -- * Example Code for Logging
    exampleLogging
  , exampleWithLogging
  , exampleWithSomeLogging
  , exampleLogPredicate
  , exampleLogCapture
  , exampleAsyncLogging
  )
where

import           Control.Eff.Log
import           Control.Eff.LogWriter.Async
import           Control.Eff.LogWriter.Console
import           Control.Eff.LogWriter.File
import           Control.Eff
import           Control.Lens (view, (%~), to)
import           Data.Text    as T
import           Data.Text.IO as T

-- * Logging examples

-- | Example code for:
--
--  * 'withConsoleLogging'
--  * 'ioLogWriter'
--  * 'printLogMessage'
--  * 'logDebug'
--  * 'logError'
--  * 'prefixLogMessagesWith'
--  * 'addLogWriter'
--  * 'debugTraceLogWriter'
--  * 'setLogPredicate'
--  * 'logInfo'
--  * 'logMsg'
--  * 'logWarning'
--  * 'logCritical'
--  * 'lmMessage'
exampleLogging :: IO ()
exampleLogging =
    runLift
  $ withConsoleLogging "my-app" local7 allLogMessages
  $ do
      logDebug "test 1.1"
      logError "test 1.2"
      censorLogs (prefixLogMessagesWith "NESTED: ")
       $ do
            addLogWriter debugTraceLogWriter
             $ setLogPredicate (\m -> (view lmMessage m) /= "not logged")
             $ do
                  logInfo "not logged"
                  logMsg  "test 2.1"
            logWarning "test 2.2"
      logCritical "test 1.3"

-- | Example code for:
--
--  * 'withLogging'
--  * 'consoleLogWriter'
exampleWithLogging :: IO ()
exampleWithLogging =
    runLift
  $ withLogging consoleLogWriter
  $ logDebug "Oh, hi there"

-- | Example code for:
--
--  * 'withSomeLogging'
--  * 'PureLogWriter'
--  * 'logDebug'
exampleWithSomeLogging :: ()
exampleWithSomeLogging =
    run
  $ withSomeLogging @PureLogWriter
  $ logDebug "Oh, hi there"

-- | Example code for:
--
--  * 'setLogPredicate'
--  * 'modifyLogPredicate'
--  * 'lmMessageStartsWith'
--  * 'lmSeverityIs'
--  * 'lmSeverityIsAtLeast'
--  * 'includeLogMessages'
--  * 'excludeLogMessages'
exampleLogPredicate :: IO Int
exampleLogPredicate =
    runLift
  $ withSomeLogging @IO
  $ setLogWriter consoleLogWriter
  $ do logInfo "test"
       setLogPredicate (lmMessageStartsWith "OMG")
                         (do logInfo "this message will not be logged"
                             logInfo "OMG logged"
                             modifyLogPredicate (\p lm -> p lm || lmSeverityIs errorSeverity lm) $ do
                               logDebug "OMG logged"
                               logInfo "Not logged"
                               logError "Logged"
                               logEmergency "Not Logged"
                               includeLogMessages (lmSeverityIsAtLeast warningSeverity) $ do
                                 logInfo "Not logged"
                                 logError "Logged"
                                 logEmergency "Logged"
                                 logWarning "Logged"
                                 logDebug "OMG still Logged"
                                 excludeLogMessages (lmMessageStartsWith "OMG") $ do
                                   logDebug "OMG NOT Logged"
                                   logError "OMG ALSO NOT Logged"
                                   logEmergency "Still Logged"
                                   logWarning "Still Logged"
                                 logWarning "Logged"
                                 logDebug "OMG still Logged"
                             return 42)

-- | Example code for:
--
--  * 'runCapturedLogsWriter'
--  * 'listLogWriter'
--  * 'mappingLogWriter'
--  * 'filteringLogWriter'
exampleLogCapture :: IO ()
exampleLogCapture = go >>= T.putStrLn
 where go = fmap (T.unlines . Prelude.map renderLogMessageConsoleLog . snd)
              $  runLift
              $  runCapturedLogsWriter
              $  withLogging listLogWriter
              $  addLogWriter (mappingLogWriter (lmMessage %~ ("CAPTURED " <>)) listLogWriter)
              $  addLogWriter (filteringLogWriter severeMessages (mappingLogWriter (lmMessage %~ ("TRACED " <>)) debugTraceLogWriter))
              $  do
                    logEmergency "test emergencySeverity 1"
                    logCritical "test criticalSeverity 2"
                    logAlert "test alertSeverity 3"
                    logError "test errorSeverity 4"
                    logWarning "test warningSeverity 5"
                    logInfo "test informationalSeverity 6"
                    logDebug "test debugSeverity 7"
       severeMessages = view (lmSeverity . to (<= errorSeverity))


-- | Example code for:
--
--  * 'withAsyncLogging'
exampleAsyncLogging :: IO ()
exampleAsyncLogging =
    runLift
  $ withLogging consoleLogWriter
  $ withAsyncLogWriter (1000::Int)
  $ do logInfo "test 1"
       logInfo "test 2"
       logInfo "test 3"
