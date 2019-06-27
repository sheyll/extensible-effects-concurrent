-- | Examples for Logging.
module Control.Eff.Log.Examples
  ( -- * Example Code for Logging
    exampleLogging
  , exampleWithLogging
  , exampleWithSomeLogging
  , exampleLogPredicate
  , exampleLogCapture
  , exampleAsyncLogging
  , exampleRFC5424Logging
  , exampleRFC3164WithRFC5424TimestampsLogging
  , exampleDevLogSyslogLogging
  , exampleDevLogRFC5424Logging
  , exampleUdpRFC5424Logging
  , exampleUdpRFC3164Logging
  -- * Example Client Code
  , loggingExampleClient
  , logPredicatesExampleClient
  )
where

import           Control.Eff.Log
import           Control.Eff.LogWriter.Async
import           Control.Eff.LogWriter.Console
import           Control.Eff.LogWriter.DebugTrace
import           Control.Eff.LogWriter.Capture
import           Control.Eff.LogWriter.IO
import           Control.Eff.LogWriter.UnixSocket
import           Control.Eff.LogWriter.UDP
import           Control.Eff
import           Control.Lens                   ( view
                                                , (%~)
                                                , to
                                                )
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           GHC.Stack

-- * Logging examples

-- | Example code for:
--
--  * 'withConsoleLogging'
--  * 'mkLogWriterIO'
-- See 'loggingExampleClient'
exampleLogging :: HasCallStack => IO ()
exampleLogging = runLift
  $ withConsoleLogging "my-app" local7 allLogMessages loggingExampleClient

-- | Example code for:
--
--  * 'withLogging'
--  * 'consoleLogWriter'
exampleWithLogging :: HasCallStack => IO ()
exampleWithLogging =
  runLift $ withLogging consoleLogWriter $ logDebug "Oh, hi there"

-- | Example code for:
--
--  * 'withSomeLogging'
--  * 'logDebug'
exampleWithSomeLogging :: HasCallStack => ()
exampleWithSomeLogging =
  run $ withSomeLogging @Logs $ logDebug "Oh, hi there"

-- | Example code for:
--
--  * 'setLogPredicate'
--  * 'modifyLogPredicate'
--  * 'lmMessageStartsWith'
--  * 'lmSeverityIs'
--  * 'lmSeverityIsAtLeast'
--  * 'includeLogMessages'
--  * 'excludeLogMessages'
exampleLogPredicate :: HasCallStack => IO Int
exampleLogPredicate =
  runLift
    $ withSomeLogging @(Lift IO)
    $ setLogWriter consoleLogWriter
    $ logPredicatesExampleClient

-- | Example code for:
--
--  * 'runCaptureLogWriter'
--  * 'captureLogWriter'
--  * 'mappingLogWriter'
--  * 'filteringLogWriter'
exampleLogCapture :: IO ()
exampleLogCapture = go >>= T.putStrLn
 where
  go =
    fmap (T.unlines . Prelude.map renderLogMessageConsoleLog . snd)
      $ runLift
      $ runCaptureLogWriter
      $ withLogging captureLogWriter
      $ addLogWriter
          (mappingLogWriter (lmMessage %~ ("CAPTURED " <>)) captureLogWriter)
      $ addLogWriter
          (filteringLogWriter
            severeMessages
            (mappingLogWriter (lmMessage %~ ("TRACED " <>))
                              (debugTraceLogWriter renderRFC5424)
            )
          )
      $ do
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
  runLift $ withLogging consoleLogWriter $ withAsyncLogWriter (1000 :: Int) $ do
    logInfo "test 1"
    logInfo "test 2"
    logInfo "test 3"


-- | Example code for RFC5424 formatted logs.
exampleRFC5424Logging :: IO Int
exampleRFC5424Logging =
  runLift
    $ withSomeLogging @(Lift IO)
    $ setLogWriter
        (defaultIoLogWriter "myapp" local2 (debugTraceLogWriter renderRFC5424))
    $ logPredicatesExampleClient

-- | Example code for RFC3164 with RFC5424 time stamp formatted logs.
exampleRFC3164WithRFC5424TimestampsLogging :: IO Int
exampleRFC3164WithRFC5424TimestampsLogging =
  runLift
    $ withSomeLogging @(Lift IO)
    $ setLogWriter
        (defaultIoLogWriter "myapp" local2 (debugTraceLogWriter renderRFC3164WithRFC5424Timestamps))
    $ logPredicatesExampleClient

-- | Example code logging via a unix domain socket to @/dev/log@.
exampleDevLogSyslogLogging :: IO Int
exampleDevLogSyslogLogging =
  runLift
    $ withUnixSocketLogging renderLogMessageSyslog "/dev/log" "myapp" local2 allLogMessages
      logPredicatesExampleClient


-- | Example code logging via a unix domain socket to @/dev/log@.
exampleDevLogRFC5424Logging :: IO Int
exampleDevLogRFC5424Logging =
  runLift
    $ withUnixSocketLogging renderRFC5424 "/dev/log" "myapp" local2 allLogMessages
      logPredicatesExampleClient


-- | Example code logging RFC5424 via UDP port 514 on localhost.
exampleUdpRFC5424Logging :: IO Int
exampleUdpRFC5424Logging =
  runLift
    $ withUDPLogging renderRFC5424 "localhost" "514"  "myapp" local2 allLogMessages
      logPredicatesExampleClient

-- | Example code logging RFC5424 via UDP port 514 on localhost.
exampleUdpRFC3164Logging :: IO Int
exampleUdpRFC3164Logging =
  runLift
    $ withUDPLogging renderRFC3164 "localhost" "514"  "myapp" local1 allLogMessages
      logPredicatesExampleClient

-- | Example logging client code
--
--  * 'addLogWriter'
--  * 'debugTraceLogWriter'
--  * 'setLogPredicate'
--  * 'prefixLogMessagesWith'
--  * 'renderRFC3164'
--  * 'logMsg'
--  * 'logDebug'
--  * 'logError'
--  * 'logInfo'
--  * 'logWarning'
--  * 'logCritical'
--  * 'lmMessage'
loggingExampleClient :: (HasCallStack, Monad (LogWriterM h), LogsTo h e) => Eff e ()
loggingExampleClient = do
  logDebug "test 1.1"
  logError "test 1.2"
  censorLogs (prefixLogMessagesWith "NESTED: ") $ do
    addLogWriter (debugTraceLogWriter renderRFC3164)
      $ setLogPredicate (\m -> view lmMessage m /= "not logged")
      $ do
          logInfo "not logged"
          logMsg "test 2.1"
    logWarning "test 2.2"
  logCritical "test 1.3"


-- | Example logging client code using many 'LogPredicate's.
--
--  * 'setLogPredicate'
--  * 'modifyLogPredicate'
--  * 'lmMessageStartsWith'
--  * 'lmSeverityIs'
--  * 'lmSeverityIsAtLeast'
--  * 'includeLogMessages'
--  * 'excludeLogMessages'
logPredicatesExampleClient :: (HasCallStack, Monad (LogWriterM h), LogsTo h e) => Eff e Int
logPredicatesExampleClient = do
  logInfo "test"
  setLogPredicate
    (lmMessageStartsWith "OMG")
    (do
      logInfo "this message will not be logged"
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
      return 42
    )
