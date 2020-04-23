-- | Examples for FilteredLogging.
module Control.Eff.Log.Examples
  ( -- * Example Code for FilteredLogging
    exampleLogging,
    exampleWithLogging,
    exampleWithSomeLogging,
    exampleSetLogWriter,
    exampleLogTrace,
    exampleAsyncLogging,
    exampleRFC5424Logging,
    exampleRFC3164WithRFC5424TimestampsLogging,
    exampleDevLogSyslogLogging,
    exampleDevLogRFC5424Logging,
    exampleUdpRFC5424Logging,
    exampleUdpRFC3164Logging,

    -- * Example Client Code
    loggingExampleClient,
    logPredicatesExampleClient,
  )
where

import Control.Eff
import Control.Eff.Log
import Control.Eff.LogWriter.Async
import Control.Eff.LogWriter.Console
import Control.Eff.LogWriter.DebugTrace
import Control.Eff.LogWriter.Rich
import Control.Eff.LogWriter.UDP
import Control.Eff.LogWriter.UnixSocket
import Control.Lens
  ( (%~),
    to,
    view,
  )
import GHC.Stack

-- * FilteredLogging examples

-- | Example code for:
--
--  * 'withConsoleLogging'
--  * 'MkLogWriter'
-- See 'loggingExampleClient'
exampleLogging :: HasCallStack => IO ()
exampleLogging =
  runLift $
    withConsoleLogging "my-app" local7 allLogEvents loggingExampleClient

-- | Example code for:
--
--  * 'withLogging'
--  * 'consoleLogWriter'
exampleWithLogging :: HasCallStack => IO ()
exampleWithLogging = do
  lw <- consoleLogWriter
  runLift $ withLogging lw $ logDebug "Oh, hi there"

-- | Example code for:
--
--  * 'withoutLogging'
--  * 'logDebug'
exampleWithSomeLogging :: HasCallStack => ()
exampleWithSomeLogging =
  run $ withoutLogging $ logDebug "Oh, hi there"

-- | Example code for: 'setLogWriter'
--
-- Also used:
--  * 'stdoutLogWriter'
--  * 'renderConsoleMinimalisticWide'
--  * 'consoleLogWriter'
--  * 'logAlert'
--  * 'withLogging'
exampleSetLogWriter :: HasCallStack => IO ()
exampleSetLogWriter = do
  lw1 <- stdoutLogWriter renderConsoleMinimalisticWide
  lw2 <- consoleLogWriter
  runLift
    $ withLogging lw1
    $ do
      logAlert "test with log writer 1"
      setLogWriter lw2 (logAlert "test with log writer 2")
      logAlert "test with log writer 1 again"

-- | Example code for:
--
--  * 'runCaptureLogWriter'
--  * 'captureLogWriter'
--  * 'mappingLogWriter'
--  * 'filteringLogWriter'
exampleLogTrace :: IO ()
exampleLogTrace = do
  lw <- consoleLogWriter
  runLift
    $ withRichLogging lw "test-app" local7 allLogEvents
    $ addLogWriter
      ( filteringLogWriter
          severeMessages
          ( mappingLogWriter
              (logEventMessage %~ (packLogMsg "TRACED " <>))
              (debugTraceLogWriter renderRFC5424)
          )
      )
    $ do
      logEmergency $ MSG "emergencySeverity 1"
      logCritical $ MSG "criticalSeverity 2"
      logAlert $ MSG "alertSeverity 3"
      logError $ MSG "errorSeverity 4"
      logWarning $ MSG "warningSeverity 5"
      logInfo $ MSG "informationalSeverity 6"
      logDebug $ MSG "debugSeverity 7"
  where
    severeMessages = view (logEventSeverity . to (<= errorSeverity))

newtype TestLogMsg = TestMSG String

instance ToLogMsg TestLogMsg where
  toLogMsg (TestMSG x) = packLogMsg x

-- | Example code for:
--
--  * 'withAsyncLogging'
exampleAsyncLogging :: IO ()
exampleAsyncLogging = do
  lw <- stdoutLogWriter renderConsoleMinimalisticWide
  runLift $ withLogging lw $ withAsyncLogWriter (1000 :: Int) $ do
    logInfo $ MSG "test 1"
    logInfo $ TestMSG "test 2"
    logInfo $ MSG "test 3"

-- | Example code for RFC5424 formatted logs.
exampleRFC5424Logging :: IO Int
exampleRFC5424Logging =
  runLift
    $ withoutLogging
    $ setLogWriter
      (richLogWriter "myapp" local2 (debugTraceLogWriter renderRFC5424))
      logPredicatesExampleClient

-- | Example code for RFC3164 with RFC5424 time stamp formatted logs.
exampleRFC3164WithRFC5424TimestampsLogging :: IO Int
exampleRFC3164WithRFC5424TimestampsLogging =
  runLift
    $ withoutLogging
    $ setLogWriter
      (richLogWriter "myapp" local2 (debugTraceLogWriter renderRFC3164WithRFC5424Timestamps))
      logPredicatesExampleClient

-- | Example code logging via a unix domain socket to @/dev/log@.
exampleDevLogSyslogLogging :: IO Int
exampleDevLogSyslogLogging =
  runLift $
    withUnixSocketLogging
      renderLogEventSyslog
      "/dev/log"
      "myapp"
      local2
      allLogEvents
      logPredicatesExampleClient

-- | Example code logging via a unix domain socket to @/dev/log@.
exampleDevLogRFC5424Logging :: IO Int
exampleDevLogRFC5424Logging =
  runLift $
    withUnixSocketLogging
      renderRFC5424
      "/dev/log"
      "myapp"
      local2
      allLogEvents
      logPredicatesExampleClient

-- | Example code logging RFC5424 via UDP port 514 on localhost.
exampleUdpRFC5424Logging :: IO Int
exampleUdpRFC5424Logging =
  runLift $
    withUDPLogging
      renderRFC5424
      "localhost"
      "514"
      "myapp"
      local2
      allLogEvents
      logPredicatesExampleClient

-- | Example code logging RFC5424 via UDP port 514 on localhost.
exampleUdpRFC3164Logging :: IO Int
exampleUdpRFC3164Logging =
  runLift $
    withUDPLogging
      renderRFC3164
      "localhost"
      "514"
      "myapp"
      local1
      allLogEvents
      logPredicatesExampleClient

-- | Example logging client code
--
--  * 'addLogWriter'
--  * 'debugTraceLogWriter'
--  * 'setLogPredicate'
--  * 'prefixLogEventsWith'
--  * 'renderRFC3164'
--  * 'sendLogEvent'
--  * 'logDebug'
--  * 'logError'
--  * 'logInfo'
--  * 'logWarning'
--  * 'logCritical'
--  * 'logEventMessage'
loggingExampleClient :: (HasCallStack, IoLogging e) => Eff e ()
loggingExampleClient = do
  logDebug (packLogMsg "test 1.1")
  logError (MSG "test 1.2")
  censorLogs (prefixLogEventsWith ("NESTED: " :: String)) $ do
    addLogWriter (debugTraceLogWriter renderRFC3164)
      $ setLogPredicate (\m -> view logEventMessage m /= packLogMsg "not logged")
      $ do
        logInfo (MSG "not logged")
        sendLogEvent (infoMessage (MSG "test 2.1"))
    logWarning (MSG "test 2.2")
  logCritical (MSG "test 1.3")

-- | Example logging client code using many 'LogPredicate's.
--
--  * 'setLogPredicate'
--  * 'modifyLogPredicate'
--  * 'logEventMessageStartsWith'
--  * 'logEventSeverityIs'
--  * 'logEventSeverityIsAtLeast'
--  * 'whitelistLogEvents'
--  * 'blacklistLogEvents'
logPredicatesExampleClient :: (HasCallStack, IoLogging e) => Eff e Int
logPredicatesExampleClient = do
  logInfo "test"
  setLogPredicate
    (logEventMessageStartsWith "OMG")
    ( do
        logInfo "this message will not be logged"
        logInfo "OMG logged"
        modifyLogPredicate (\p lm -> p lm || logEventSeverityIs errorSeverity lm) $ do
          logDebug "OMG logged"
          logInfo "Not logged"
          logError "Logged"
          logEmergency "Not Logged"
          whitelistLogEvents (logEventSeverityIsAtLeast warningSeverity) $ do
            logInfo "Not logged"
            logError "Logged"
            logEmergency "Logged"
            logWarning "Logged"
            logDebug "OMG still Logged"
            blacklistLogEvents (logEventMessageStartsWith "OMG") $ do
              logDebug "OMG NOT Logged"
              logError "OMG ALSO NOT Logged"
              logEmergency "Still Logged"
              logWarning "Still Logged"
            logWarning "Logged"
            logDebug "OMG still Logged"
        return 42
    )
