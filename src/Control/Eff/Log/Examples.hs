-- | Examples for FilteredLogging.
module Control.Eff.Log.Examples
  ( -- * Example Code for FilteredLogging
    exampleLogging
  , exampleWithLogging
  , exampleWithSomeLogging
  , exampleSetLogWriter
  , exampleLogTrace
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
import           Control.Eff.LogWriter.Rich
import           Control.Eff.LogWriter.UnixSocket
import           Control.Eff.LogWriter.UDP
import           Control.Eff
import           Control.Lens                   ( view
                                                , (%~)
                                                , to
                                                )
import           GHC.Stack

-- * FilteredLogging examples

-- | Example code for:
--
--  * 'withConsoleLogging'
--  * 'MkLogWriter'
-- See 'loggingExampleClient'
exampleLogging :: HasCallStack => IO ()
exampleLogging = runLift
  $ withConsoleLogging "my-app" local7 allLogMessages loggingExampleClient

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
    $ do  logAlert "test with log writer 1"
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
    $ withRichLogging lw "test-app" local7 allLogMessages
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
  where
    severeMessages = view (lmSeverity . to (<= errorSeverity))


-- | Example code for:
--
--  * 'withAsyncLogging'
exampleAsyncLogging :: IO ()
exampleAsyncLogging = do
  lw <- stdoutLogWriter renderConsoleMinimalisticWide
  runLift $ withLogging lw $ withAsyncLogWriter (1000 :: Int) $ do
    logInfo "test 1"
    logInfo "test 2"
    logInfo "test 3"


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
--  * 'prefixLogEventsWith'
--  * 'renderRFC3164'
--  * 'logMsg'
--  * 'logDebug'
--  * 'logError'
--  * 'logInfo'
--  * 'logWarning'
--  * 'logCritical'
--  * 'lmMessage'
loggingExampleClient :: (HasCallStack, IoLogging e) => Eff e ()
loggingExampleClient = do
  logDebug "test 1.1"
  logError "test 1.2"
  censorLogs (prefixLogEventsWith "NESTED: ") $ do
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
--  * 'whitelistLogEvents'
--  * 'blacklistLogEvents'
logPredicatesExampleClient :: (HasCallStack, IoLogging e) => Eff e Int
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
        whitelistLogEvents (lmSeverityIsAtLeast warningSeverity) $ do
          logInfo "Not logged"
          logError "Logged"
          logEmergency "Logged"
          logWarning "Logged"
          logDebug "OMG still Logged"
          blacklistLogEvents (lmMessageStartsWith "OMG") $ do
            logDebug "OMG NOT Logged"
            logError "OMG ALSO NOT Logged"
            logEmergency "Still Logged"
            logWarning "Still Logged"
          logWarning "Logged"
          logDebug "OMG still Logged"
      return 42
    )
