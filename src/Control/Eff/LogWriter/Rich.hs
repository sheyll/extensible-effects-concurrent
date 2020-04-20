-- | Enrich 'LogEvent's with timestamps and OS dependent information.
module Control.Eff.LogWriter.Rich
  ( richLogWriter
  , withRichLogging
  , stdoutLogWriter
  )
where

import           Control.Eff                   as Eff
import           Control.Eff.Log
import           Control.Monad                  ( (>=>) )
import           Control.Lens                   (set)
import           Data.Text

-- | Enable logging to IO using the 'richLogWriter'.
--
-- Example:
--
-- > exampleWithIoLogging :: IO ()
-- > exampleWithIoLogging =
-- >     runLift
-- >   $ withRichLogging debugTraceLogWriter
-- >                   "my-app"
-- >                   local7
-- >                   (logEventSeverityIsAtLeast informationalSeverity)
-- >   $ logInfo "Oh, hi there"
--
withRichLogging
  :: Lifted IO e
  => LogWriter -- ^ The 'LogWriter' that will be used to write log messages.
  -> String -- ^ The default application name to put into the 'logEventAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'logEventFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader : e) a
  -> Eff e a
withRichLogging lw appName facility defaultPredicate =
  withLogging (richLogWriter appName facility lw)
    . setLogPredicate defaultPredicate

-- | Decorate an IO based 'LogWriter' to fill out these fields in 'LogEvent's:
--
-- * The messages will carry the given application name in the 'logEventAppName' field.
-- * The 'logEventTimestamp' field contains the UTC time of the log event
-- * The 'logEventHostname' field contains the FQDN of the current host
-- * The 'logEventFacility' field contains the given 'Facility'
--
-- It works by using 'mappingLogWriterIO'.
richLogWriter
  :: String -- ^ The default application name to put into the 'logEventAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'logEventFacility' field.
  -> LogWriter -- ^ The IO based writer to decorate
  -> LogWriter
richLogWriter appName facility =
  mappingLogWriterIO
    (   setLogEventsTimestamp
    >=> setLogEventsHostname
    >=> setLogEventsThreadId
    )
  . mappingLogWriter
    ( set logEventFacility facility
    . set logEventAppName  (Just (pack appName))
    )
