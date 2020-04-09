-- | FilteredLogging via @extensible-effects@
--
-- FilteredLogging consist of __two__ effects:
--
-- * __Receiving__ log messages sent by the code using e.g. 'logInfo'; this also include deep evaluation and
--    dropping messages not satisfying the current 'LogPredicate'.
--
-- * __Writing__ log message to disk, network, ... etc; this also includes rendering log messages and setting
--     fields like the hostname, timestamp, etc
--
-- == Example
--
--
-- > exampleLogging :: IO ()
-- > exampleLogging =
-- >     runLift
-- >   $ withLogging consoleLogWriter
-- >   $ do
-- >       logDebug "test 1.1"
-- >       logError "test 1.2"
-- >       censorLogs (prefixLogEventsWith "NESTED: ")
-- >        $ do
-- >             addLogWriter debugTraceLogWriter
-- >              $ setLogPredicate (\m -> (view lmMessage m) /= "not logged")
-- >              $ do
-- >                   logInfo "not logged"
-- >                   logMsg "test 2.1"
-- >             logWarning "test 2.2"
-- >       logCritical "test 1.3"
--
-- == Log Message Data Type
--
-- A singular /logging event/  is contained in a __'LogEvent's__ value.
--
-- The 'LogEvent' is modelled along RFC-5424.
--
-- There is the 'ToLogEntry' class for converting to 'LogEvent'.
--  /Although the author is not clear on how to pursue the approach./
--
-- == Receiving and Filtering
--
-- 'LogEvent's are sent using 'logMsg' and friends, see "Control.Eff.Log#SendingLogs"
--
-- === Log Message Predicates
--
-- There is a single global 'LogPredicate' that can be used to suppress logs before
-- they are passed to any 'LogWriter'.
--
-- This is done by the 'logMsg' function.
--
-- Also, 'LogEvent's are evaluated using 'Control.DeepSeq.deepseq', __after__ they pass the 'LogPredicate',
-- also inside 'logMsg'.
--
-- See "Control.Eff.Log#LogPredicate"
--
-- == Writing and Rendering
--
-- Writing is done through a 'LogWriter'; the current 'LogWriter' value to use is held by the
-- 'LogWriterReader' effect.
--
-- === Log Message Rendering
--
-- Message are rendered by 'LogMessageRenderer's found in the "Control.Eff.Log.MessageRenderer".
--
-- === 'LogWriter's
--
-- * FilteredLogging in a 'Control.Concurrent.Async.withAsync' spawned thread is done using 'withAsyncLogging'.

module Control.Eff.Log
  ( -- * FilteredLogging API
    -- ** Sending Log Messages #SendingLogs#
    logMsg
  , logWithSeverity
  , logWithSeverity'
  , logEmergency
  , logEmergency'
  , logAlert
  , logAlert'
  , logCritical
  , logCritical'
  , logError
  , logError'
  , logWarning
  , logWarning'
  , logNotice
  , logNotice'
  , logInfo
  , logInfo'
  , logDebug
  , logDebug'
  , logCallStack
  , logMultiLine
  , logMultiLine'

    -- ** Log Message Pre-Filtering #LogPredicate#
    -- $LogPredicate
  , whitelistLogEvents
  , blacklistLogEvents
  , setLogPredicate
  , modifyLogPredicate
  , askLogPredicate

    -- * Log Handling API

    -- ** Writing Logs
  , setLogWriter
  , addLogWriter
  , modifyLogWriter

    -- *** Log Message Modification
  , censorLogs
  , censorLogsIo

    -- ** 'Logs' Effect Handling
  , Logs()
  , FilteredLogging
  , IoLogging
  , LoggingAndIo
  , withLogging
  , withoutLogging

    -- ** Low-Level API for Custom Extensions
    -- *** Log Message Interception
  , runLogs
  , respondToLogEvent
  , interceptLogMessages

    -- * Module Re-Exports
    -- | The module that contains the 'LogEvent' and 'LogPredicate' definitions.
    --
    -- The log message type corresponds roughly to RFC-5424, including structured data.
  , module Control.Eff.Log.Message
    -- | Rendering functions for 'LogEvent's
    --
    -- The functions have been seperated from "Control.Eff.Log.Message"
  , module Control.Eff.Log.MessageRenderer

    -- | This module defines the 'LogWriter' type, which is used to give
    -- callback functions for log messages an explicit type.
  , module Control.Eff.Log.Writer
  )
where

import           Control.Eff.Log.Handler
import           Control.Eff.Log.Message
import           Control.Eff.Log.MessageRenderer
import           Control.Eff.Log.Writer

-- $LogPredicate
--
-- Ways to change the 'LogPredicate' are:
--
--  * 'setLogPredicate'.
--  * 'modifyLogPredicate'.
--  * 'whitelistLogEvents'
--  * 'blacklistLogEvents'
--
-- The current predicate is retrieved via 'askLogPredicate'.
--
-- Some pre-defined 'LogPredicate's can be found here: "Control.Eff.Log.Message#PredefinedPredicates"
