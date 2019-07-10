-- | Logging via @extensible-effects@
--
-- Logging consist of __two__ effects:
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
-- >       censorLogs (prefixLogMessagesWith "NESTED: ")
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
-- A singular /logging event/  is contained in a __'LogMessage's__ value.
--
-- The 'LogMessage' is modelled along RFC-5424.
--
-- There is the 'ToLogMessage' class for converting to 'LogMessage'.
--  /Although the author is not clear on how to pursue the approach./
--
-- == Receiving and Filtering
--
-- 'LogMessage's are sent using 'logMsg' and friends, see "Control.Eff.Log#SendingLogs"
--
-- === Log Message Predicates
--
-- There is a single global 'LogPredicate' that can be used to suppress logs before
-- they are passed to any 'LogWriter'.
--
-- This is done by the 'logMsg' function.
--
-- Also, 'LogMessage's are evaluated using 'Control.DeepSeq.deepseq', __after__ they pass the 'LogPredicate',
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
-- * Logging in a 'Control.Concurrent.Async.withAsync' spawned thread is done using 'withAsyncLogging'.

module Control.Eff.Log
  ( -- * Logging API
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
  , includeLogMessages
  , excludeLogMessages
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
  , censorLogsM

    -- ** 'Logs' Effect Handling
  , Logs()
  , LogsTo
  , LogIo
  , withLogging
  , withSomeLogging

    -- ** Low-Level API for Custom Extensions
    -- *** Log Message Interception
  , runLogs
  , respondToLogMessage
  , interceptLogMessages

    -- * Module Re-Exports
    -- | The module that contains the 'LogMessage' and 'LogPredicate' definitions.
    --
    -- The log message type corresponds roughly to RFC-5424, including structured data.
  , module Control.Eff.Log.Message
    -- | Rendering functions for 'LogMessage's
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
--  * 'includeLogMessages'
--  * 'excludeLogMessages'
--
-- The current predicate is retrieved via 'askLogPredicate'.
--
-- Some pre-defined 'LogPredicate's can be found here: "Control.Eff.Log.Message#PredefinedPredicates"
