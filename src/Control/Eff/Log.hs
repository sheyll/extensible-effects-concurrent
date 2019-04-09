-- | A logging effect.
--
-- There is just one log message type: 'LogMessage' and it is written using 'logMsg' and
-- the functions built on top of it.
--
-- The 'Logs' effect is tightly coupled with the 'LogWriterReader' effect.
-- When using the 'Control.Monad.Trans.ControlMonadBaseControl' instance, the underlying monad of the 'LogWriter',
-- that is expected to be present through the respective 'LogWriterReader', is
-- constrained to be the base monad itself, e.g. 'IO'.
--
-- The log message type is fixed to 'LogMessage', and there is a type class for
-- converting to that, call 'ToLogMessage'.
--
-- There is a single global 'LogPredicate' that can be used to suppress logs directly
-- at the point where they are sent, in the 'logMsg' function.
--
-- Note that all logging is eventually done via 'logMsg'; 'logMsg' is the __only__ place where
-- log filtering should happen.
--
-- Also, 'LogMessage's are evaluated using 'Control.DeepSeq.deepseq', __after__ they pass the 'LogPredicate', also inside 'logMsg'.
--
-- Example:
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
-- == Asynchronous Logging
--
-- Logging in a 'Control.Concurrent.Async.withAsync' spawned thread is done using 'withAsyncLogging'.
--
-- == 'LogPredicate's
--
-- See "Control.Eff.Log.Handler#LogPredicate"
module Control.Eff.Log
  ( -- * Logging API
    -- ** Sending Log Messages
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
  , withIoLogging
  , withLogging
  , withSomeLogging
  , LoggingAndIo

    -- ** Log Writers
  , withConsoleLogging
  , withLogFileAppender
  , withUdp514
  , withDevLog
  , withRFC5424UnixDomainSocket
  , withRFC3164UnixDomainSocketWriter

    -- ** Low-Level API for Custom Extensions
    -- *** Log Message Interception
  , runLogs
  , respondToLogMessage
  , interceptLogMessages

    -- * Module Re-Exports
    -- | The module that contains the 'LogMessage' and 'LogPredicate' definitions.
    --
    -- The log message type corresponds to RFC-5424, including structured data.
  , module Control.Eff.Log.Message
    -- | This module only exposes a 'LogWriter' for asynchronous logging;
  , module Control.Eff.Log.Async
    -- | This module defines the 'LogWriter' type, which is used to give
    -- callback functions for log messages an explicit type.
  , module Control.Eff.Log.Writer
  )
where

import           Control.Eff.Log.Async
import           Control.Eff.Log.Handler
import           Control.Eff.Log.Message
import           Control.Eff.Log.Writer
