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
  ( -- * Module Re-Exports
    -- | This module contains the API for __sending__ log messages and for
    -- handling the messages in the frame work of extensible effects.
    --
    -- It also defines the reader effect to access 'LogWriter's
    module Control.Eff.Log.Handler
    -- | The module that contains the 'LogMessage' and 'LogPredicate' definitions.
    --
    -- The log message type corresponds to RFC-5424, including structured data.
  , module Control.Eff.Log.Message
    -- | This module only exposes a 'LogWriter' for asynchronous logging;
  , module Control.Eff.Log.Channel
    -- | This module defines the 'LogWriter' type, which is used to give
    -- callback functions for log messages an explicit type.
  , module Control.Eff.Log.Writer
  -- * Example Code for Logging
  , exampleLogging
  , exampleWithLogging
  , exampleWithSomeLogging
  , exampleLogPredicate
  , exampleLogCapture
  , exampleAsyncLogging
  )
where

import           Control.Eff.Log.Channel
import           Control.Eff.Log.Handler
import           Control.Eff.Log.Message
import           Control.Eff.Log.Writer
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
  $ withSomeLogging @IO
  $ withAsyncLogging (1000::Int) consoleLogWriter
  $ do logInfo "test 1"
       logInfo "test 2"
       logInfo "test 3"
