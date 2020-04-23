-- | This helps to setup logging via "Debug.Trace".
module Control.Eff.LogWriter.DebugTrace
  ( withTraceLogWriter,
    withTraceLogging,
    debugTraceLogWriter,
  )
where

import Control.Eff as Eff
import Control.Eff.Log
import Control.Eff.LogWriter.Rich
import Data.Text as T
import Debug.Trace

-- | Enable logging via 'traceM' using the 'debugTraceLogWriter', with some 'LogEvent' fields preset
-- as in 'withRichLogging'.
--
-- Log messages are rendered using 'renderLogEventConsoleLog'.
--
-- Example:
--
-- > exampleWithTraceLogging :: IO ()
-- > exampleWithTraceLogging =
-- >     runLift
-- >   $ withTraceLogging "my-app" local7 allLogEvents
-- >   $ logInfo "Oh, hi there"
withTraceLogging ::
  Lifted IO e =>
  -- | The default application name to put into the 'logEventAppName' field.
  String ->
  -- | The default RFC-5424 facility to put into the 'logEventFacility' field.
  Facility ->
  -- | The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  LogPredicate ->
  Eff (Logs : LogWriterReader : e) a ->
  Eff e a
withTraceLogging = withRichLogging (debugTraceLogWriter renderLogEventConsoleLog)

-- | Enable logging via 'traceM' using the 'debugTraceLogWriter'. The
-- logging monad type can be /any/ type with a 'Monad' instance.
--
-- Log messages are rendered using 'renderLogEventConsoleLog'.
--
-- Example:
--
-- > exampleWithTraceLogWriter :: IO ()
-- > exampleWithTraceLogWriter =
-- >     runLift
-- >   $ withoutLogging @IO
-- >   $ withTraceLogWriter
-- >   $ logInfo "Oh, hi there"
withTraceLogWriter :: IoLogging e => Eff e a -> Eff e a
withTraceLogWriter = addLogWriter (debugTraceLogWriter renderLogEventConsoleLog)

-- | Write 'LogEvent's  via 'traceM'.
debugTraceLogWriter :: LogEventPrinter -> LogWriter
debugTraceLogWriter render = MkLogWriter (traceM . T.unpack . render)
