-- | This helps to setup logging via "Debug.Trace".
module Control.Eff.LogWriter.DebugTrace
  ( withTraceLogWriter
  , withTraceLogging
  , debugTraceLogWriter
  )
where

import           Debug.Trace
import           Control.Eff                   as Eff
import           Control.Eff.Log
import           Control.Eff.LogWriter.IO
import           Data.Text                     as T

-- | Enable logging via 'traceM' using the 'debugTraceLogWriter', with some 'LogMessage' fields preset
-- as in 'withIoLogging'.
--
-- Log messages are rendered using 'renderLogMessageConsoleLog'.
--
-- Example:
--
-- > exampleWithTraceLogging :: IO ()
-- > exampleWithTraceLogging =
-- >     runLift
-- >   $ withTraceLogging "my-app" local7 allLogMessages
-- >   $ logInfo "Oh, hi there"
withTraceLogging
  :: Lifted IO e
  => Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader IO : e) a
  -> Eff e a
withTraceLogging = withIoLogging (debugTraceLogWriter renderLogMessageConsoleLog)


-- | Enable logging via 'traceM' using the 'debugTraceLogWriter'. The
-- logging monad type can be /any/ type with a 'Monad' instance.
--
-- Log messages are rendered using 'renderLogMessageConsoleLog'.
--
-- Example:
--
-- > exampleWithTraceLogWriter :: IO ()
-- > exampleWithTraceLogWriter =
-- >     runLift
-- >   $ withSomeLogging @IO
-- >   $ withTraceLogWriter
-- >   $ logInfo "Oh, hi there"
withTraceLogWriter :: forall h e a . (Monad h, LogsTo h e) => Eff e a -> Eff e a
withTraceLogWriter = addLogWriter @h (debugTraceLogWriter renderLogMessageConsoleLog)

-- | Write 'LogMessage's  via 'traceM'.
debugTraceLogWriter :: forall h . (Monad h) => LogMessageRenderer Text -> LogWriter h
debugTraceLogWriter render = MkLogWriter (traceM . T.unpack . render)
