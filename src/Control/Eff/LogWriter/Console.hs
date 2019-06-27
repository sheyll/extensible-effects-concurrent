-- | This helps to setup logging to /standard ouput/.
module Control.Eff.LogWriter.Console
  ( withConsoleLogWriter
  , withConsoleLogging
  , consoleLogWriter
  , stdoutLogWriter
  ) where

import Control.Eff as Eff
import Control.Eff.Log
import Control.Eff.LogWriter.IO
import Data.Text
import qualified Data.Text.IO                  as T

-- | Enable logging to @standard output@ using the 'consoleLogWriter', with some 'LogMessage' fields preset
-- as in 'withIoLogging'.
--
-- Log messages are rendered using 'renderLogMessageConsoleLog'.
--
-- Example:
--
-- > exampleWithConsoleLogging :: IO ()
-- > exampleWithConsoleLogging =
-- >     runLift
-- >   $ withConsoleLogging "my-app" local7 allLogMessages
-- >   $ logInfo "Oh, hi there"
--
-- To vary the 'LogWriter' use 'withIoLogging'.
withConsoleLogging
  :: Lifted IO e
  => Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader (Lift IO) : e) a
  -> Eff e a
withConsoleLogging = withIoLogging consoleLogWriter


-- | Enable logging to @standard output@ using the 'consoleLogWriter'.
--
-- Log messages are rendered using 'renderLogMessageConsoleLog'.
--
-- Example:
--
-- > exampleWithConsoleLogWriter :: IO ()
-- > exampleWithConsoleLogWriter =
-- >     runLift
-- >   $ withSomeLogging @IO
-- >   $ withConsoleLogWriter
-- >   $ logInfo "Oh, hi there"
withConsoleLogWriter
  :: (LogIo e)
  => Eff e a -> Eff e a
withConsoleLogWriter = addLogWriter consoleLogWriter

-- | Write 'LogMessage's to standard output, formatted with 'printLogMessage'.
--
-- It uses 'stdoutLogWriter' with 'renderLogMessageConsoleLog'.
consoleLogWriter :: LogWriter (Lift IO)
consoleLogWriter = mkLogWriterIO (T.putStrLn . renderLogMessageConsoleLog)

-- | A 'LogWriter' that uses a 'LogMessageRenderer' to render, and 'T.putStrLn' to print it.
stdoutLogWriter :: LogMessageRenderer Text -> LogWriter (Lift IO)
stdoutLogWriter render = mkLogWriterIO (T.putStrLn . render)
