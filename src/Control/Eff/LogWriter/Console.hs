-- | This helps to setup logging to /standard ouput/.
module Control.Eff.LogWriter.Console
  ( withConsoleLogWriter
  , withConsoleLogging
  , consoleLogWriter
  ) where

import Control.Eff as Eff
import Control.Eff.Log
import Control.Eff.LogWriter.Rich
import Data.Text

-- | Enable logging to @standard output@ using the 'consoleLogWriter', with some 'LogMessage' fields preset
-- as in 'withRichLogging'.
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
-- To vary the 'LogWriter' use 'withRichLogging'.
withConsoleLogging
  :: Lifted IO e
  => Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader : e) a
  -> Eff e a
withConsoleLogging a b c d = do
  lw <- lift consoleLogWriter
  withRichLogging lw a b c d


-- | Enable logging to @standard output@ using the 'consoleLogWriter'.
--
-- Log messages are rendered using 'renderLogMessageConsoleLog'.
--
-- Example:
--
-- > exampleWithConsoleLogWriter :: IO ()
-- > exampleWithConsoleLogWriter =
-- >     runLift
-- >   $ withoutLogging @IO
-- >   $ withConsoleLogWriter
-- >   $ logInfo "Oh, hi there"
withConsoleLogWriter
  :: (IoLogging e)
  => Eff e a -> Eff e a
withConsoleLogWriter e = do
  lw <- lift consoleLogWriter
  addLogWriter lw e


-- | Render a 'LogMessage' to 'IO.stdout' using 'renderLogMessageConsoleLog'.
--
-- See 'stdoutLogWriter'.
--
-- @since 0.31.0
consoleLogWriter :: IO LogWriter
consoleLogWriter = stdoutLogWriter renderLogMessageConsoleLog
