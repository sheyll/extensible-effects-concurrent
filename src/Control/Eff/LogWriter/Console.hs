-- | This helps to setup logging to /standard ouput/.
module Control.Eff.LogWriter.Console
  ( withConsoleLogWriter
  , withConsoleLogging
  ) where

import Control.Eff as Eff
import Control.Eff.Log
import Data.Text

-- | Enable logging to @standard output@ using the 'consoleLogWriter', with some 'LogMessage' fields preset
-- as in 'withIoLogging'.
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
  -> Eff (Logs : LogWriterReader IO : e) a
  -> Eff e a
withConsoleLogging = withIoLogging consoleLogWriter


-- | Enable logging to @standard output@ using the 'consoleLogWriter'.
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
  :: (LogsTo IO e, Lifted IO e)
  => Eff e a -> Eff e a
withConsoleLogWriter = addLogWriter consoleLogWriter
