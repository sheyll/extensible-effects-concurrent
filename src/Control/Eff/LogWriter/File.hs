-- | This helps to setup logging to a /file/.
module Control.Eff.LogWriter.File
  ( withFileLogging
  , withFileLogWriter
  )
where

import           Control.Eff                   as Eff
import           Control.Eff.Log
import           Control.Eff.LogWriter.Rich
import           GHC.Stack
import           Data.Text                     as T
import qualified System.IO                     as IO
import           System.Directory               ( canonicalizePath
                                                , createDirectoryIfMissing
                                                )
import           System.FilePath                ( takeDirectory )
import qualified Control.Exception.Safe        as Safe
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Trans.Control    ( MonadBaseControl
                                                , liftBaseOp
                                                )

-- | Enable logging to a file, with some 'LogEvent' fields preset
-- as described in 'withRichLogging'.
--
-- If the file or its directory does not exist, it will be created.
--
-- Example:
--
-- > exampleWithFileLogging :: IO ()
-- > exampleWithFileLogging =
-- >     runLift
-- >   $ withFileLogging "/var/log/my-app.log" "my-app" local7 allLogEvents renderLogEventConsoleLog
-- >   $ logInfo "Oh, hi there"
--
-- To vary the 'LogWriter' use 'withRichLogging'.
withFileLogging
  :: (Lifted IO e, MonadBaseControl IO (Eff e), HasCallStack)
  => FilePath -- ^ Path to the log-file.
  -> Text -- ^ The default application name to put into the 'logEventAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'logEventFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> LogEventPrinter -- ^ The 'LogEvent' render function
  -> Eff (Logs : LogWriterReader : e) a
  -> Eff e a
withFileLogging fnIn a f p render e = do
  liftBaseOp (withOpenedLogFile fnIn render) (\lw -> withRichLogging lw a f p e)


-- | Enable logging to a file.
--
-- If the file or its directory does not exist, it will be created.
-- Example:
--
-- > exampleWithFileLogWriter :: IO ()
-- > exampleWithFileLogWriter =
-- >     runLift
-- >   $ withoutLogging
-- >   $ withFileLogWriter "test.log" renderLogEventConsoleLog
-- >   $ logInfo "Oh, hi there"
withFileLogWriter
  :: (IoLogging e, MonadBaseControl IO (Eff e), HasCallStack)
  => FilePath -- ^ Path to the log-file.
  -> LogEventPrinter
  -> Eff e b
  -> Eff e b
withFileLogWriter fnIn render e =
  liftBaseOp (withOpenedLogFile fnIn render) (`addLogWriter` e)

withOpenedLogFile
  :: HasCallStack
  => FilePath
  -> LogEventPrinter
  -> (LogWriter -> IO a)
  -> IO a
withOpenedLogFile fnIn render ioE = Safe.bracket
  (do
    fnCanon <- canonicalizePath fnIn
    createDirectoryIfMissing True (takeDirectory fnCanon)
    h <- IO.openFile fnCanon IO.AppendMode
    IO.hSetBuffering h IO.LineBuffering
    return h
  )
  (\h -> Safe.try @IO @Catch.SomeException (IO.hFlush h) >> IO.hClose h)
  (\h -> ioE (ioHandleLogWriter h render))
