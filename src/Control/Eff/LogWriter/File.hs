-- | This helps to setup logging to a /file/.
module Control.Eff.LogWriter.File
  ( withFileLogging
  , withFileLogWriter
  ) where

import Control.Eff as Eff
import Control.Eff.Log
import GHC.Stack
import           Data.Text                     as T
import qualified System.IO                     as IO
import           System.Directory               ( canonicalizePath
                                                , createDirectoryIfMissing
                                                )
import           System.FilePath                ( takeDirectory )
import qualified Control.Exception.Safe        as Safe
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Trans.Control     ( MonadBaseControl
                                                   , liftBaseOp
                                                 )

-- | Enable logging to a file, with some 'LogMessage' fields preset
-- as described in 'withIoLogging'.
--
-- If the file or its directory does not exist, it will be created.
--
-- Example:
--
-- > exampleWithFileLogging :: IO ()
-- > exampleWithFileLogging =
-- >     runLift
-- >   $ withFileLogging "/var/log/my-app.log" "my-app" local7 allLogMessages
-- >   $ logInfo "Oh, hi there"
--
-- To vary the 'LogWriter' use 'withIoLogging'.
withFileLogging
  :: ( Lifted IO e
     , MonadBaseControl IO (Eff e)
     )
  => FilePath -- ^ Path to the log-file.
  -> Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader IO : e) a
  -> Eff e a
withFileLogging fnIn a f p e =
  liftBaseOp (withOpenedLogFile fnIn)
    (\lw -> withIoLogging lw a f p e)


-- | Enable logging to a file.
--
-- If the file or its directory does not exist, it will be created.
-- Example:
--
-- > exampleWithFileLogWriter :: IO ()
-- > exampleWithFileLogWriter =
-- >     runLift
-- >   $ withSomeLogging @IO
-- >   $ withFileLogWriter "test.log"
-- >   $ logInfo "Oh, hi there"
withFileLogWriter
  :: ( Lifted IO e
     , LogsTo IO e
     , MonadBaseControl IO (Eff e)
     )
  => FilePath -- ^ Path to the log-file.
  -> Eff e b
  -> Eff e b
withFileLogWriter fnIn e = liftBaseOp (withOpenedLogFile fnIn) (`addLogWriter` e)

withOpenedLogFile
  :: HasCallStack
  => FilePath
  -> (LogWriter IO -> IO a)
  -> IO a
withOpenedLogFile fnIn ioE =
  Safe.bracket
      (do
        fnCanon <- canonicalizePath fnIn
        createDirectoryIfMissing True (takeDirectory fnCanon)
        h <- IO.openFile fnCanon IO.AppendMode
        IO.hSetBuffering h (IO.BlockBuffering (Just 1024))
        return h
      )
      (\h -> Safe.try @IO @Catch.SomeException (IO.hFlush h) >> IO.hClose h)
      (\h -> ioE (ioHandleLogWriter h))
