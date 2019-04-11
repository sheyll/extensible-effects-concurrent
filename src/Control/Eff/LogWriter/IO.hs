-- | Functions for generic, IO-based 'LogWriter's.
--
-- This module is more low-level than the others in this directory.
module Control.Eff.LogWriter.IO
  ( mkLogWriterIO
  , ioHandleLogWriter
  , defaultIoLogWriter
  , withIoLogging
  , LoggingAndIo
  , printLogMessage
  )
where

import           Control.Eff                   as Eff
import           Control.Eff.Log
import           Control.Monad                  ( (>=>) )
import           Control.Lens                   (set)
import           Data.Text
import qualified System.IO                     as IO
import           GHC.Stack
import qualified Data.Text.IO                  as T

-- | A 'LogWriter' that uses an 'IO' action to write the message.
--
-- This is just an alias for 'MkLogWriter' but with @IO@ as parameter. This reduces the need to
-- apply something to the extra type argument @\@IO@.
--
-- Example use cases for this function are the 'consoleLogWriter' and the 'ioHandleLogWriter'.
mkLogWriterIO :: HasCallStack => (LogMessage -> IO ()) -> LogWriter IO
mkLogWriterIO = MkLogWriter

-- | A 'LogWriter' that renders 'LogMessage's to strings via 'renderLogMessageConsoleLog'
-- and prints them to an 'IO.Handle' using 'hPutStrLn'.
ioHandleLogWriter :: HasCallStack => IO.Handle -> LogWriter IO
ioHandleLogWriter h =
  mkLogWriterIO (T.hPutStrLn h . renderLogMessageConsoleLog)

-- | Enable logging to IO using the 'defaultIoLogWriter'.
--
-- Example:
--
-- > exampleWithIoLogging :: IO ()
-- > exampleWithIoLogging =
-- >     runLift
-- >   $ withIoLogging debugTraceLogWriter
-- >                   "my-app"
-- >                   local7
-- >                   (lmSeverityIsAtLeast informationalSeverity)
-- >   $ logInfo "Oh, hi there"
--
withIoLogging
  :: SetMember Lift (Lift IO) e
  => LogWriter IO -- ^ The 'LogWriter' that will be used to write log messages.
  -> Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader IO : e) a
  -> Eff e a
withIoLogging lw appName facility defaultPredicate =
  withLogging (defaultIoLogWriter appName facility lw)
    . setLogPredicate defaultPredicate

-- | Decorate an IO based 'LogWriter' to fill out these fields in 'LogMessage's:
--
-- * The messages will carry the given application name in the 'lmAppName' field.
-- * The 'lmTimestamp' field contains the UTC time of the log event
-- * The 'lmHostname' field contains the FQDN of the current host
-- * The 'lmFacility' field contains the given 'Facility'
--
-- It works by using 'mappingLogWriterM'.
defaultIoLogWriter
  :: Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogWriter IO -- ^ The IO based writer to decorate
  -> LogWriter IO
defaultIoLogWriter appName facility = mappingLogWriterM
  (   setLogMessageTimestamp
  >=> setLogMessageHostname
  >=> pure
  .   set lmFacility facility
  .   set lmAppName  (Just appName)
  )

-- | The concrete list of 'Eff'ects for logging with an IO based 'LogWriter', and a 'LogWriterReader'.
type LoggingAndIo = '[Logs, LogWriterReader IO, Lift IO]

-- | Render a 'LogMessage' but set the timestamp and thread id fields.
printLogMessage :: LogMessage -> IO ()
printLogMessage = T.putStrLn . renderLogMessageConsoleLog
