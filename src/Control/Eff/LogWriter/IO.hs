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
import           Control.Eff.Extend
import           Control.Lens                   (set)
import           Data.Text
import qualified System.IO                     as IO
import           GHC.Stack
import qualified Data.Text.IO                  as T
import           Data.Function                  ( fix )
import           Control.Monad                  ( (>=>)
                                                , when
                                                )
import           Control.Monad.Base             ( MonadBase() )
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Trans.Control    ( MonadBaseControl
                                                  ( restoreM
                                                  , liftBaseWith
                                                  , StM
                                                  )
                                                )

-- | A 'LogWriter' that uses an 'IO' action to write the message.
--
-- This is just an alias for 'MkLogWriter' but with @IO@ as parameter. This reduces the need to
-- apply something to the extra type argument @\@IO@.
--
-- Example use cases for this function are the 'consoleLogWriter' and the 'ioHandleLogWriter'.
mkLogWriterIO :: HasCallStack => (LogMessage -> IO ()) -> LogWriter (Lift IO)
mkLogWriterIO f = MkLogWriter (IOLogWriter . f)

-- | A 'LogWriter' that renders 'LogMessage's to strings via 'renderLogMessageConsoleLog'
-- and prints them to an 'IO.Handle' using 'hPutStrLn'.
ioHandleLogWriter :: HasCallStack => IO.Handle -> LogWriter (Lift IO)
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
  :: Lifted IO e
  => LogWriter (Lift IO) -- ^ The 'LogWriter' that will be used to write log messages.
  -> Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader (Lift IO) : e) a
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
  -> LogWriter (Lift IO )-- ^ The IO based writer to decorate
  -> LogWriter (Lift IO)
defaultIoLogWriter appName facility = mappingLogWriterM
  (   (IOLogWriter . setLogMessageTimestamp)
  >=> (IOLogWriter . setLogMessageHostname)
  >=> pure
  .   set lmFacility facility
  .   set lmAppName  (Just appName)
  )

-- | The concrete list of 'Eff'ects for logging with an IO based 'LogWriter', and a 'LogWriterReader'.
type LoggingAndIo = '[Logs, LogWriterReader IoLogger, IoLogger, Lift IO]

-- | Render a 'LogMessage' but set the timestamp and thread id fields.
printLogMessage :: LogMessage -> IO ()
printLogMessage = T.putStrLn . renderLogMessageConsoleLog

data IoLogger v where
  IoLogger :: IoLogger ()

-- | Embed 'IO' actions consuming all 'LogMessage's
--
-- @since 0.29.1
instance HandleLogWriter IoLogger where
  newtype instance LogWriterM IoLogger a = IOLogWriter { runIOLogWriter :: IO a }
          deriving (Applicative, Functor, Monad)
  handleLogWriterEffect = send . Lift . runIOLogWriter

instance Handle IoLogger e a k where
  handle k q IoLogger  = k (q ^$ ())

runIoLogger :: Lifted IO e => Eff (IoLogger ': e) a -> Eff e a
runIoLogger = fix (handle_relay return)

instance forall r. (Lifted IO r, LiftedBase IO r)
  => MonadBaseControl IO (Eff (IoLogger ': r)) where
  type StM (Eff (IoLogger ': r)) a =  StM (Eff r) a
  liftBaseWith f =
    raise (liftBaseWith (\runInBase -> f (runInBase . runIoLogger)))
  restoreM = raise . restoreM

instance (LiftedBase IO r, Catch.MonadThrow (Eff r))
  => Catch.MonadThrow (Eff (IoLogger ': r)) where
  throwM exception = raise (Catch.throwM exception)

instance (LiftedBase IO r, Catch.MonadCatch (Eff r))
  => Catch.MonadCatch (Eff (IoLogger ': r)) where
  catch effect handler = do
    let nestedEffects = runIoLogger effect
        nestedHandler exception = runIoLogger (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (LiftedBase IO r, Catch.MonadMask (Eff r))
  => Catch.MonadMask (Eff (IoLogger ': r)) where
  mask maskedEffect =
    raise
      (Catch.mask
        (\nestedUnmask -> runIoLogger (maskedEffect (raise . nestedUnmask . runIoLogger)))
      )
  uninterruptibleMask maskedEffect =
    raise
      (Catch.uninterruptibleMask
        (\nestedUnmask -> runIoLogger (maskedEffect (raise . nestedUnmask . runIoLogger)))
      )
  generalBracket acquire release useIt =
    raise
      (Catch.generalBracket (runIoLogger acquire)
                            (((.) . (.)) runIoLogger release)
                            (runIoLogger . useIt)
      )
