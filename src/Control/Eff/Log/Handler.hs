-- | A logging effect based on 'Control.Monad.Log.MonadLog'.
module Control.Eff.Log.Handler
  (
    -- * Logging Effect
    Logs
  , LogWriter(..)
  , askLogWriter
  , logMsg
  , interceptLogging
  , foldLogMessages
  , ignoreLogs
  , handleLogsWith
  , handleLogsWithLoggingTHandler
  )
where

import           Control.DeepSeq
import           Control.Eff                   as Eff
import           Control.Eff.Reader.Strict
import qualified Control.Eff.Lift              as Eff
import qualified Control.Monad.Log             as Log
import           Control.Monad.IO.Class
import           Data.Foldable                  ( traverse_ )
import           Data.Kind                      ( )

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter message = LogWriter { runLogWriter :: message -> IO () }

-- | Logging effect type, parameterized by a log message type. This is a reader
-- for a 'LogWriter'.
type Logs message = Reader (LogWriter message)

-- | Log a message.
logMsg :: (NFData m, MonadIO (Eff r), Member (Logs m) r) => m -> Eff r ()
logMsg !(force -> m) = do
  lw <- ask
  liftIO (runLogWriter lw m)


-- | Get the current 'LogWriter'
askLogWriter :: (Member (Logs m) r) => Eff r (LogWriter m)
askLogWriter = ask

-- | Change, add or remove log messages and perform arbitrary actions upon
-- intercepting a log message.
--
-- Requirements:
--
--   * All log meta data for typical prod code can be added without
--     changing much of the code
--
--   * Add timestamp to a log messages of a sub-computation.
--
--   * Write some messages to a file.
--
--   * Log something extra, e.g. runtime memory usage in load tests
--
-- Approach: Install a callback that sneaks into to log message
-- sending/receiving, to intercept the messages and execute some code and then
-- return a new message.
interceptLogging
  :: forall r m a
   . (Member (Logs m) r, Eff.Lifted IO r)
  => (m -> Eff '[Logs m, Eff.Lift IO] ())
  -> Eff r a
  -> Eff r a
interceptLogging interceptor action = do
  old <- ask
  let new = LogWriter (Eff.runLift . runReader old . interceptor)
  local (const new) action

-- | Intercept logging to change, add or remove log messages.
--
-- This is without side effects, hence faster than 'interceptLogging'.
foldLogMessages
  :: forall r m a f
   . (NFData m, Foldable f, Member (Logs m) r, Eff.Lifted IO r)
  => (m -> f m)
  -> Eff r a
  -> Eff r a
foldLogMessages f = interceptLogging (traverse_ logMsg . f)

-- | Throw away all log messages.
ignoreLogs :: forall message r a . Eff (Logs message ': r) a -> Eff r a
ignoreLogs = runReader (LogWriter (const (pure ())))

-- | Apply a function that returns an effect to each log message.
handleLogsWith
  :: forall message r a
   . (message -> IO ())
  -> Eff (Logs message ': r) a
  -> Eff r a
handleLogsWith = runReader . LogWriter

-- | Handle the 'Logs' effect using 'Log.LoggingT' 'Log.Handler's.
handleLogsWithLoggingTHandler
  :: forall r message a
   . (Eff.Lifted IO r)
  => (forall b . (Log.Handler IO message -> IO b) -> IO b)
  -> Eff (Logs message ': r) a
  -> Eff r a
handleLogsWithLoggingTHandler foldHandler =
  handleLogsWith (foldHandler . flip ($!))
