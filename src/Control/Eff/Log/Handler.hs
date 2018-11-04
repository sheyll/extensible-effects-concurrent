{-# LANGUAGE ImplicitParams #-}

-- | A logging effect based on 'Control.Monad.Log.MonadLog'.
module Control.Eff.Log.Handler
  ( logMsg
  , logMsgs
  , LogsM
  , Logs
  , handleLogs
  , ignoreLogs
  , mapLogMessages
  , filterLogMessages
  , interceptLogging
  , HasLogWriter
  , HasLogWriterIO
  , LogWriter()
  , writeAllLogMessages
  , foldingLogWriter
  , singleMessageLogWriter
  , multiMessageLogWriter
  , HasLogWriterProxy
  , LogWriterProxy(..)
  , usingLogWriterProxy
  , logWriterProxy
  , askLogWriter
  )
where

import           Control.DeepSeq
import           Control.Eff                   as Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Lift              as Eff
import           Data.Foldable                  ( traverse_, toList )
import           Data.Kind
import           Data.Functor.Identity
import           GHC.Stack
import           Data.Default

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter message writerM =
  LogWriter
  { runLogWriter
      :: forall f. (HasCallStack, Traversable f, Foldable f, Functor f)
      => f message
      -> writerM ()
  }

instance Applicative w => Default (LogWriter m w) where
  def = LogWriter (const (pure ()))

-- | Efficiently apply the 'LogWriter' to a 'Foldable' container of log
-- messages.
writeAllLogMessages
  :: (Foldable f, Functor f, Traversable f, Applicative writerM, HasCallStack)
  => LogWriter message writerM -> f message -> writerM ()
writeAllLogMessages = runLogWriter

-- | Create a 'LogWriter' from a function that can write
-- a 'Foldable' container.
foldingLogWriter
  :: (forall f. (Foldable f, Traversable f, HasCallStack) => f message -> writerM ())
  -> LogWriter message writerM
foldingLogWriter = LogWriter

-- | Create a 'LogWriter' from a function that is applied to each
-- individual log message. NOTE: This is probably the simplest,
-- but also the most inefficient and annoying way to make
-- a 'LogWriter'. Better use 'foldingLogWriter' or even
-- 'multiMessageLogWriter'.
singleMessageLogWriter
  :: (Applicative writerM, HasCallStack)
  => (message -> writerM ())
  -> LogWriter message writerM
singleMessageLogWriter writeMessage =
  foldingLogWriter (traverse_ writeMessage)

-- | Create a 'LogWriter' from a function that is applied to each
-- individual log message. Don't be scared by the type signature,
-- here is an example file appender that re-opens the log file
-- everytime a bunch of log messages are written:
--
-- > fileAppender fn = multiMessageLogWriter
-- >   (\writeLogMessageWith ->
-- >      withFile fn AppendMode (writeLogMessageWith . hPutStrLn))
--
multiMessageLogWriter
  :: (Applicative writerM, HasCallStack)
  => (((message -> writerM ()) -> writerM ()) -> writerM ())
  -> LogWriter message writerM
multiMessageLogWriter withMessageWriter =
  foldingLogWriter
    (\xs -> withMessageWriter (\writer -> traverse_ writer xs))

-- | Logging effect type, parameterized by a log message type. This is a reader
-- for a 'LogWriter'.
type LogsM message writerM = Reader (LogWriter message writerM)

-- | The logging effect on IO.
type Logs message = LogsM message IO

-- | A constraint that combines constraints for logging into any
-- log writer monad.
type HasLogWriter message logWriterMonad effects =
  ( Member (LogsM message logWriterMonad) effects
  , NFData message
  , Monad logWriterMonad
  , HasLogWriterProxy logWriterMonad
  , Lifted logWriterMonad effects )

-- | A constraint that combines constraints for IO based logging.
type HasLogWriterIO message effects =
  ( Member (Logs message) effects
  , NFData message
  , HasLogWriterProxy IO
  , Lifted IO effects )

-- | Type constraint alias for the implicit 'LogWriterProxy'
type HasLogWriterProxy m = ( ?logWriterProxy :: LogWriterProxy m )

-- | A proxy type to carry the 'LogWriter' effects.
data LogWriterProxy (e :: Type -> Type) = LogWriterProxy

-- | Set the implicit parameter defined in 'HasLogWriterProxy' for function
-- applications like 'logMsg'
usingLogWriterProxy
  :: forall e r a
  . LogWriterProxy e
  -> (HasLogWriterProxy e => Eff r a) -> Eff r a
usingLogWriterProxy px e = let ?logWriterProxy = px in e

-- | Return the current 'LogWriterProxy'
logWriterProxy :: HasLogWriterProxy e => LogWriterProxy e
logWriterProxy = ?logWriterProxy

-- | Get the current 'LogWriter'
askLogWriter :: forall m h r . (HasLogWriter m h r) => Eff r (LogWriter m h)
askLogWriter = ask

-- | Log a message.
logMsg :: (NFData m, HasLogWriter m h r) => m -> Eff r ()
logMsg !(force -> m) = do
  lw <- askLogWriter
  lift (runLogWriter lw (Identity m))

-- | Log a bunch of messages. This might be more efficient than calling 'logMsg'
-- multiple times.
logMsgs :: (NFData (f m), HasLogWriter m h r, Functor f, Traversable f, Foldable f) => f m -> Eff r ()
logMsgs !(force -> m) = do
  lw <- askLogWriter
  lift (runLogWriter lw m)

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
  :: forall r m h a
   . (HasLogWriter m h r)
  => (m -> Eff '[LogsM m h, Lift h] ())
  -> (HasLogWriterProxy h => Eff r a)
  -> Eff r a
interceptLogging interceptor  =
  let replaceWriter old =
        multiMessageLogWriter
          ($ (runLift . runReader old . interceptor))
  in local replaceWriter

-- | Map a pure function over log messages.
mapLogMessages
  :: forall r m h a
   . (HasLogWriter m h r)
  => (m -> m)
  -> (HasLogWriterProxy h => Eff r a)
  -> Eff r a
mapLogMessages f =
  local @(LogWriter m h)
    (\(LogWriter old) ->
      LogWriter (\xs ->
        old (fmap f xs)))

-- | Filter which messages are handled.
filterLogMessages
  :: forall r m h a
   . (HasLogWriter m h r, HasCallStack)
  => (m -> Bool)
  -> (HasLogWriterProxy h => Eff r a)
  -> Eff r a
filterLogMessages predicate =
  local
    @(LogWriter m h)
    (\(LogWriter old) ->
      LogWriter (old . filter predicate . toList))

-- | Define the the 'LogWriter' callback and set the 'HasLogWriterProxy'
-- constraint in the given log writer action. This assumes that the monad used
-- is 'Lift'ed to @r@.
handleLogs
  :: forall message writerM r a
   . (Lifted writerM r)
  => LogWriter message writerM
  -> (HasLogWriterProxy writerM => Eff (LogsM message writerM ': r) a)
  -> Eff r a
handleLogs lw e =
  usingLogWriterProxy (LogWriterProxy @writerM) (runReader lw e)

-- | Throw away all log messages.
ignoreLogs
  :: forall message m r a
  . (Lifted m r, Applicative m)
  => (HasLogWriterProxy m => Eff (LogsM message m ': r) a)
  -> Eff r a
ignoreLogs = handleLogs def
