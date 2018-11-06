-- | A logging effect based on 'Control.Monad.Log.MonadLog'.
module Control.Eff.Log.Handler
  ( logMsg
  , logMsgs
  , HasLogWriter
  , mapLogMessages
  , filterLogMessages
  , traverseLogMessages
  , changeLogWriter
  , ignoreLogs
  , traceLogs
  , LogWriter()
  , LogWriterReader
  , foldingLogWriter
  , writeAllLogMessages
  , singleMessageLogWriter
  , multiMessageLogWriter
  , askLogWriter
  , Logs()
  , writeLogs
  , runLogs
  )
where

import           Control.DeepSeq
import           Control.Eff                   as Eff
import           Control.Eff.Extend
import           Control.Eff.Reader.Strict
import           Control.Eff.Lift              as Eff
import           Data.Foldable                  ( traverse_ )
import           Data.Default
import           Control.Monad
import           Control.Monad.Trans.Control    ( MonadBaseControl
                                                  ( restoreM
                                                  , liftBaseWith
                                                  , StM
                                                  )
                                                )
import           Control.Monad.Base             ( MonadBase() )
import           Debug.Trace

-- * Message Logging Effect

-- | Log a message. The message is reduced to normal form (strict).
logMsg :: (NFData m, Member (Logs m) e) => m -> Eff e ()
logMsg (force -> msg) = logMsgs [msg]

-- | Log a bunch of messages. This might be more efficient than calling 'logMsg'
-- multiple times.
-- The messages are reduced to normal form (strict).
logMsgs
  :: (Traversable f, MonadPlus f, NFData1 f, NFData m, Member (Logs m) e)
  => f m
  -> Eff e ()
logMsgs !msgs = rnf1 msgs `seq` send (LogMsgs msgs)

-- ** Filter and Transform Log Messages

-- | Map a pure function over log messages.
mapLogMessages
  :: forall m r b . (Member (Logs m) r) => (m -> m) -> Eff r b -> Eff r b
mapLogMessages f = interpose return go
 where
  go :: Logs m a -> Arr r a b -> Eff r b
  go (LogMsgs ms) k = logMsgs (fmap f ms) >>= k

-- | Keep only those messages, for which the predicate holds.
filterLogMessages
  :: forall m r b . (Member (Logs m) r) => (m -> Bool) -> Eff r b -> Eff r b
filterLogMessages predicate = interpose return go
 where
  go :: Logs m a -> Arr r a b -> Eff r b
  go (LogMsgs ms) k = logMsgs (mfilter predicate ms) >>= k

-- ** Filter and Transform Log Messages effectfully

-- | Map an 'Eff'ectful function over every bunch of log messages.
--
-- For example, to attach the current time to each log message:
--
-- > appendTimestamp
-- >  :: ( Member (Logs String) e
-- >     , Lifted IO e)
-- >  => Eff e a
-- >  -> Eff e a
-- > appendTimestamp = traverseLogMessages $ \ms -> do
-- >   now <- lift getCurrentTime
-- >   return (fmap (show now ++) ms)
traverseLogMessages
  :: forall m r b
   . (Member (Logs m) r)
  => (  forall f
      . (MonadPlus f, Traversable f, NFData1 f)
     => f m
     -> Eff r (f m)
     )
  -> Eff r b
  -> Eff r b
traverseLogMessages f = interpose return go
 where
  go :: Logs m a -> Arr r a b -> Eff r b
  go (LogMsgs ms) k = f ms >>= logMsgs >>= k

-- ** Change the Log Writer

-- | Change the way log messages are *written*.
-- Replaces the existing 'LogWriter' by a new one. The new 'LogWriter'
-- is constructed from a function that gets a /bunch/ of messages and
-- returns an 'Eff'ect.
-- That effect has a 'Reader' for the previous 'LogWriter' and 'Lift's
-- the log writer base monad.
changeLogWriter
  :: forall r m h a
   . (Monad h, Lifted h r, Member (Reader (LogWriter m h)) r)
  => (  forall f
      . (Traversable f, NFData1 f, MonadPlus f)
     => f m
     -> Eff '[Reader (LogWriter m h), Lift h] ()
     )
  -> Eff r a
  -> Eff r a
changeLogWriter interceptor =
  let replaceWriter old = LogWriter (runLift . runReader old . interceptor)
  in  local replaceWriter

-- * Handle Log Messages

-- | Throw away all log messages.
ignoreLogs :: forall message r a . Eff (Logs message ': r) a -> Eff r a
ignoreLogs = handle_relay return go
 where
  go :: Logs message v -> Arr r v a -> Eff r a
  go (LogMsgs _) k = k ()

-- | Trace all log messages using 'traceM'. The message value is
-- converted to 'String' using the given function.
traceLogs
  :: forall message r a
   . (message -> String)
  -> Eff (Logs message ': r) a
  -> Eff r a
traceLogs toString = handle_relay return go
 where
  go :: Logs message v -> Arr r v a -> Eff r a
  go (LogMsgs ms) k = traverse_ (traceM . toString) ms >> k ()

-- ** Log Message Writer Creation

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter message writerM =
  LogWriter
  { runLogWriter
      :: forall f. (MonadPlus f, Traversable f, NFData1 f)
      => f message
      -> writerM ()
  }

instance Applicative w => Default (LogWriter m w) where
  def = LogWriter (const (pure ()))

-- | Type alias for the 'Reader' effect that writes logs
type LogWriterReader message writerM =
  Reader (LogWriter message writerM)

-- | Create a 'LogWriter' from a function that can write
-- a 'Traversable' container.
foldingLogWriter
  :: (  forall f
      . (MonadPlus f, Traversable f, NFData1 f)
     => f message
     -> writerM ()
     )
  -> LogWriter message writerM
foldingLogWriter = LogWriter

-- | Efficiently apply the 'LogWriter' to a 'Traversable' container of log
-- messages.
writeAllLogMessages
  :: (NFData1 f, MonadPlus f, Traversable f, Applicative writerM)
  => LogWriter message writerM
  -> f message
  -> writerM ()
writeAllLogMessages = runLogWriter

-- | Create a 'LogWriter' from a function that is applied to each
-- individual log message. NOTE: This is probably the simplest,
-- but also the most inefficient and annoying way to make
-- a 'LogWriter'. Better use 'foldingLogWriter' or even
-- 'multiMessageLogWriter'.
singleMessageLogWriter
  :: (Applicative writerM)
  => (message -> writerM ())
  -> LogWriter message writerM
singleMessageLogWriter writeMessage = foldingLogWriter (traverse_ writeMessage)

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
  :: (Applicative writerM)
  => (((message -> writerM ()) -> writerM ()) -> writerM ())
  -> LogWriter message writerM
multiMessageLogWriter withMessageWriter =
  foldingLogWriter (\xs -> withMessageWriter (\writer -> traverse_ writer xs))

-- | Get the current 'LogWriter'
askLogWriter
  :: forall m h r . (Member (Reader (LogWriter m h)) r) => Eff r (LogWriter m h)
askLogWriter = ask

-- ** Low-Level Log Message Sending

-- | A constraint that combines constraints for logging into any
-- log writer monad.
type HasLogWriter message logWriterMonad effects =
  ( Member (Reader (LogWriter message logWriterMonad)) effects
  , Member (Logs message) effects
  , NFData message
  , Monad logWriterMonad
  , Lifted logWriterMonad effects
  )

-- | This effect sends log messages.
-- The logs are not sent one-by-one, but always in batches of
-- containers that must be 'Traversable' and 'MonadPlus' instances.
-- Log messages are consumed by 'LogWriter's installed via
-- 'runLogs' or more high level functions like 'writeLogs'.
data Logs m v where
  LogMsgs
    :: (Traversable f, MonadPlus f, NFData1 f, NFData m)
    => f m
    -> Logs m ()

-- | Install 'Logs' handler that 'ask's for a 'LogWriter' for the
-- message type and applies the log writer to the messages.
runLogs
  :: forall m h e b
   . (Applicative h, Lifted h e, Member (LogWriterReader m h) e)
  => Eff (Logs m ': e) b
  -> Eff e b
runLogs = handle_relay return go
 where
  go :: Logs m a -> Arr e a b -> Eff e b
  go (LogMsgs ms) k = do
    logWrtr <- ask
    lift (writeAllLogMessages logWrtr ms)
    k ()

-- | Handle log message effects by a monadic action, e.g. an IO action
-- to send logs to the console output or a log-server.
-- The monadic log writer action is wrapped in a newtype called
-- 'LogWriter'.
--
-- Use the smart constructors below to create them, e.g.
-- 'foldingLogWriter', 'singleMessageLogWriter' or
-- 'mulitMessageLogWriter'.
writeLogs
  :: forall message writerM r a
   . (Applicative writerM, Lifted writerM r)
  => LogWriter message writerM
  -> Eff (Logs message ': Reader (LogWriter message writerM) ': r) a
  -> Eff r a
writeLogs w = runReader w . runLogs

-- | This instance allows liftings of the 'Logs' effect, but only, if there is
-- a 'LogWriter' in effect.
instance
  ( MonadBase m m
  , Lifted m r
  , MonadBaseControl m (Eff r)
  )
  => MonadBaseControl m (Eff (Logs l ': LogWriterReader l m ': r)) where

    type StM (Eff (Logs l ': LogWriterReader l m ': r)) a =
      StM (Eff r) a

    liftBaseWith f = do
      l <- askLogWriter
      raise (raise (liftBaseWith (\runInBase -> f (runInBase . writeLogs l))))

    restoreM = raise . raise . restoreM
