module Control.Eff.Log.Writer
  ( LogWriter(MkLogWriter, runLogWriter)
  , LogWriterReader
  , HasLogWriter
  , askLogWriter
  , foldingLogWriter
  , writeAllLogMessages
  , singleMessageLogWriter
  , multiMessageLogWriter
  ) where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Monad
import Data.Default
import Data.Foldable (traverse_)

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter message writerM = MkLogWriter
  { runLogWriter :: forall f. (MonadPlus f, Traversable f, NFData1 f) =>
                                f message -> writerM ()
  }

instance Applicative w => Default (LogWriter m w) where
  def = MkLogWriter (const (pure ()))

-- | Type alias for the 'Reader' effect that writes logs
type LogWriterReader message writerM = Reader (LogWriter message writerM)

-- | A constraint that combines constraints for logging into any
-- log writer monad.
type HasLogWriter message logWriterMonad effects
   = ( Member (Reader (LogWriter message logWriterMonad)) effects
     , NFData message
     , Monad logWriterMonad
     , Lifted logWriterMonad effects)

-- | Get the current 'LogWriter'
askLogWriter ::
     forall m h r. (Member (Reader (LogWriter m h)) r)
  => Eff r (LogWriter m h)
askLogWriter = ask

-- | Create a 'LogWriter' from a function that can write
-- a 'Traversable' container.
foldingLogWriter ::
     (forall f. (MonadPlus f, Traversable f, NFData1 f) =>
                  f message -> writerM ())
  -> LogWriter message writerM
foldingLogWriter = MkLogWriter

-- | Efficiently apply the 'LogWriter' to a 'Traversable' container of log
-- messages.
writeAllLogMessages ::
     (NFData1 f, MonadPlus f, Traversable f, Applicative writerM)
  => LogWriter message writerM
  -> f message
  -> writerM ()
writeAllLogMessages = runLogWriter

-- | Create a 'LogWriter' from a function that is applied to each
-- individual log message. NOTE: This is probably the simplest,
-- but also the most inefficient and annoying way to make
-- a 'LogWriter'. Better use 'foldingLogWriter' or even
-- 'multiMessageLogWriter'.
singleMessageLogWriter :: (Applicative writerM) => (message -> writerM ()) -> LogWriter message writerM
singleMessageLogWriter writeMessage = foldingLogWriter (traverse_ writeMessage)

-- | Create a 'LogWriter' from a function that is applied to each
-- individual log message. Don't be scared by the type signature,
-- here is an example file appender that re-opens the log file
-- every time a bunch of log messages are written:
--
-- > fileAppender fn = multiMessageLogWriter
-- >   (\writeLogMessageWith ->
-- >      withFile fn AppendMode (writeLogMessageWith . hPutStrLn))
--
multiMessageLogWriter ::
     (Applicative writerM) => (((message -> writerM ()) -> writerM ()) -> writerM ()) -> LogWriter message writerM
multiMessageLogWriter withMessageWriter = foldingLogWriter (\xs -> withMessageWriter (\writer -> traverse_ writer xs))