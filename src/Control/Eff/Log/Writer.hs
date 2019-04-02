module Control.Eff.Log.Writer
  ( LogWriter(MkLogWriter, runLogWriter)
  , applyLogWriter
  , foldingLogWriter
  , singleMessageLogWriter
  , multiMessageLogWriter
  , localLogWriter
  , traverseLogMessages
  , withLogWriter
  , writeLogs
  , writeLogsFiltered
  , LogWriterReader
  , runLogWriterReader
  , HasLogWriter
  , askLogWriter
  ) where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Log.Handler
import Control.Eff.Reader.Lazy
import Data.Default
import Data.Foldable (traverse_)

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter message writerM = MkLogWriter
  { runLogWriter :: NFData message => message -> writerM ()
  }

-- | Write a log message using a 'LogWriter'.
applyLogWriter :: (NFData message, Lifted writerM e) => LogWriter message writerM -> message -> Eff e ()
applyLogWriter w m = lift (runLogWriter w m)

instance Applicative w => Default (LogWriter m w) where
  def = MkLogWriter (const (pure ()))

-- | Create a 'LogWriter' for a log message type that is a 'Foldable' composite message type,
-- like e.g.: @[String]@.
--
-- This is just an alias for @'MkLogWriter'@.
foldingLogWriter
  :: (Foldable f, NFData1 f, NFData (f message))
  => (f message -> writerM ())
  -> LogWriter (f message) writerM
foldingLogWriter = MkLogWriter

-- | Create a 'LogWriter' for single, atomic message types like @String@ or @Text@.
--
-- This is just an alias for @'MkLogWriter'@.
singleMessageLogWriter
  :: (Traversable f, Applicative writerM, NFData1 f, NFData (f message))
  => (message -> writerM ())
  -> LogWriter message writerM
singleMessageLogWriter = MkLogWriter

-- | Create a 'LogWriter' from a function that is applied to each
-- individual log message. Don't be scared by the type signature,
-- here is an example file appender that re-opens the log file
-- every time a bunch of log messages are written:
--
-- > fileAppender fn = multiMessageLogWriter
-- >   (\writeLogMessageWith ->
-- >      withFile fn AppendMode (writeLogMessageWith . hPutStrLn))
--
multiMessageLogWriter
  :: (Traversable f, Applicative writerM, NFData1 f, NFData (f message))
  => (((message -> writerM ()) -> writerM ()) -> writerM ())
  -> LogWriter (f message) writerM
multiMessageLogWriter withMessageWriter =
  foldingLogWriter (\xs -> withMessageWriter (`traverse_` xs))

-- * A 'Reader' for the current 'LogWriter'

-- | Type alias for the 'Reader' effect that writes logs
type LogWriterReader message writerM = Reader (LogWriter message writerM)

-- | Handle the 'LogWriterReader' Effect.
runLogWriterReader :: LogWriter msg wm -> Eff (LogWriterReader msg wm ': e) a -> Eff e a
runLogWriterReader = runReader

-- | A constraint that combines constraints for logging into any
-- log writer monad.
type HasLogWriter message logWriterMonad effects
   = ( Member (Reader (LogWriter message logWriterMonad)) effects
     , Member (Logs message) effects
     , NFData message
     , Monad logWriterMonad
     , Lifted logWriterMonad effects)

-- | Get the current 'LogWriter'
askLogWriter ::
     forall m h r. (Member (Reader (LogWriter m h)) r)
  => Eff r (LogWriter m h)
askLogWriter = ask

-- ** Change the Log Writer

-- | Change the way log messages are *written*.
-- Replaces the existing 'LogWriter' by a new one. The new 'LogWriter'
-- is constructed from a function that gets a /bunch/ of messages and
-- returns an 'Eff'ect.
-- That effect has a 'Reader' for the previous 'LogWriter' and 'Lift's
-- the log writer base monad.
localLogWriter
  :: forall r m h a
   . (Monad h, Lifted h r, Member (Reader (LogWriter m h)) r)
  => (m -> Eff '[Reader (LogWriter m h), Lift h] ())
  -> Eff r a
  -> Eff r a
localLogWriter interceptor =
  let replaceWriter old = MkLogWriter (runLift . runReader old . interceptor)
  in  local replaceWriter

-- ** Filter and Transform Log Messages effectfully

-- | Map an 'Eff'ectful function over log messages.
--
-- For example, to attach the current time to each log message:
--
-- > appendTimestamp
-- >  :: ( Member (Logs String) e
-- >     , Lifted IO e)
-- >  => Eff e a
-- >  -> Eff e a
-- > appendTimestamp = traverseLogMessages $ \ms -> do
-- >   now <- getCurrentTime
-- >   return (fmap (show now ++) ms)
traverseLogMessages
  :: forall m r h b
   . ( Member (Logs m) r
     , Member (Reader (LogWriter m h)) r
     , Monad h
     , Lifted h r
     , NFData m
     )
  => (m -> h m)
  -> Eff r b
  -> Eff r b
traverseLogMessages f = localLogWriter
  (\msg -> do
    lw   <- ask
    msg' <- lift (f msg)
    lift (runLogWriter lw msg')
  )

-- * Handle Logs by Writing them

-- | Apply a 'LogWriter' to those log messages, that pass the filter.
withLogWriter
  :: forall m h e b
   . (NFData m, Lifted h e, SetMember Logs (Logs m) e)
  => LogWriter m h
  -> Eff e b
  -> Eff e b
withLogWriter = withLogMessageHandler . applyLogWriter

-- | Handle the 'Logs' effect, as well as the 'LogWriterReader' effect.
writeLogsFiltered
  :: forall message writerM r a
   . (Applicative writerM, Lifted writerM r, NFData message)
  => (message -> Maybe message)
  -> LogWriter message writerM
  -> Eff (Logs message ': LogWriterReader message writerM ': r) a
  -> Eff r a
writeLogsFiltered f w = runLogWriterReader w . runLogsFiltered f . withLogWriter w

-- | Handle the 'Logs' effect, as well as the 'LogWriterReader' effect.
--
-- This behaves like @'writeLogsFiltered' Just@
writeLogs
  :: forall message writerM r a
   . (Applicative writerM, Lifted writerM r, NFData message)
  => LogWriter message writerM
  -> Eff (Logs message ': LogWriterReader message writerM ': r) a
  -> Eff r a
writeLogs = writeLogsFiltered Just

