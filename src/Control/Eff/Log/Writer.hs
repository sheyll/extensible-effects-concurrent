{-# LANGUAGE UndecidableInstances #-}
-- | The 'LogWriter' type encapsulates an 'IO' action to write 'LogEvent's.
module Control.Eff.Log.Writer
  (
  -- * 'LogWriter' Definition
    LogWriter(..)
  -- * LogWriter Reader Effect
  , LogWriterReader
  , localLogWriterReader
  , askLogWriter
  , runLogWriterReader
  -- * LogWriter utilities
  , liftWriteLogMessage
  , noOpLogWriter
  , filteringLogWriter
  , mappingLogWriter
  , mappingLogWriterIO
  , ioHandleLogWriter
  , stdoutLogWriter
  )
where

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Log.Message
import           Control.Eff.Log.MessageRenderer
import           Control.Monad                  ( (>=>)
                                                , when
                                                )
import qualified Data.Text.IO as Text
import           Data.Text (Text)
import qualified System.IO as IO

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter = MkLogWriter
  { runLogWriter :: LogEvent -> IO ()
  }

instance Semigroup LogWriter where
  (MkLogWriter l) <> (MkLogWriter r) = MkLogWriter (l >> r)

instance Monoid LogWriter where
  mempty = MkLogWriter (const (pure ()))

-- | A 'Reader' effect for 'LogWriter's.
--
-- @since 0.31.0
type LogWriterReader = Reader LogWriter

-- | Provide the 'LogWriter'
--
-- Exposed for custom extensions, if in doubt use 'withLogging'.
runLogWriterReader :: LogWriter -> Eff (Reader LogWriter ': e) a -> Eff e a
runLogWriterReader = runReader

-- | Get the current 'LogWriter'.
askLogWriter :: Member LogWriterReader e => Eff e LogWriter
askLogWriter = ask

-- | Modify the current 'LogWriter'.
localLogWriterReader
  :: forall e a
   . Member LogWriterReader e
  => (LogWriter -> LogWriter)
  -> Eff e a
  -> Eff e a
localLogWriterReader = local

-- | Write a message using the 'LogWriter' found in the environment.
liftWriteLogMessage
  :: ( Member LogWriterReader  e, Lifted IO e)
  => LogEvent
  -> Eff e ()
liftWriteLogMessage m = do
  w <- askLogWriter
  lift (runLogWriter w m)

-- | This 'LogWriter' will discard all messages.
--
-- NOTE: This is just an alias for 'mempty'
noOpLogWriter :: LogWriter
noOpLogWriter = mempty

-- | A 'LogWriter' that applies a predicate to the 'LogEvent' and delegates to
-- to the given writer of the predicate is satisfied.
filteringLogWriter :: LogPredicate -> LogWriter -> LogWriter
filteringLogWriter p lw =
  MkLogWriter (\msg -> when (p msg) (runLogWriter lw msg))

-- | A 'LogWriter' that applies a function to the 'LogEvent' and delegates the result to
-- to the given writer.
mappingLogWriter :: (LogEvent -> LogEvent) -> LogWriter -> LogWriter
mappingLogWriter f lw = MkLogWriter (runLogWriter lw . f)

-- | Like 'mappingLogWriter' allow the function that changes the 'LogEvent' to have effects.
mappingLogWriterIO
  :: (LogEvent -> IO LogEvent) -> LogWriter -> LogWriter
mappingLogWriterIO f lw = MkLogWriter (f >=> runLogWriter lw)

-- | Append the 'LogEvent' to an 'IO.Handle' after rendering it.
--
-- @since 0.31.0
ioHandleLogWriter :: IO.Handle -> LogEventReader Text -> LogWriter
ioHandleLogWriter outH r = MkLogWriter (Text.hPutStrLn outH . r)

-- | Render a 'LogEvent' to 'IO.stdout'.
--
-- This function will also set the 'IO.BufferMode' of 'IO.stdout' to 'IO.LineBuffering'.
--
-- See 'ioHandleLogWriter'.
--
-- @since 0.31.0
stdoutLogWriter :: LogEventReader Text -> IO LogWriter
stdoutLogWriter render = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  return (ioHandleLogWriter IO.stdout render)
