{-# LANGUAGE UndecidableInstances #-}
-- | Functions to write 'LogMessages'.
module Control.Eff.Log.Writer
  ( LogWriter(MkLogWriter, runLogWriter)
  , noOpLogWriter
  , writeFiltered
  , writeModified
  , writeModifiedM
  , makeIoLogWriter
  , writeToIoHandle
  , LiftsLogWriter(..)
  , traceLogMessages
  , TraceLogs(..)
  , captureLogMessages
  , CaptureLogs(..)
  , CapturedLogsWriter
  , runCapturedLogsWriter
  , LogWriterReader
  , runLogWriterReader
  , askLogWriter
  , localLogWriter
  , localAddLogWriter
  , localModifiedWriter
  , localModifiedWriterM
  , localFilteredWriter
  , setThreadIdAndTimestamp
  , increaseLogMessageDistance
  , dropDistantLogMessages
  , withLogFileAppender
  ) where

import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Eff.Log.Message
import Data.Default
import Debug.Trace
import GHC.Stack
import Control.Eff.Writer.Strict (Writer, tell, runListWriter)
import Data.Functor.Identity (Identity)
import Control.DeepSeq (deepseq)
import Data.Foldable (traverse_)
import System.IO
import Control.Monad ((>=>))
import Control.Lens ((^.), over)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import qualified Control.Exception.Safe as Safe
import qualified Control.Monad.Catch as Catch
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter writerM = MkLogWriter
  { runLogWriter :: LogMessage -> writerM ()
  }

instance Applicative w => Default (LogWriter w) where
  def = MkLogWriter (const (pure ()))

-- * Basic 'LogWriter's

-- | This 'LogWriter' will discard all messages.
--
-- NOTE: This is just an alias for 'def'
noOpLogWriter :: Applicative m => LogWriter m
noOpLogWriter = def

-- | A 'LogWriter' that applies a predicate to the 'LogMessage' and delegates to
-- to the given writer of the predicate is satisfied.
writeFiltered :: Monad e => LogPredicate -> LogWriter e -> LogWriter e
writeFiltered p lw = MkLogWriter (\msg -> if p msg then (runLogWriter lw msg) else return ())

-- | A 'LogWriter' that applies a function to the 'LogMessage' and delegates the result to
-- to the given writer.
writeModified :: (LogMessage -> LogMessage) -> LogWriter e -> LogWriter e
writeModified f lw = MkLogWriter (runLogWriter lw . f)

-- | Like 'writeModified' allow the function that changes the 'LogMessage' to have effects.
writeModifiedM :: Monad e => (LogMessage -> e LogMessage) -> LogWriter e -> LogWriter e
writeModifiedM f lw = MkLogWriter (f >=> runLogWriter lw)

-- * LogWriter liftings

-- | This class describes how to lift the log writer action into some monad.
--
-- The second parameter is almost always @Eff x@
-- so usually the method of this class lifts the log writer action into an effect monad.
class LiftsLogWriter h e where
  liftLogWriter :: LogWriter h -> LogMessage -> Eff e ()

-- ** 'IO' based 'LogWriter's

-- | A 'LogWriter' that uses an 'IO' action to write the message.
makeIoLogWriter :: HasCallStack => (LogMessage-> IO ()) -> LogWriter IO
makeIoLogWriter = MkLogWriter

-- | A 'LogWriter' that uses an 'IO' action to write the message.
writeToIoHandle :: HasCallStack => Handle -> LogWriter IO
writeToIoHandle h = makeIoLogWriter (hPutStrLn h . renderLogMessage)

instance (Lifted IO e) => LiftsLogWriter IO e where
  liftLogWriter = (lift . ) . runLogWriter

-- * LogWriter variants

-- ** Pure Log Writers

-- | A 'LogWriter' that applies 'renderLogMessage' to the log message and then
-- traces it using 'traceM'.
traceLogMessages :: LogWriter TraceLogs
traceLogMessages = MkLogWriter (traceM . renderLogMessage)

-- | The monad for 'traceLogMessages'.
-- This is just a wrapper around 'Identity' and serves as a type that has a special
-- 'LiftsLogWriter' instance.
newtype TraceLogs a = MkTraceLogs { runTraceLogs :: Identity a }
  deriving (Functor, Applicative, Monad)

-- | A 'LogWriter' monad for 'Debug.Trace' based pure logging.
instance LiftsLogWriter TraceLogs e where
  liftLogWriter lw msg = deepseq (runTraceLogs (runLogWriter lw msg)) (return ())

-- | A 'LogWriter' monad that provides pure logging by capturing via the 'Writer' effect.
captureLogMessages :: LogWriter CaptureLogs
captureLogMessages = MkLogWriter (MkCaptureLogs . tell)

-- | A 'LogWriter' monad that provides pure logging by capturing via the 'Writer' effect.
newtype CaptureLogs a = MkCaptureLogs { unCaptureLogs :: Eff '[CapturedLogsWriter] a }
  deriving (Functor, Applicative, Monad)

-- | A 'LogWriter' monad for pure logging.
--
-- The 'LiftsLogWriter' instance for this type assumes a 'Writer' effect.
instance Member CapturedLogsWriter e => LiftsLogWriter CaptureLogs e where
  liftLogWriter lw = traverse_ (tell @LogMessage) . snd . run . runListWriter . unCaptureLogs . runLogWriter lw

-- | Run a 'Writer' for 'LogMessage's.
--
-- Such a 'Writer' is needed for 'CaptureLogs'
runCapturedLogsWriter :: Eff (CapturedLogsWriter ': e) a -> Eff e (a, [LogMessage])
runCapturedLogsWriter = runListWriter

-- | Alias for the 'Writer' that contains the captured 'LogMessage's from 'CaptureLogs'.
type CapturedLogsWriter = Writer LogMessage

-- * 'LogWriter' 'Reader'

-- | A 'Reader' for the 'LogWriter'
type LogWriterReader h = Reader (LogWriter h)

-- | Provide the 'LogWriter'
runLogWriterReader :: LogWriter h -> Eff (LogWriterReader h ': e) a -> Eff e a
runLogWriterReader = runReader

-- | Get the current 'LogWriter'.
askLogWriter :: Member (LogWriterReader h) e => Eff e (LogWriter h)
askLogWriter = ask

-- | Change the current 'LogWriter'.
localLogWriter :: Member (LogWriterReader h) e => (LogWriter h -> LogWriter h) -> Eff e a -> Eff e a
localLogWriter = local

-- | Modify the the 'LogMessage's written in the given sub-expression.
--
-- Note: This is equivalent to @'localLogWriter' . 'writeModified'@
localModifiedWriter :: forall h e a . (Member (LogWriterReader h) e) => (LogMessage -> LogMessage) -> Eff e a -> Eff e a
localModifiedWriter = localLogWriter @h . writeModified

-- | Modify the the 'LogMessage's written in the given sub-expression, as in 'localModifiedWriter'
-- but with a effectful function.
--
-- Note: This is equivalent to @'localLogWriter' . 'writeModifiedM'@
localModifiedWriterM :: forall h e a . (Monad h, Member (LogWriterReader h) e) => (LogMessage -> h LogMessage) -> Eff e a -> Eff e a
localModifiedWriterM = localLogWriter @h . writeModifiedM

-- | Combine the effects of a given 'LogWriter' and the existing one.
--
-- @runLogWriterReader w1 . localAddLogWriter w2  == runLogWriterReader (\m -> w1 m >> w2 m)@
localAddLogWriter :: forall h e a . (Monad h, Member (LogWriterReader h) e) => LogWriter h -> Eff e a -> Eff e a
localAddLogWriter lw2 = localLogWriter (\lw1 -> MkLogWriter (\m -> runLogWriter lw1 m >> runLogWriter lw2 m))

-- | Filter the 'LogMessage's written in the given sub-expression with the given
--   'LogPredicate'.
--
-- Note: This is equivalent to @'localLogWriter' . 'writeFiltered'@
localFilteredWriter :: forall h e a . (Monad h, Member (LogWriterReader h) e) => LogPredicate -> Eff e a -> Eff e a
localFilteredWriter = localLogWriter @h . writeFiltered

-- | Set the current thread id and time of each 'LogMessage' (at the moment the message is logged).
setThreadIdAndTimestamp :: (LiftsLogWriter IO e, Member (LogWriterReader IO) e, Lifted IO e) => Eff e a -> Eff e a
setThreadIdAndTimestamp = localModifiedWriterM @IO (setLogMessageThreadId >=> setLogMessageTimestamp)

-- ** Distance Based Log Message Filtering

-- | Increase the /distance/ of log messages by one.
-- Logs can be filtered by their distance with 'dropDistantLogMessages'
increaseLogMessageDistance
  :: forall h e a . (HasCallStack, Member (LogWriterReader h) e) => Eff e a -> Eff e a
increaseLogMessageDistance = localModifiedWriter @h (over lmDistance (+ 1))

-- | Drop all log messages with an 'lmDistance' greater than the given
-- value.
dropDistantLogMessages :: forall h e a . (HasCallStack, Monad h, Member (LogWriterReader h) e) => Int -> Eff e a -> Eff e a
dropDistantLogMessages maxDistance =
  localFilteredWriter @h (\lm -> lm ^. lmDistance <= maxDistance)

-- ** File Based Logging

-- | Open a file and add the 'LogWriter' in the 'LogWriterReader' tha appends the log messages to it.
withLogFileAppender
  :: (HasCallStack, LiftsLogWriter IO e, Lifted IO e, Member (LogWriterReader IO) e, MonadBaseControl IO (Eff e))
  => FilePath
  -> Eff e b
  -> Eff e b
withLogFileAppender fnIn e = liftBaseOp withOpenedLogFile (`localAddLogWriter` e)
  where
    withOpenedLogFile
      :: HasCallStack
      => (LogWriter IO -> IO a)
      -> IO a
    withOpenedLogFile ioE =
      Safe.bracket
          (do
            fnCanon <- canonicalizePath fnIn
            createDirectoryIfMissing True (takeDirectory fnCanon)
            h <- openFile fnCanon AppendMode
            hSetBuffering h (BlockBuffering (Just 1024))
            return h
          )
          (\h -> Safe.try @IO @Catch.SomeException (hFlush h) >> hClose h)
          (\h -> ioE (writeToIoHandle h))
