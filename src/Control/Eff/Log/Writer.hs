{-# LANGUAGE UndecidableInstances #-}
module Control.Eff.Log.Writer
  ( LogWriter(MkLogWriter, runLogWriter)
  , runLogWriterLifted
  , ioLogMessageWriter
  , LiftsLogWriter(..)
  , discardLogMessages
  , DiscardLogs(..)
  , traceLogMessages
  , TraceLogs(..)
  , captureLogMessages
  , CaptureLogs(..)
  , runLogMessageListWriter
  ) where

import Control.Eff
import Control.Eff.Log.Message
import Control.Monad ((>=>))
import Data.Default
import Debug.Trace
import GHC.Stack
import Control.Eff.Writer.Strict (Writer, tell, runListWriter)
import Data.Functor.Identity (Identity)
import Control.DeepSeq (deepseq)
import Data.Foldable (traverse_)

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter writerM = MkLogWriter
  { runLogWriter :: LogMessage -> writerM ()
  }

-- | Write a log message by lifting and executing the given 'LogWriter'.
runLogWriterLifted :: Lifted writerM e => LogWriter writerM -> LogMessage -> Eff e ()
runLogWriterLifted w m = lift (runLogWriter w m)

instance Applicative w => Default (LogWriter w) where
  def = MkLogWriter (const (pure ()))

-- * LogWriter liftings

-- | This class describes how to lift the log writer action into some monad.
--
-- The second parameter is almost always @Eff x@
-- so usually the method of this class lifts the log writer action into an effect monad.
class LiftsLogWriter h e where
  liftLogWriter :: LogWriter h -> LogMessage -> Eff e ()

-- ** 'IO' based 'LogWriter's

-- | Set a timestamp (if not set), the thread id (if not set) using IO actions
-- then /write/ the log message using the 'IO' and 'String' based 'LogWriter'.
ioLogMessageWriter :: HasCallStack => (String -> IO ()) -> LogWriter IO
ioLogMessageWriter delegatee = MkLogWriter
  (   setLogMessageTimestamp
  >=> setLogMessageThreadId
  >=> (delegatee . renderLogMessage)
  )

instance (Lifted IO e) => LiftsLogWriter IO e where
  liftLogWriter = (lift . ) . runLogWriter

-- * LogWriter variants

-- ** Pure Log Writers

-- | A 'LogWriter' that simply discards the messages.
discardLogMessages :: LogWriter DiscardLogs
discardLogMessages = MkLogWriter (const (return ()))

-- | The monad for 'traceLogMessages'.
-- This is just a wrapper around 'Identity' and serves as a type that has a special
-- 'LiftsLogWriter' instance.
newtype DiscardLogs a = MkDiscardLogs { runDiscardLogs :: Identity a }
  deriving (Functor, Applicative, Monad)

-- | A 'LogWriter' monad discarding all messages.
instance LiftsLogWriter DiscardLogs e where
  liftLogWriter _ = const (return ())


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
newtype CaptureLogs a = MkCaptureLogs { unCaptureLogs :: Eff '[Writer LogMessage] a }
  deriving (Functor, Applicative, Monad)

-- | A 'LogWriter' monad for pure logging.
--
-- The 'LiftsLogWriter' instance for this type assumes a 'Writer' effect.
instance Member (Writer LogMessage) e => LiftsLogWriter CaptureLogs e where
  liftLogWriter lw = traverse_ (tell @LogMessage) . snd . run . runListWriter . unCaptureLogs . runLogWriter lw

-- | Run a 'Writer' for 'LogMessage's.
--
-- Such a 'Writer' is needed for 'CaptureLogs'
runLogMessageListWriter :: Eff (Writer LogMessage ': e) a -> Eff e (a, [LogMessage])
runLogMessageListWriter = runListWriter