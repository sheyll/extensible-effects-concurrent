{-# LANGUAGE UndecidableInstances #-}
-- | The 'LogWriter' type encapsulates an effectful function to write 'LogMessage's.
--
-- Used in conjunction with the 'SupportsLogger' class, it
-- can be used to write messages from within an effectful
-- computation.
module Control.Eff.Log.Writer
  ( -- * 'LogWriter' Definition
    LogWriter(MkLogWriter, runLogWriter)
  , SupportsLogger(..)
  -- ** 'LogWriter' Zoo
  -- *** Pure Writer
  , noOpLogWriter
  , debugTraceLogWriter
  , PureLogWriter(..)
  , listLogWriter
  , CaptureLogs(..)
  , CapturedLogsWriter
  , runCapturedLogsWriter
  -- ** IO Based 'LogWriter'
  , consoleLogWriter
  , ioHandleLogWriter
  -- *** General Combinator
  , filteringLogWriter
  , mappingLogWriter
  , mappingLogWriterM
  -- *** IO Based Combinator
  , ioLogWriter
  , defaultIoLogWriter
  ) where

import Control.Eff
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
import Control.Lens

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter writerM = MkLogWriter
  { runLogWriter :: LogMessage -> writerM ()
  }

instance Applicative w => Default (LogWriter w) where
  def = MkLogWriter (const (pure ()))

-- * LogWriter liftings

-- | This class describes how to lift the log writer action into some monad.
--
-- The second parameter is almost always @Eff x@
-- so usually the method of this class lifts the log writer action into an effect monad.
class SupportsLogger h e where
  liftLogWriter :: LogWriter h -> LogMessage -> Eff e ()

-- * 'LogWriter' Zoo

-- ** Pure Log Writers

-- | A base monad for all side effect free 'LogWriter'.
--
-- This is only required e.g. when logs are only either discarded or traced.
-- See 'debugTraceLogWriter' or 'noOpLogWriter'.
--
-- This is just a wrapper around 'Identity' and serves as a type that has a special
-- 'SupportsLogger' instance.
newtype PureLogWriter a = MkPureLogWriter { runPureLogWriter :: Identity a }
  deriving (Functor, Applicative, Monad)

-- | A 'LogWriter' monad for 'Debug.Trace' based pure logging.
instance SupportsLogger PureLogWriter e where
  liftLogWriter lw msg = deepseq (runPureLogWriter (runLogWriter lw msg)) (return ())

-- | This 'LogWriter' will discard all messages.
--
-- NOTE: This is just an alias for 'def'
noOpLogWriter :: Applicative m => LogWriter m
noOpLogWriter = def

-- | A 'LogWriter' that applies 'renderLogMessage' to the log message and then
-- traces it using 'traceM'.
-- This 'LogWriter' work with /any/ base monad.
debugTraceLogWriter :: Monad h => LogWriter h
debugTraceLogWriter = MkLogWriter (traceM . renderLogMessage)

-- ** Impure logging

-- | A 'LogWriter' monad that provides pure logging by capturing via the 'Writer' effect.
listLogWriter :: LogWriter CaptureLogs
listLogWriter = MkLogWriter (MkCaptureLogs . tell)

-- | A 'LogWriter' monad that provides pure logging by capturing via the 'Writer' effect.
newtype CaptureLogs a = MkCaptureLogs { unCaptureLogs :: Eff '[CapturedLogsWriter] a }
  deriving (Functor, Applicative, Monad)

-- | A 'LogWriter' monad for pure logging.
--
-- The 'SupportsLogger' instance for this type assumes a 'Writer' effect.
instance Member CapturedLogsWriter e => SupportsLogger CaptureLogs e where
  liftLogWriter lw = traverse_ (tell @LogMessage) . snd . run . runListWriter . unCaptureLogs . runLogWriter lw

-- | Run a 'Writer' for 'LogMessage's.
--
-- Such a 'Writer' is needed for 'CaptureLogs'
runCapturedLogsWriter :: Eff (CapturedLogsWriter ': e) a -> Eff e (a, [LogMessage])
runCapturedLogsWriter = runListWriter

-- | Alias for the 'Writer' that contains the captured 'LogMessage's from 'CaptureLogs'.
type CapturedLogsWriter = Writer LogMessage

-- | A 'LogWriter' that uses an 'IO' action to write the message.
--
-- Example use cases for this function are the 'consoleLogWriter' and the 'ioHandleLogWriter'.
ioLogWriter :: HasCallStack => (LogMessage-> IO ()) -> LogWriter IO
ioLogWriter = MkLogWriter

-- | A 'LogWriter' that uses an 'IO' action to write the message.
ioHandleLogWriter :: HasCallStack => Handle -> LogWriter IO
ioHandleLogWriter h = ioLogWriter (hPutStrLn h . renderLogMessage)

instance (Lifted IO e) => SupportsLogger IO e where
  liftLogWriter = (lift . ) . runLogWriter

-- | Write 'LogMessage's to standard output, formatted with 'printLogMessage'.
consoleLogWriter :: LogWriter IO
consoleLogWriter = ioLogWriter printLogMessage

-- | Decorate an IO based 'LogWriter' to set important fields in log messages.
--
-- ALl log messages are censored to include basic log message information:
--
-- * The messages will carry the given application name in the 'lmAppName' field.
-- * The 'lmTimestamp' field contains the UTC time of the log event
-- * The 'lmThreadId' field contains the thread-Id
-- * The 'lmHostname' field contains the FQDN of the current host
-- * The 'lmFacility' field contains the given 'Facility'
--
-- It installs the given 'LogWriter', wrapped using 'mappingLogWriterM'.
defaultIoLogWriter :: String -> Facility -> LogWriter IO -> LogWriter IO
defaultIoLogWriter appName facility =
  mappingLogWriterM
    (   setLogMessageThreadId
    >=> setLogMessageTimestamp
    >=> setLogMessageHostname
    >=> pure
         . set lmFacility facility
         . set lmAppName (Just appName)
    )

-- | A 'LogWriter' that applies a predicate to the 'LogMessage' and delegates to
-- to the given writer of the predicate is satisfied.
filteringLogWriter :: Monad e => LogPredicate -> LogWriter e -> LogWriter e
filteringLogWriter p lw = MkLogWriter (\msg -> if p msg then (runLogWriter lw msg) else return ())

-- | A 'LogWriter' that applies a function to the 'LogMessage' and delegates the result to
-- to the given writer.
mappingLogWriter :: (LogMessage -> LogMessage) -> LogWriter e -> LogWriter e
mappingLogWriter f lw = MkLogWriter (runLogWriter lw . f)

-- | Like 'mappingLogWriter' allow the function that changes the 'LogMessage' to have effects.
mappingLogWriterM :: Monad e => (LogMessage -> e LogMessage) -> LogWriter e -> LogWriter e
mappingLogWriterM f lw = MkLogWriter (f >=> runLogWriter lw)
