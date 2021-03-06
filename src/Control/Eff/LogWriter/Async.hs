-- | This module only exposes a 'LogWriter' for asynchronous logging;
module Control.Eff.LogWriter.Async
  ( withAsyncLogWriter,
    withAsyncLogging,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Eff as Eff
import Control.Eff.Log
import Control.Eff.LogWriter.Rich
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.Trans.Control
  ( MonadBaseControl,
    liftBaseOp,
  )
import Data.Foldable (traverse_)
import Data.Kind ()

-- | This is a wrapper around 'withAsyncLogWriter' and 'withRichLogging'.
--
-- Example:
--
-- > exampleWithAsyncLogging :: IO ()
-- > exampleWithAsyncLogging =
-- >     runLift
-- >   $ withAsyncLogWriter consoleLogWriter (1000::Int) "my-app" local0 allLogEvents
-- >   $ do sendLogEvent "test 1"
-- >        sendLogEvent "test 2"
-- >        sendLogEvent "test 3"
-- >
withAsyncLogging ::
  (Lifted IO e, MonadBaseControl IO (Eff e), Integral len) =>
  LogWriter ->
  -- | Size of the log message input queue. If the queue is full, message
  -- are dropped silently.
  len ->
  -- | The default application name to put into the 'logEventAppName' field.
  String ->
  -- | The default RFC-5424 facility to put into the 'logEventFacility' field.
  Facility ->
  -- | The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  LogPredicate ->
  Eff (Logs : LogWriterReader : e) a ->
  Eff e a
withAsyncLogging lw queueLength a f p e =
  liftBaseOp
    (withAsyncLogChannel queueLength (runLogWriter lw . force))
    (\lc -> withRichLogging (makeLogChannelWriter lc) a f p e)

-- | /Move/ the current 'LogWriter' into its own thread.
--
-- A bounded queue is used to forward logs to the process.
--
-- If an exception is received, the logging process will be killed.
--
-- Log messages are deeply evaluated before being sent to the logger process,
-- to prevent that lazy evaluation leads to heavy work being done in the
-- logger process instead of the caller process.
--
-- Example:
--
-- > exampleAsyncLogWriter :: IO ()
-- > exampleAsyncLogWriter =
-- >     runLift
-- >   $ withLogging consoleLogWriter
-- >   $ withAsyncLogWriter (1000::Int)
-- >   $ do sendLogEvent "test 1"
-- >        sendLogEvent "test 2"
-- >        sendLogEvent "test 3"
-- >
withAsyncLogWriter ::
  (IoLogging e, MonadBaseControl IO (Eff e), Integral len) =>
  -- | Size of the log message input queue. If the queue is full, message
  -- are dropped silently.
  len ->
  Eff e a ->
  Eff e a
withAsyncLogWriter queueLength e = do
  lw <- askLogWriter
  liftBaseOp
    (withAsyncLogChannel queueLength (runLogWriter lw . force))
    (\lc -> setLogWriter (makeLogChannelWriter lc) e)

withAsyncLogChannel ::
  forall a len.
  (Integral len) =>
  len ->
  (LogEvent -> IO ()) ->
  (LogChannel -> IO a) ->
  IO a
withAsyncLogChannel queueLen ioWriter action = do
  msgQ <- newTBQueueIO (fromIntegral queueLen)
  withAsync (logLoop msgQ) (action . ConcurrentLogChannel msgQ)
  where
    logLoop tq = do
      ms <- atomically $ do
        isEmpty <- isEmptyTBQueue tq
        when isEmpty retry
        flushTBQueue tq
      traverse_ ioWriter ms
      logLoop tq

makeLogChannelWriter :: LogChannel -> LogWriter
makeLogChannelWriter lc = MkLogWriter logChannelPutIO
  where
    logChannelPutIO (force -> me) = do
      !m <- evaluate me
      isFull <-
        atomically
          ( if m ^. logEventSeverity <= warningSeverity
              then do
                writeTBQueue logQ m
                return False
              else do
                isFull <- isFullTBQueue logQ
                unless isFull (writeTBQueue logQ m)
                return isFull
          )
      when isFull $
        threadDelay 1_000
    logQ = fromLogChannel lc

data LogChannel = ConcurrentLogChannel
  { fromLogChannel :: TBQueue LogEvent,
    _logChannelThread :: Async ()
  }
