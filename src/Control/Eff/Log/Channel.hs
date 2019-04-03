-- | Concurrent Logging
module Control.Eff.Log.Channel
  ( LogChannel()
  , withAsyncLogChannel
  , handleLoggingAndIO
  , handleLoggingAndIO_
  )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Eff                   as Eff
import           Control.Exception              ( evaluate )
import           Control.Monad                  ( void
                                                , unless
                                                )
import           Control.Eff.Log.Handler
import           Control.Eff.Log.Message
import           Control.Eff.Log.Writer
import           Data.Foldable                  ( traverse_ )
import           Data.Kind                      ( )
import           Control.DeepSeq
import           GHC.Stack
import           Debug.Trace

-- | Fork a new process in which the given log message writer, will listen
-- on a message queue in a 'LogChannel', which is passed to the second function.
-- If the function returns or throws, the logging process will be killed.
--
-- Log messages are deeply evaluated before being sent to the logger process,
-- to prevent that lazy evaluation leads to heavy work being done in the
-- logger process instead of the caller process.
--
-- Example usage, a super stupid log to file:
--
-- >
-- > main =
-- >   withAsyncLogChannel
-- >      1000
-- >      (singleMessageLogWriter putStrLn)
-- >      (handleLoggingAndIO
-- >        (do logMsg "test 1"
-- >            logMsg "test 2"
-- >            logMsg "test 3"))
-- >
--
withAsyncLogChannel
  :: forall a len
   . (Integral len)
  => len -- ^ Size of the log message input queue. If the queue is full, message
         -- are dropped silently.
  -> LogWriter IO -- ^ An IO action to write the log messages
  -> (LogChannel -> IO a)
  -> IO a
withAsyncLogChannel queueLen ioWriter action = do
  traceShowM "C - 0000"
  msgQ <- newTBQueueIO (fromIntegral queueLen)
  traceShowM "C - 0001"
  withAsync (logLoop msgQ) (action . ConcurrentLogChannel msgQ)
 where
  logLoop tq = do
    traceShowM "C - 0002"
    ms <- atomically $ do
      traceShowM "C - 0003"
      h <- readTBQueue tq
      traceShowM "C - 0004"
      t <- flushTBQueue tq
      traceShowM "C - 0005"
      return (h : t)
    traceShowM "C - 0006"
    traverse_ (runLogWriter ioWriter) ms
    traceShowM "C - 0007"
    logLoop tq

-- | Fork an IO based log writer thread and set the 'LogWriter' to an action
-- that will send all logs to that thread via a bounded queue.
-- When the queue is full, flush it
handleLoggingAndIO
  :: HasCallStack
  => Eff '[Logs IO, Lift IO] a
  -> LogChannel
  -> IO a
handleLoggingAndIO e lc = runLift
  (runLogs (MkLogWriter logChannelPutIO) e)
 where
  logQ = fromLogChannel lc
  logChannelPutIO (force -> me) = do
    traceShowM "D - 0000"
    !m <- evaluate me
    traceShowM "D - 0001"
    atomically
      (do
        traceShowM "D - 0002"
        dropMessage <- isFullTBQueue logQ
        traceShowM "D - 0003"
        unless dropMessage (writeTBQueue logQ m)
        traceShowM ("D - 0004", dropMessage, renderRFC5424 m)
      )

-- | Like 'handleLoggingAndIO' but return @()@.
handleLoggingAndIO_
  :: HasCallStack
  => Eff '[Logs IO, Lift IO] a
  -> LogChannel
  -> IO ()
handleLoggingAndIO_ e lc = void (handleLoggingAndIO e lc)

-- | A log channel processes logs from the 'Logs' effect by en-queuing them in a
-- shared queue read from a separate processes. A channel can contain log
-- message filters.
data LogChannel =
   ConcurrentLogChannel
   { fromLogChannel :: TBQueue LogMessage
   , _logChannelThread :: Async ()
   }
   -- ^ send all log messages to a log process
