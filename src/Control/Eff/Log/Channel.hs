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
import           Control.Eff.Lift
import           Control.Exception              ( evaluate )
import           Control.Monad                  ( void
                                                , unless
                                                )
import           Control.Eff.Log.Handler
import           Data.Foldable                  ( traverse_ )
import           Data.Kind                      ( )
import           Control.DeepSeq
import           GHC.Stack

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
  :: forall message a
   . (NFData message)
  => Int -- ^ Size of the log message input queue. If the queue is full, message
        -- are dropped silently.
  -> LogWriter message IO -- ^ An IO action to write the log messages
  -> (LogChannel message -> IO a)
  -> IO a
withAsyncLogChannel queueLen ioWriter action = do
  msgQ <- newTBQueueIO (fromIntegral @Int queueLen)
  withAsync (logLoop msgQ) (action . ConcurrentLogChannel msgQ)
 where
  logLoop tq = do
    ms <- atomically $ do
      h <- readTBQueue tq
      t <- flushTBQueue tq
      return (h : t)
    writeAllLogMessages ioWriter ms
    logLoop tq

-- | Fork an IO based log writer thread and set the 'LogWriter' to an action
-- that will send all logs to that thread via a bounded queue.
-- When the queue is full, flush it
handleLoggingAndIO
  :: (NFData m, HasCallStack)
  => Eff '[Logs m, LogWriterReader m IO, Lift IO] a
  -> LogChannel m
  -> IO a
handleLoggingAndIO e lc = runLift
  (writeLogs (foldingLogWriter (traverse_ logChannelPutIO)) e)
 where
  logQ = fromLogChannel lc
  logChannelPutIO (force -> me) = do
    !m <- evaluate me
    atomically
      (do
        dropMessage <- isFullTBQueue logQ
        unless dropMessage (writeTBQueue logQ m)
      )

-- | Like 'handleLoggingAndIO' but return @()@.
handleLoggingAndIO_
  :: (NFData m, HasCallStack)
  => Eff '[Logs m, LogWriterReader m IO, Lift IO] a
  -> LogChannel m
  -> IO ()
handleLoggingAndIO_ e lc = void (handleLoggingAndIO e lc)

-- | A log channel processes logs from the 'Logs' effect by en-queuing them in a
-- shared queue read from a seperate processes. A channel can contain log
-- message filters.
data LogChannel message =
   ConcurrentLogChannel
   { fromLogChannel :: TBQueue message
   , _logChannelThread :: Async ()
   }
   -- ^ send all log messages to a log process
