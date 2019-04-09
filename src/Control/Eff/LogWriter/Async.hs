-- | This module only exposes a 'LogWriter' for asynchronous logging;
module Control.Eff.LogWriter.Async
  ( withAsyncLogWriter
  , withAsyncLogging
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Eff as Eff
import Control.Eff.Log
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Data.Foldable (traverse_)
import Data.Kind ()
import Data.Text as T


-- | This is a wrapper around 'withAsyncLogWriter' and 'withIoLogging'.
--
-- Example:
--
-- > exampleWithAsyncLogging :: IO ()
-- > exampleWithAsyncLogging =
-- >     runLift
-- >   $ withAsyncLogWriter consoleLogWriter (1000::Int) "my-app" local0 allLogMessages
-- >   $ do logMsg "test 1"
-- >        logMsg "test 2"
-- >        logMsg "test 3"
-- >
--
withAsyncLogging ::
     ( LogsTo IO e
     , Lifted IO e
     , MonadBaseControl IO (Eff e)
     , Integral len
     )
  => LogWriter IO
  -> len -- ^ Size of the log message input queue. If the queue is full, message
         -- are dropped silently.
  -> Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader IO : e) a
  -> Eff e a
withAsyncLogging lw queueLength a f p e =
  liftBaseOp
    (withAsyncLogChannel queueLength (runLogWriter lw . force))
    (\lc -> withIoLogging (makeLogChannelWriter lc) a f p e)


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
-- >   $ do logMsg "test 1"
-- >        logMsg "test 2"
-- >        logMsg "test 3"
-- >
--
withAsyncLogWriter ::
     ( LogsTo IO e
     , Lifted IO e
     , MonadBaseControl IO (Eff e)
     , Integral len
     )
  => len -- ^ Size of the log message input queue. If the queue is full, message
         -- are dropped silently.
  -> Eff e a
  -> Eff e a
withAsyncLogWriter queueLength e = do
  lw <- askLogWriter
  liftBaseOp
    (withAsyncLogChannel queueLength (runLogWriter lw . force))
    (\lc -> setLogWriter (makeLogChannelWriter lc) e)

withAsyncLogChannel ::
     forall a len. (Integral len)
  => len
  -> (LogMessage -> IO ())
  -> (LogChannel -> IO a)
  -> IO a
withAsyncLogChannel queueLen ioWriter action = do
  msgQ <- newTBQueueIO (fromIntegral queueLen)
  withAsync (logLoop msgQ) (action . ConcurrentLogChannel msgQ)
  where
    logLoop tq = do
      ms <-
        atomically $ do
          h <- readTBQueue tq
          t <- flushTBQueue tq
          return (h : t)
      traverse_ ioWriter ms
      logLoop tq

makeLogChannelWriter :: LogChannel -> LogWriter IO
makeLogChannelWriter lc = ioLogWriter logChannelPutIO
  where
    logChannelPutIO (force -> me) = do
      !m <- evaluate me
      atomically
        (do dropMessage <- isFullTBQueue logQ
            unless dropMessage (writeTBQueue logQ m))
    logQ = fromLogChannel lc

data LogChannel = ConcurrentLogChannel
  { fromLogChannel :: TBQueue LogMessage
  , _logChannelThread :: Async ()
  }