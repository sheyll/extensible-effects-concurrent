-- | Asynchronous Logging
module Control.Eff.Log.Channel
  ( withAsyncLogging
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Eff as Eff
import Control.Eff.Log.Handler
import Control.Eff.Log.Message
import Control.Eff.Log.Writer
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Data.Foldable (traverse_)
import Data.Kind ()

-- | Fork a new process in which the given log message writer, will listen
-- on a message queue, to which all log message will be relayed.
--
-- If an exception is received, the logging process will be killed.
--
-- Log messages are deeply evaluated before being sent to the logger process,
-- to prevent that lazy evaluation leads to heavy work being done in the
-- logger process instead of the caller process.
--
-- Example:
--
-- > exampleAsyncLogging :: IO ()
-- > exampleAsyncLogging =
-- >     runLift
-- >   $ withSomeLogging @IO
-- >   $ withAsyncLogging (1000::Int) consoleLogWriter
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
  => len -- ^ Size of the log message input queue. If the queue is full, message
         -- are dropped silently.
  -> LogWriter IO
  -> Eff e a
  -> Eff e a
withAsyncLogging queueLength lw e =
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