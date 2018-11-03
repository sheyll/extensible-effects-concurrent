-- | Concurrent Logging
module Control.Eff.Log.Channel
  ( LogChannel()
  , logToChannel
  , nullLogChannel
  , forkLogger
  , filterLogChannel
  , joinLogChannel
  , killLogChannel
  , closeLogChannelAfter
  , logChannelBracket
  , logChannelPutIO
  , JoinLogChannelException()
  , KillLogChannelException()
  , handleLoggingAndIO
  , handleLoggingAndIO_
  )
where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Eff                   as Eff
import           Control.Eff.Lift
import           Control.Exception              ( bracket )
import qualified Control.Exception             as Exc
import           Control.Monad                  ( void
                                                , when
                                                , unless
                                                )
import           Control.Monad.Log             as ExtLog
                                         hiding ( )
import           Control.Monad.Trans.Control
import           Control.Eff.Log.Handler
import qualified Control.Eff.Lift              as Eff
import           Data.Foldable                  ( traverse_ )
import           Data.Kind                      ( )
import           Data.String
import           Data.Typeable
import           GHC.Stack

-- | A log channel processes logs from the 'Logs' effect by en-queuing them in a
-- shared queue read from a seperate processes. A channel can contain log
-- message filters.
data LogChannel message =
   FilteredLogChannel (message -> Bool) (LogChannel message)
   -- ^ filter log messages
 | DiscardLogs
   -- ^ discard all log messages
 | ConcurrentLogChannel
   { fromLogChannel :: TBQueue message
   , _logChannelThread :: ThreadId
   }
   -- ^ send all log messages to a log process

-- | Send the log messages to a 'LogChannel'.
logToChannel
  :: forall r message a
   . (SetMember Eff.Lift (Eff.Lift IO) r)
  => LogChannel message
  -> Eff (Logs message ': r) a
  -> Eff r a
logToChannel logChan =
  handleLogsWithLoggingTHandler ($ logChannelPutIO logChan)

-- | Enqueue a log message into a log channel
logChannelPutIO :: LogChannel message -> message -> IO ()
logChannelPutIO DiscardLogs               _ = return ()
logChannelPutIO (FilteredLogChannel f lc) m = when (f m) (logChannelPutIO lc m)
logChannelPutIO c                         m = atomically $ do
  dropMessage <- isFullTBQueue (fromLogChannel c)
  unless dropMessage (writeTBQueue (fromLogChannel c) m)

-- | Create a 'LogChannel' that will discard all messages sent
-- via 'forwardLogstochannel' or 'logChannelPutIO'.
nullLogChannel :: LogChannel message
nullLogChannel = DiscardLogs

-- | Fork a new process, that applies a monadic action to all log messages sent
-- via 'logToChannel' or 'logChannelPutIO'.
forkLogger
  :: forall message
   . (Typeable message)
  => Int -- ^ Size of the log message input queue. If the queue is full, message
        -- are dropped silently.
  -> (message -> IO ()) -- ^ An IO action to log the messages
  -> Maybe message -- ^ Optional __first__ message to log
  -> IO (LogChannel message)
forkLogger queueLen handle mFirstMsg = do
  msgQ <- atomically
    (do
      tq <- newTBQueue (fromIntegral @Int queueLen)
      mapM_ (writeTBQueue tq) mFirstMsg
      return tq
    )
  thread <- forkFinally (logLoop msgQ) (writeLastLogs msgQ)
  return (ConcurrentLogChannel msgQ thread)
 where
  writeLastLogs :: TBQueue message -> Either Exc.SomeException () -> IO ()
  writeLastLogs tq ee = do
    logMessages <- atomically $ flushTBQueue tq
    case ee of
      Right _  -> return ()
      Left  se -> case Exc.fromException se of
        Just JoinLogChannelException -> traverse_ handle logMessages
        Nothing                      -> case Exc.fromException se of
          Just KillLogChannelException -> return ()
          Nothing                      -> mapM_ handle logMessages

  logLoop :: TBQueue message -> IO ()
  logLoop tq = do
    m <- atomically $ do
      h <- readTBQueue tq
      t <- flushTBQueue tq
      return (h : t)
    traverse_ handle m
    logLoop tq

-- | Filter logs sent to a 'LogChannel' using a predicate.
filterLogChannel
  :: (message -> Bool) -> LogChannel message -> LogChannel message
filterLogChannel = FilteredLogChannel

-- | Run an action and close a 'LogChannel' created by 'nullLogChannel', 'forkLogger'
-- or 'filterLogChannel' afterwards using 'joinLogChannel'. If a
-- 'Exc.SomeException' was thrown, the log channel is killed with
-- 'killLogChannel', and the exception is re-thrown.
closeLogChannelAfter
  :: (Typeable message, IsString message) => LogChannel message -> IO a -> IO a
closeLogChannelAfter logC ioAction = do
  res <- closeLogAndRethrow `Exc.handle` ioAction
  closeLogSuccess
  return res
 where
  closeLogAndRethrow :: Exc.SomeException -> IO a
  closeLogAndRethrow se = do
    void $ Exc.try @Exc.SomeException $ killLogChannel logC
    Exc.throw se

  closeLogSuccess :: IO ()
  closeLogSuccess = joinLogChannel logC

-- | Close a log channel created by e.g. 'forkLogger'. Message already enqueue
-- are handled. Subsequent log message
-- will not be handled anymore. If the log channel must be closed immediately,
-- use 'killLogChannel' instead.
joinLogChannel :: (Typeable message) => LogChannel message -> IO ()
joinLogChannel DiscardLogs                = return ()
joinLogChannel (FilteredLogChannel _f lc) = joinLogChannel lc
joinLogChannel (ConcurrentLogChannel _tq thread) =
  throwTo thread JoinLogChannelException

-- | Close a log channel quickly, without logging messages already in the queue.
-- Subsequent logging requests will not be handled anymore. If the log channel
-- must be closed without loosing any messages, use 'joinLogChannel' instead.
killLogChannel :: (Typeable message) => LogChannel message -> IO ()
killLogChannel DiscardLogs                = return ()
killLogChannel (FilteredLogChannel _f lc) = killLogChannel lc
killLogChannel (ConcurrentLogChannel _tq thread) =
  throwTo thread KillLogChannelException

-- | Internal exception to shutdown a 'LogChannel' process created by
-- 'forkLogger'. This exception is handled such that all message already
-- en-queued are handled and then an optional final message is written.
data JoinLogChannelException = JoinLogChannelException
  deriving (Show, Typeable)

instance Exc.Exception JoinLogChannelException

-- | Internal exception to **immediately** shutdown a 'LogChannel' process
-- created by 'forkLogger', other than 'JoinLogChannelException' the message queue
-- will not be flushed, not further messages will be logged, except for the
-- optional final message.
data KillLogChannelException = KillLogChannelException
  deriving (Show, Typeable)

instance Exc.Exception KillLogChannelException

-- | Wrap 'LogChannel' creation and destruction around a monad action in
-- 'bracket'y manner. This function uses 'joinLogChannel', so en-queued messages
-- are flushed on exit. The resulting action is a 'LoggingT' action, which
-- is essentially a reader for a log handler function in 'IO'.
logChannelBracket
  :: (Typeable message)
  => Int -- ^ Size of the log message input queue. If the queue is full, message
        -- are dropped silently.
  -> Maybe message -- ^ Optional __first__ message to log
  -> (LogChannel message -> IO a) -- ^ An IO action that will use the
                                -- 'LogChannel', after the action returns (even
                                -- because of an exception) the log channel is
                                -- destroyed.
  -> LoggingT message IO a
logChannelBracket queueLen mWelcome f = control
  (\runInIO -> do
    let logHandler = void . runInIO . logMessage
    bracket (forkLogger queueLen logHandler mWelcome) joinLogChannel f
  )

-- | Handle the 'LoggingAndIO' effects, using a 'LogChannel' for the 'Logs'
-- effect.
handleLoggingAndIO
  :: HasCallStack => Eff '[Logs m, Lift IO] a -> LogChannel m -> IO a
handleLoggingAndIO e lc = runLift (logToChannel lc e)

-- | Like 'handleLoggingAndIO' but return @()@.
handleLoggingAndIO_
  :: HasCallStack => Eff '[Logs m, Lift IO] a -> LogChannel m -> IO ()
handleLoggingAndIO_ = ((.) . (.)) void handleLoggingAndIO
