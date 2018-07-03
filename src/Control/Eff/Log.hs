{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
-- | A logging effect based on 'Control.Monad.Log.MonadLog'.
module Control.Eff.Log
  (
    -- * Logging Effect
    Logs(..)
  , logMsg
  , foldLog
  , foldLogFast
  , module ExtLog
  , captureLogs
  , ignoreLogs
  , handleLogsWith
    -- * Concurrent Logging
  , LogChannel()
  , logToChannel
  , noLogger
  , forkLogger
  , filterLogChannel
  , joinLogChannel
  , killLogChannel
  , closeLogChannelAfter
  , logChannelBracket
  , logChannelPutIO
  )
where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Eff                   as Eff
import           Control.Eff.Extend            as Eff
import           Control.Exception              ( bracket )
import qualified Control.Exception             as Exc
import           Control.Monad                  ( void
                                                , when
                                                , unless
                                                )
import           Control.Monad.Log             as ExtLog
                                         hiding ( )
import           Control.Monad.Trans.Control
import qualified Control.Eff.Lift              as Eff
import qualified Control.Monad.Log             as Log
import           Data.Foldable                  ( traverse_ )
import           Data.Kind                      ( )
import           Data.Sequence                  ( Seq() )
import qualified Data.Sequence                 as Seq
import           Data.String
import           Data.Typeable

-- | Logging effect type, parameterized by a log message type.
data Logs message a where
  LogMsg :: message -> Logs message ()

-- | Log a message.
logMsg :: Member (Logs m) r => m -> Eff r ()
logMsg msg = send (LogMsg msg)

-- | Change, add or remove log messages.
--
-- Requirements:
--
--   * All log meta data for typical prod code can be added without
--     changing much of the code
--
--   * Add timestamp to a log messages of a sub-computation.
--
--   * Write some messages to a file.
--
--   * Log something extra, e.g. runtime memory usage in load tests
--
-- Approach: Install a callback that sneaks into to log message
-- sending/receiving, to intercept the messages and execute some code and then
-- return a new message.
foldLog
  :: forall r m a . Member (Logs m) r => (m -> Eff r ()) -> Eff r a -> Eff r a
foldLog interceptor effect = interpose return go effect
 where
  go :: Member (Logs m) r => Logs m x -> (Arr r x y) -> Eff r y
  go (LogMsg m) k = do
    interceptor m
    k ()

-- | Change, add or remove log messages without side effects, faster than
-- 'foldLog'.
--
-- Requirements:
--
--  * Tests run fast in unit tests so travis won't time out
--
--  * Drop debug logs
--
--  * /Grep like/ log filtering
--
-- Approach: Install a callback that sneaks into to log message
-- sending/receiving, to intercept the messages and execute some code and then
-- return a new message.
foldLogFast
  :: forall r m a f
   . (Foldable f, Member (Logs m) r)
  => (m -> f m)
  -> Eff r a
  -> Eff r a
foldLogFast interceptor effect = interpose return go effect
 where
  go :: Member (Logs m) r => Logs m x -> (Arr r x y) -> Eff r y
  go (LogMsg m) k = do
    traverse_ logMsg (interceptor m)
    k ()

-- | Capture the logs in a 'Seq'.
captureLogs :: Eff (Logs message ': r) a -> Eff r (a, Seq message)
captureLogs actionThatLogs = Eff.handle_relay_s
  Seq.empty
  (\logs result -> return (result, logs))
  handleLogs
  actionThatLogs
 where
  handleLogs
    :: Seq message -> Logs message x -> (Seq message -> Arr r x y) -> Eff r y
  handleLogs logs (LogMsg m) k = k (logs Seq.:|> m) ()

-- | Throw away all log messages.
ignoreLogs :: forall message r a . Eff (Logs message ': r) a -> Eff r a
ignoreLogs actionThatLogs = Eff.handle_relay return handleLogs actionThatLogs
 where
  handleLogs :: Logs m x -> Arr r x y -> Eff r y
  handleLogs (LogMsg _) k = k ()

-- | Handle 'Logs' effects using 'Log.LoggingT' 'Log.Handler's.
handleLogsWith
  :: forall m r message a
   . (Monad m, SetMember Eff.Lift (Eff.Lift m) r)
  => Eff (Logs message ': r) a
  -> (forall b . (Log.Handler m message -> m b) -> m b)
  -> Eff r a
handleLogsWith actionThatLogs foldHandler = Eff.handle_relay return
                                                             go
                                                             actionThatLogs
 where
  go :: Logs message b -> (b -> Eff r c) -> Eff r c
  go (LogMsg m) k = Eff.lift (foldHandler (\doLog -> doLog m)) >>= k

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
logToChannel logChan actionThatLogs = do
  handleLogsWith actionThatLogs
                 (\withHandler -> withHandler (logChannelPutIO logChan))

-- | Enqueue a log message into a log channel
logChannelPutIO :: LogChannel message -> message -> IO ()
logChannelPutIO DiscardLogs               _ = return ()
logChannelPutIO (FilteredLogChannel f lc) m = when (f m) (logChannelPutIO lc m)
logChannelPutIO c                         m = atomically $ do
  dropMessage <- isFullTBQueue (fromLogChannel c)
  unless dropMessage (writeTBQueue (fromLogChannel c) m)

-- | Create a 'LogChannel' that will discard all messages sent
-- via 'forwardLogstochannel' or 'logChannelPutIO'.
noLogger :: LogChannel message
noLogger = DiscardLogs

-- | Fork a new process, that applies a monadic action to all log messages sent
-- via 'logToChannel' or 'logChannelPutIO'.
forkLogger
  :: forall message
   . (Typeable message, Show message)
  => Int -- ^ Size of the log message input queue. If the queue is full, message
        -- are dropped silently.
  -> (message -> IO ()) -- ^ An IO action to log the messages
  -> Maybe message -- ^ Optional __first__ message to log
  -> IO (LogChannel message)
forkLogger queueLen handle mFirstMsg = do
  msgQ <- atomically
    (do
      tq <- newTBQueue queueLen
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
        Just (JoinLogChannelException mCloseMsg) -> do
          traverse_ handle logMessages
          traverse_ handle mCloseMsg
        Nothing -> case Exc.fromException se of
          Just (KillLogChannelException mCloseMsg) ->
            traverse_ handle mCloseMsg
          Nothing -> mapM_ handle logMessages

  logLoop :: TBQueue message -> IO ()
  logLoop tq = do
    m <- atomically $ readTBQueue tq
    handle m
    logLoop tq

-- | Filter logs sent to a 'LogChannel' using a predicate.
filterLogChannel
  :: (message -> Bool) -> LogChannel message -> LogChannel message
filterLogChannel = FilteredLogChannel

-- | Run an action and close a 'LogChannel' created by 'noLogger', 'forkLogger'
-- or 'filterLogChannel' afterwards using 'joinLogChannel'. If a
-- 'Exc.SomeException' was thrown, the log channel is killed with
-- 'killLogChannel', and the exception is re-thrown.
closeLogChannelAfter
  :: (Show message, Typeable message, IsString message)
  => Maybe message
  -> LogChannel message
  -> IO a
  -> IO a
closeLogChannelAfter mGoodbye logC ioAction = do
  res <- closeLogAndRethrow `Exc.handle` ioAction
  closeLogSuccess
  return res
 where
  closeLogAndRethrow :: Exc.SomeException -> IO a
  closeLogAndRethrow se = do
    let closeMsg = Just (fromString (Exc.displayException se))
    void $ Exc.try @Exc.SomeException $ killLogChannel closeMsg logC
    Exc.throw se

  closeLogSuccess :: IO ()
  closeLogSuccess = joinLogChannel mGoodbye logC

-- | Close a log channel created by e.g. 'forkLogger'. Message already enqueue
-- are handled, as well as an optional final message. Subsequent log message
-- will not be handled anymore. If the log channel must be closed immediately,
-- use 'killLogChannel' instead.
joinLogChannel
  :: (Show message, Typeable message)
  => Maybe message
  -> LogChannel message
  -> IO ()
joinLogChannel _closeLogMessage DiscardLogs = return ()
joinLogChannel Nothing (FilteredLogChannel _f lc) = joinLogChannel Nothing lc
joinLogChannel (Just closeLogMessage) (FilteredLogChannel f lc) =
  if f closeLogMessage
    then joinLogChannel (Just closeLogMessage) lc
    else joinLogChannel Nothing lc
joinLogChannel closeLogMessage (ConcurrentLogChannel _tq thread) = do
  throwTo thread (JoinLogChannelException closeLogMessage)

-- | Close a log channel quickly, without logging messages already in the queue.
-- Subsequent logging requests will not be handled anymore. If the log channel
-- must be closed without loosing any messages, use 'joinLogChannel' instead.
killLogChannel
  :: (Show message, Typeable message)
  => Maybe message
  -> LogChannel message
  -> IO ()
killLogChannel _closeLogMessage DiscardLogs = return ()
killLogChannel Nothing (FilteredLogChannel _f lc) = killLogChannel Nothing lc
killLogChannel (Just closeLogMessage) (FilteredLogChannel f lc) =
  if f closeLogMessage
    then killLogChannel (Just closeLogMessage) lc
    else killLogChannel Nothing lc
killLogChannel closeLogMessage (ConcurrentLogChannel _tq thread) =
  throwTo thread (KillLogChannelException closeLogMessage)

-- | Internal exception to shutdown a 'LogChannel' process created by
-- 'forkLogger'. This exception is handled such that all message already
-- en-queued are handled and then an optional final message is written.
newtype JoinLogChannelException m = JoinLogChannelException (Maybe m)
  deriving (Show, Typeable)

instance (Typeable m, Show m) => Exc.Exception (JoinLogChannelException m)

-- | Internal exception to **immediately** shutdown a 'LogChannel' process
-- created by 'forkLogger', other than 'JoinLogChannelException' the message queue
-- will not be flushed, not further messages will be logged, except for the
-- optional final message.
newtype KillLogChannelException m = KillLogChannelException (Maybe m)
  deriving (Show, Typeable)

instance (Typeable m, Show m) => Exc.Exception (KillLogChannelException m)

-- | Wrap 'LogChannel' creation and destruction around a monad action in
-- 'bracket'y manner. This function uses 'joinLogChannel', so en-queued messages
-- are flushed on exit. The resulting action in in the 'LoggingT' monad, which
-- is essentially a reader for the log handler function.
logChannelBracket
  :: (Show message, Typeable message)
  => Int -- ^ Size of the log message input queue. If the queue is full, message
        -- are dropped silently.
  -> Maybe message -- ^ Optional __first__ message to log
  -> Maybe message -- ^ Optional __last__ message to log
  -> (LogChannel message -> IO a) -- ^ An IO action that will use the
                                -- 'LogChannel', after the action returns (even
                                -- because of an exception) the log channel is
                                -- destroyed.
  -> LoggingT message IO a
logChannelBracket queueLen mWelcome mGoodbye f = control
  (\runInIO -> do
    let logHandler = void . runInIO . logMessage
    bracket (forkLogger queueLen logHandler mWelcome)
            (joinLogChannel mGoodbye)
            f
  )
