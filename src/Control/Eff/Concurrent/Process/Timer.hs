-- | Functions for timeouts when receiving messages.
--
-- NOTE: If you use a single threaded scheduler, these functions will not work
-- as expected. (This is an open TODO)
--
-- @since 0.12.0
module Control.Eff.Concurrent.Process.Timer
  ( Timeout(TimeoutMicros, fromTimeoutMicros)
  , TimerReference()
  , TimerElapsed(fromTimerElapsed)
  , sendAfter
  , startTimer
  , sendAfterWithTitle
  , startTimerWithTitle
  , cancelTimer
  , selectTimerElapsed
  , receiveAfter
  , receiveSelectedAfter
  , receiveSelectedWithMonitorAfter
  )
   -- , receiveSelectedAfter, receiveAnyAfter, sendMessageAfter)
where

import           Control.Eff.Concurrent.Process
import           Control.Concurrent
import           Control.Eff
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Typeable
import           Data.Text as T
import           Control.Applicative
import           GHC.Stack


-- | Wait for a message of the given type for the given time. When no message
-- arrives in time, return 'Nothing'. This is based on
-- 'receiveSelectedAfter'.
--
-- @since 0.12.0
receiveAfter
  :: forall a r q
   . ( Lifted IO q
     , HasCallStack
     , HasProcesses r q
     , Typeable a
     , NFData a
     , Show a
     )
  => Timeout
  -> Eff r (Maybe a)
receiveAfter t =
  either (const Nothing) Just <$> receiveSelectedAfter (selectMessage @a) t

-- | Wait for a message of the given type for the given time. When no message
-- arrives in time, return 'Left' 'TimerElapsed'. This is based on
-- 'selectTimerElapsed' and 'startTimer'.
--
-- @since 0.12.0
receiveSelectedAfter
  :: forall a r q
   . ( Lifted IO q
     , HasCallStack
     , HasProcesses r q
     , Show a
     )
  => MessageSelector a
  -> Timeout
  -> Eff r (Either TimerElapsed a)
receiveSelectedAfter sel t = do
  timerRef <- startTimer t
  res      <- receiveSelectedMessage
    (Left <$> selectTimerElapsed timerRef <|> Right <$> sel)
  cancelTimer timerRef
  return res

-- | Like 'receiveWithMonitor' combined with 'receiveSelectedAfter'.
--
-- @since 0.22.0
receiveSelectedWithMonitorAfter
  :: forall a r q
   . ( Lifted IO q
     , HasCallStack
     , HasProcesses r q
     , Show a
     )
  => ProcessId
  -> MessageSelector a
  -> Timeout
  -> Eff r (Either (Either ProcessDown TimerElapsed) a)
receiveSelectedWithMonitorAfter pid sel t = do
  timerRef <- startTimer t
  res      <- withMonitor pid $ \pidMon -> do
                receiveSelectedMessage
                  (   Left . Left  <$> selectProcessDown pidMon
                  <|> Left . Right <$> selectTimerElapsed timerRef
                  <|> Right        <$> sel
                  )
  cancelTimer timerRef
  return res

-- | A 'MessageSelector' matching 'TimerElapsed' messages created by
-- 'startTimer'.
--
-- @since 0.12.0
selectTimerElapsed :: TimerReference -> MessageSelector TimerElapsed
selectTimerElapsed timerRef =
  filterMessage (\(TimerElapsed timerRefIn) -> timerRef == timerRefIn)


-- | A number of micro seconds.
--
-- @since 0.12.0
newtype Timeout = TimeoutMicros {fromTimeoutMicros :: Int}
  deriving (NFData, Ord,Eq, Num, Integral, Real, Enum, Typeable)

instance Show Timeout where
  showsPrec d (TimeoutMicros t) =
    showParen (d >= 10) (showString "timeout: " . shows t . showString " µs")

-- | The reference to a timer started by 'startTimer', required to stop
-- a timer via 'cancelTimer'.
--
-- @since 0.12.0
newtype TimerReference = TimerReference ProcessId
  deriving (NFData, Ord,Eq, Num, Integral, Real, Enum, Typeable)

instance Show TimerReference where
  showsPrec d (TimerReference t) =
    showParen (d >= 10) (showString "timer: " . shows t)

-- | A value to be sent when timer started with 'startTimer' has elapsed.
--
-- @since 0.12.0
newtype TimerElapsed = TimerElapsed {fromTimerElapsed :: TimerReference}
  deriving (NFData, Ord,Eq, Typeable)

instance Show TimerElapsed where
  showsPrec d (TimerElapsed t) =
    showParen (d >= 10) (shows t . showString " elapsed")--

-- | Send a message to a given process after waiting. The message is created by
-- applying the function parameter to the 'TimerReference', such that the
-- message can directly refer to the timer.
--
-- @since 0.12.0
sendAfter
  :: forall r q message
   . ( Lifted IO q
     , HasCallStack
     , HasProcesses r q
     , Typeable message
     , NFData message
     )
  => ProcessId
  -> Timeout
  -> (TimerReference -> message)
  -> Eff r TimerReference
sendAfter pid t@(TimeoutMicros us) mkMsg =
  sendAfterWithTitle
    (MkProcessTitle ("after_" <> T.pack (show us) <> "us_to_" <> T.pack (show pid)))
    pid
    t
    mkMsg

-- | Like 'sendAfter' but with a user provided name for the timer process.
--
-- @since 0.30.0
sendAfterWithTitle
  :: forall r q message
   . ( Lifted IO q
     , HasCallStack
     , HasProcesses r q
     , Typeable message
     , NFData message
     )
  => ProcessTitle
  -> ProcessId
  -> Timeout
  -> (TimerReference -> message)
  -> Eff r TimerReference
sendAfterWithTitle title pid (TimeoutMicros us) mkMsg =
  TimerReference <$>
  spawn
    title
    ((if us == 0
        then yieldProcess
        else liftIO (threadDelay us)) >>
     self >>=
     (sendMessage pid . force . mkMsg . TimerReference))

-- | Start a new timer, after the time has elapsed, 'TimerElapsed' is sent to
-- calling process. The message also contains the 'TimerReference' returned by
-- this function. Use 'cancelTimer' to cancel the timer. Use
-- 'selectTimerElapsed' to receive the message using 'receiveSelectedMessage'.
-- To receive messages with guarded with a timeout see 'receiveAfter'.
--
-- This calls 'sendAfterWithTitle' under the hood with 'TimerElapsed' as
-- message.
--
-- @since 0.30.0
startTimerWithTitle
  :: forall r q
   . ( Lifted IO q
     , HasCallStack
     , HasProcesses r q
     )
  => ProcessTitle
  -> Timeout
  -> Eff r TimerReference -- TODO add a parameter to the TimerReference
startTimerWithTitle title t = do
  p <- self
  sendAfterWithTitle title p t TimerElapsed

-- | Start a new timer, after the time has elapsed, 'TimerElapsed' is sent to
-- calling process. The message also contains the 'TimerReference' returned by
-- this function. Use 'cancelTimer' to cancel the timer. Use
-- 'selectTimerElapsed' to receive the message using 'receiveSelectedMessage'.
-- To receive messages with guarded with a timeout see 'receiveAfter'.
--
-- Calls 'sendAfter' under the hood.
--
-- @since 0.12.0
startTimer
  :: forall r q
   . ( Lifted IO q
     , HasCallStack
     , HasProcesses r q
     )
  => Timeout
  -> Eff r TimerReference -- TODO add a parameter to the TimerReference
startTimer t = do
  p <- self
  sendAfter p t TimerElapsed

-- | Cancel a timer started with 'startTimer'.
--
-- @since 0.12.0
cancelTimer
  :: forall r q
   . ( Lifted IO q
     , HasCallStack
     , HasProcesses r q
     )
  => TimerReference
  -> Eff r ()
cancelTimer (TimerReference tr) = sendShutdown tr ExitNormally
