-- | Functions for timeouts when receiving messages.
--
-- NOTE: If you use a single threaded scheduler, these functions will not work
-- as expected. (This is an open TODO)
--
-- @since 0.12.0
module Control.Eff.Concurrent.Process.Timer
  ( Timeout(fromTimeoutMicros)
  , TimerReference()
  , TimerElapsed(fromTimerElapsed)
  , sendAfter
  , startTimer
  , cancelTimer
  , selectTimerElapsed
  , receiveAfter
  , receiveSelectedAfter
  )
   -- , receiveSelectedAfter, receiveAnyAfter, sendMessageAfter)
where

import           Control.Eff.Concurrent.Process
import           Control.Concurrent
import           Control.Eff.Lift
import           Control.Eff
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Typeable
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
     , SetMember Process (Process q) r
     , Member Interrupts r
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
     , SetMember Process (Process q) r
     , Member Interrupts r
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
-- @since 0.12.0


-- | Send a message to a given process after waiting. The message is created by
-- applying the function parameter to the 'TimerReference', such that the
-- message can directly refer to the timer.
--
-- @since 0.12.0
sendAfter
  :: forall r q message
   . ( Lifted IO q
     , HasCallStack
     , SetMember Process (Process q) r
     , Member Interrupts r
     , Typeable message
     , NFData message
     )
  => ProcessId
  -> Timeout
  -> (TimerReference -> message)
  -> Eff r TimerReference
sendAfter pid (TimeoutMicros 0) mkMsg = TimerReference <$> spawn
  (yieldProcess >> self >>= (sendMessage pid . force . mkMsg . TimerReference))
sendAfter pid (TimeoutMicros t) mkMsg = TimerReference <$> spawn
  (   liftIO (threadDelay t)
  >>  self
  >>= (sendMessage pid . force . mkMsg . TimerReference)
  )

-- | Start a new timer, after the time has elapsed, 'TimerElapsed' is sent to
-- calling process. The message also contains the 'TimerReference' returned by
-- this function. Use 'cancelTimer' to cancel the timer. Use
-- 'selectTimerElapsed' to receive the message using 'receiveSelectedMessage'.
-- To receive messages with guarded with a timeout see 'receiveAfter'.
--
-- @since 0.12.0
startTimer
  :: forall r q
   . ( Lifted IO q
     , HasCallStack
     , SetMember Process (Process q) r
     , Member Interrupts r
     )
  => Timeout
  -> Eff r TimerReference
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
     , SetMember Process (Process q) r
     , Member Interrupts r
     )
  => TimerReference
  -> Eff r ()
cancelTimer (TimerReference tr) = sendShutdown tr ExitNormally
