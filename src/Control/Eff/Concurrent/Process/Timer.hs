-- | Functions for receive timeouts and delayed messages sending.
--
-- Based on the 'delay' function.
--
-- @since 0.12.0
module Control.Eff.Concurrent.Process.Timer
  ( TimerReference()
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
  , receiveAfterWithTitle
  , receiveSelectedAfterWithTitle
  , receiveSelectedWithMonitorAfterWithTitle
  )

where

import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Misc
import           Control.Eff
import           Control.DeepSeq
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
   . ( HasCallStack
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
   . ( HasCallStack
     , HasProcesses r q
     , Show a
     , Typeable a
     )
  => MessageSelector a
  -> Timeout
  -> Eff r (Either TimerElapsed a)
receiveSelectedAfter sel t = do
  let timerTitle =
        MkProcessTitle
          (  pack "receive-timer-"
          <> pack (showSTypeable @a "")
          <> pack "-"
          <> pack (show t)
          )
  timerRef <- startTimerWithTitle timerTitle t
  res      <- receiveSelectedMessage
    (Left <$> selectTimerElapsed timerRef <|> Right <$> sel)
  cancelTimer timerRef
  return res

-- | Like 'receiveWithMonitor' combined with 'receiveSelectedAfter'.
--
-- @since 0.22.0
receiveSelectedWithMonitorAfter
  :: forall a r q
   . ( HasCallStack
     , HasProcesses r q
     , Show a
     , Typeable a
     )
  => ProcessId
  -> MessageSelector a
  -> Timeout
  -> Eff r (Either (Either ProcessDown TimerElapsed) a)
receiveSelectedWithMonitorAfter pid sel t =
  let timerTitle =
        MkProcessTitle
          (  pack "receive-timer-"
          <> pack (showSTypeable @a "")
          <> pack "-monitoring-"
          <> pack (show pid)
          <> pack "-"
          <> pack (show t)
          )
  in receiveSelectedWithMonitorAfterWithTitle pid sel t timerTitle


-- | Wait for a message of the given type for the given time. When no message
-- arrives in time, return 'Nothing'. This is based on
-- 'receiveSelectedAfterWithTitle'.
--
-- @since 0.12.0
receiveAfterWithTitle
  :: forall a r q
   . ( HasCallStack
     , HasProcesses r q
     , Typeable a
     , NFData a
     , Show a
     )
  => Timeout
  -> ProcessTitle
  -> Eff r (Maybe a)
receiveAfterWithTitle t timerTitle =
  either (const Nothing) Just <$> receiveSelectedAfterWithTitle (selectMessage @a) t timerTitle

-- | Wait for a message of the given type for the given time. When no message
-- arrives in time, return 'Left' 'TimerElapsed'. This is based on
-- 'selectTimerElapsed' and 'startTimerWithTitle'.
--
-- @since 0.12.0
receiveSelectedAfterWithTitle
  :: forall a r q
   . ( HasCallStack
     , HasProcesses r q
     , Show a
     , Typeable a
     )
  => MessageSelector a
  -> Timeout
  -> ProcessTitle
  -> Eff r (Either TimerElapsed a)
receiveSelectedAfterWithTitle sel t timerTitle = do
  timerRef <- startTimerWithTitle timerTitle t
  res      <- receiveSelectedMessage
    (Left <$> selectTimerElapsed timerRef <|> Right <$> sel)
  cancelTimer timerRef
  return res

-- | Like 'receiveWithMonitorWithTitle' combined with 'receiveSelectedAfterWithTitle'.
--
-- @since 0.30.0
receiveSelectedWithMonitorAfterWithTitle
  :: forall a r q
   . ( HasCallStack
     , HasProcesses r q
     , Show a
     , Typeable a
     )
  => ProcessId
  -> MessageSelector a
  -> Timeout
  -> ProcessTitle
  -> Eff r (Either (Either ProcessDown TimerElapsed) a)
receiveSelectedWithMonitorAfterWithTitle pid sel t timerTitle = do
  timerRef <- startTimerWithTitle timerTitle t
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
   . ( HasCallStack
     , HasProcesses r q
     , Typeable message
     , NFData message
     )
  => ProcessId
  -> Timeout
  -> (TimerReference -> message)
  -> Eff r TimerReference
sendAfter pid t mkMsg =
  sendAfterWithTitle
    (MkProcessTitle ( T.pack "send-after-timer-" <> T.pack (show t) <>  T.pack "-" <> T.pack (showSTypeable @message "") <>  T.pack "-" <> T.pack (show pid)))
    pid
    t
    mkMsg

-- | Like 'sendAfter' but with a user provided name for the timer process.
--
-- @since 0.30.0
sendAfterWithTitle
  :: forall r q message
   . ( HasCallStack
     , HasProcesses r q
     , Typeable message
     , NFData message
     )
  => ProcessTitle
  -> ProcessId
  -> Timeout
  -> (TimerReference -> message)
  -> Eff r TimerReference
sendAfterWithTitle title pid t mkMsg =
  TimerReference <$>
  (spawn
    title
    (delay t
    >>  self
    >>= (sendMessage pid . force . mkMsg . TimerReference)))

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
   . ( HasCallStack
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
   . ( HasCallStack
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
   . ( HasCallStack
     , HasProcesses r q
     )
  => TimerReference
  -> Eff r ()
cancelTimer (TimerReference tr) = sendShutdown tr ExitNormally
