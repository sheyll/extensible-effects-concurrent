{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Functions for receive timeouts and delayed messages sending.
--
-- Based on the 'delay' function.
--
-- @since 0.12.0
module Control.Eff.Concurrent.Process.Timer
  ( TimerReference (),
    TimerElapsed (fromTimerElapsed),
    sendAfter,
    startTimer,
    sendAfterWithTitle,
    sendAfterWithTitleAndLogMsg,
    startTimerWithTitle,
    cancelTimer,
    selectTimerElapsed,
    receiveAfter,
    receiveSelectedAfter,
    receiveSelectedWithMonitorAfter,
    receiveAfterWithTitle,
    receiveSelectedAfterWithTitle,
    receiveSelectedWithMonitorAfterWithTitle,
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Log.Handler
import Control.Eff.Log.Message
import Data.Foldable
import Data.Proxy
import Data.Typeable
import GHC.Stack

-- | Wait for a message of the given type for the given time. When no message
-- arrives in time, return 'Nothing'. This is based on
-- 'receiveSelectedAfter'.
--
-- @since 0.12.0
receiveAfter ::
  forall a r q.
  ( HasCallStack,
    HasProcesses r q,
    Member Logs q,
    Typeable a
  ) =>
  Timeout ->
  Eff r (Maybe a)
receiveAfter t =
  either (const Nothing) Just <$> receiveSelectedAfter (selectMessage @a) t

-- | Wait for a message of the given type for the given time. When no message
-- arrives in time, return 'Left' 'TimerElapsed'. This is based on
-- 'selectTimerElapsed' and 'startTimer'.
--
-- @since 0.12.0
receiveSelectedAfter ::
  forall a r q.
  ( HasCallStack,
    HasProcesses r q,
    Member Logs q
  ) =>
  MessageSelector a ->
  Timeout ->
  Eff r (Either TimerElapsed a)
receiveSelectedAfter sel t = do
  let timerTitle = MkProcessTitle "receive-timer"
  timerRef <- startTimerWithTitle timerTitle t
  res <-
    receiveSelectedMessage
      (Left <$> selectTimerElapsed timerRef <|> Right <$> sel)
  cancelTimer timerRef
  return res

-- | Like 'receiveWithMonitor' combined with 'receiveSelectedAfter'.
--
-- @since 0.22.0
receiveSelectedWithMonitorAfter ::
  forall a r q.
  ( HasCallStack,
    HasProcesses r q,
    Member Logs q,
    Member Logs r,
    ToTypeLogMsg a
  ) =>
  ProcessId ->
  MessageSelector a ->
  Timeout ->
  Eff r (Either (Either ProcessDown TimerElapsed) a)
receiveSelectedWithMonitorAfter pid sel t = do
  fromPid <- self
  let timerTitle = MkProcessTitle "receive-timeout"
      initialLog =
        packLogMsg "receice-timeout receiver: "
          <> toLogMsg fromPid
          <> packLogMsg " expected message type: "
          <> toTypeLogMsg (Proxy @a)
  receiveSelectedWithMonitorAfterWithTitle pid sel t timerTitle initialLog

-- | Wait for a message of the given type for the given time. When no message
-- arrives in time, return 'Nothing'. This is based on
-- 'receiveSelectedAfterWithTitle'.
--
-- @since 0.12.0
receiveAfterWithTitle ::
  forall a r q.
  ( HasCallStack,
    HasProcesses r q,
    Member Logs q,
    Typeable a
  ) =>
  Timeout ->
  ProcessTitle ->
  Eff r (Maybe a)
receiveAfterWithTitle t timerTitle =
  either (const Nothing) Just <$> receiveSelectedAfterWithTitle (selectMessage @a) t timerTitle

-- | Wait for a message of the given type for the given time. When no message
-- arrives in time, return 'Left' 'TimerElapsed'. This is based on
-- 'selectTimerElapsed' and 'startTimerWithTitle'.
--
-- @since 0.12.0
receiveSelectedAfterWithTitle ::
  forall a r q.
  ( HasCallStack,
    HasProcesses r q,
    Member Logs q
  ) =>
  MessageSelector a ->
  Timeout ->
  ProcessTitle ->
  Eff r (Either TimerElapsed a)
receiveSelectedAfterWithTitle sel t timerTitle = do
  timerRef <- startTimerWithTitle timerTitle t
  res <-
    receiveSelectedMessage
      (Left <$> selectTimerElapsed timerRef <|> Right <$> sel)
  cancelTimer timerRef
  return res

-- | Like 'receiveWithMonitorWithTitle' combined with 'receiveSelectedAfterWithTitle'.
--
-- @since 0.30.0
receiveSelectedWithMonitorAfterWithTitle ::
  forall a r q.
  ( HasCallStack,
    HasProcesses r q,
    Member Logs q,
    Member Logs r
  ) =>
  ProcessId ->
  MessageSelector a ->
  Timeout ->
  ProcessTitle ->
  LogMsg ->
  Eff r (Either (Either ProcessDown TimerElapsed) a)
receiveSelectedWithMonitorAfterWithTitle pid sel t timerTitle initialLogMsg = do
  timerRef <- startTimerWithTitle timerTitle t
  logDebug (LABEL "started" timerRef)
  logDebug initialLogMsg
  res <- withMonitor pid $ \pidMon -> do
    receiveSelectedMessage
      ( Left . Left <$> selectProcessDown pidMon
          <|> Left . Right <$> selectTimerElapsed timerRef
          <|> Right <$> sel
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
  deriving (NFData, Ord, Eq, Num, Integral, Real, Enum, Typeable)

instance ToLogMsg TimerReference where
  toLogMsg (TimerReference p) = "timer-ref" <> toLogMsg p

-- | A value to be sent when timer started with 'startTimer' has elapsed.
--
-- @since 0.12.0
newtype TimerElapsed = TimerElapsed {fromTimerElapsed :: TimerReference}
  deriving (NFData, Ord, Eq, Typeable)

instance ToTypeLogMsg TimerElapsed where
  toTypeLogMsg _ = "timer-elapsed"

instance ToLogMsg TimerElapsed where
  toLogMsg x = packLogMsg "elapsed: " <> toLogMsg (fromTimerElapsed x)

-- | Send a message to a given process after waiting. The message is created by
-- applying the function parameter to the 'TimerReference', such that the
-- message can directly refer to the timer.
--
-- @since 0.12.0
sendAfter ::
  forall r q message.
  ( HasCallStack,
    HasProcesses r q,
    Typeable message,
    NFData message,
    Member Logs q
  ) =>
  ProcessId ->
  Timeout ->
  (TimerReference -> message) ->
  Eff r TimerReference
sendAfter pid t mkMsg =
  sendAfterWithTitle
    (MkProcessTitle "send-after-timer")
    pid
    t
    mkMsg

-- | Like 'sendAfter' but with a user provided name for the timer process.
--
-- @since 0.30.0
sendAfterWithTitle ::
  forall r q message.
  ( HasCallStack,
    HasProcesses r q,
    Typeable message,
    NFData message,
    Member Logs q
  ) =>
  ProcessTitle ->
  ProcessId ->
  Timeout ->
  (TimerReference -> message) ->
  Eff r TimerReference
sendAfterWithTitle = sendAfterWithTitleAndMaybeLogMsg Nothing

-- | Like 'sendAfterWithTitle' but with a user provided initial debug `LogMsg`.
--
-- @since 1.0.0
sendAfterWithTitleAndLogMsg ::
  forall r q message.
  ( HasCallStack,
    HasProcesses r q,
    Member Logs q,
    Typeable message,
    NFData message
  ) =>
  LogMsg ->
  ProcessTitle ->
  ProcessId ->
  Timeout ->
  (TimerReference -> message) ->
  Eff r TimerReference
sendAfterWithTitleAndLogMsg =
  sendAfterWithTitleAndMaybeLogMsg . Just

-- | Like 'sendAfterWithTitle' but with a user provided, optional initial debug `LogMsg`.
--
-- @since 1.0.0
sendAfterWithTitleAndMaybeLogMsg ::
  forall r q message.
  ( HasCallStack,
    HasProcesses r q,
    Member Logs q,
    Typeable message,
    NFData message
  ) =>
  Maybe LogMsg ->
  ProcessTitle ->
  ProcessId ->
  Timeout ->
  (TimerReference -> message) ->
  Eff r TimerReference
sendAfterWithTitleAndMaybeLogMsg initialLogMsg title pid t mkMsg =
  TimerReference
    <$> ( spawn
            title
            ( do
                traverse_ logDebug initialLogMsg
                me <- self
                let meRef = TimerReference me
                logDebug (LABEL "timer started" title) pid t meRef
                delay t
                logDebug (LABEL "timer elapsed" title) pid t meRef
                sendMessage pid (force (mkMsg meRef))
            )
        )

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
startTimerWithTitle ::
  forall r q.
  ( HasCallStack,
    Member Logs q,
    HasProcesses r q
  ) =>
  ProcessTitle ->
  Timeout ->
  Eff r TimerReference -- TODO add a parameter to the TimerReference
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
startTimer ::
  forall r q.
  ( HasCallStack,
    HasProcesses r q,
    Member Logs q
  ) =>
  Timeout ->
  Eff r TimerReference -- TODO add a parameter to the TimerReference
startTimer t = do
  p <- self
  sendAfter p t TimerElapsed

-- | Cancel a timer started with 'startTimer'.
--
-- @since 0.12.0
cancelTimer ::
  forall r q. HasProcesses r q => TimerReference -> Eff r ()
cancelTimer (TimerReference tr) = sendShutdown tr ExitNormally
