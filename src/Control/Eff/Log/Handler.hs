-- | A logging effect based on 'Control.Monad.Log.MonadLog'.
module Control.Eff.Log.Handler
  (
    -- * Logging Effect
    Logs(..)
  , logMsg
  , interceptLogging
  , foldLogMessages
  , relogAsString
  , captureLogs
  , ignoreLogs
  , handleLogsWith
  , handleLogsLifted
  , handleLogsWithLoggingTHandler
  )
where

import           Control.DeepSeq
import           Control.Eff                   as Eff
import           Control.Eff.Extend            as Eff
import qualified Control.Eff.Lift              as Eff
import qualified Control.Monad.Log             as Log
import           Data.Foldable                  ( traverse_ )
import           Data.Kind                      ( )
import           Data.Sequence                  ( Seq() )
import qualified Data.Sequence                 as Seq

-- | Logging effect type, parameterized by a log message type.
data Logs message a where
  LogMsg :: message -> Logs message ()

-- | Log a message.
logMsg :: Member (Logs m) r => m -> Eff r ()
logMsg = send . LogMsg

-- | Change, add or remove log messages and perform arbitrary actions upon
-- intercepting a log message.
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
interceptLogging
  :: forall r m a . Member (Logs m) r => (m -> Eff r ()) -> Eff r a -> Eff r a
interceptLogging interceptor = interpose return go
 where
  go :: Member (Logs m) r => Logs m x -> (Arr r x y) -> Eff r y
  go (LogMsg m) k = do
    interceptor m
    k ()

-- | Intercept logging to change, add or remove log messages.
--
-- This is without side effects, hence faster than 'interceptLogging'.
foldLogMessages
  :: forall r m a f
   . (Foldable f, Member (Logs m) r)
  => (m -> f m)
  -> Eff r a
  -> Eff r a
foldLogMessages interceptor = interpose return go
 where
  go :: Member (Logs m) r => Logs m x -> (Arr r x y) -> Eff r y
  go (LogMsg m) k = do
    traverse_ logMsg (interceptor m)
    k ()

-- | Capture all log messages in a 'Seq' (strict).
captureLogs
  :: NFData message => Eff (Logs message ': r) a -> Eff r (a, Seq message)
captureLogs = Eff.handle_relay_s Seq.empty
                                 (\logs result -> return (result, logs))
                                 handleLogs
 where
  handleLogs
    :: NFData message
    => Seq message
    -> Logs message x
    -> (Seq message -> Arr r x y)
    -> Eff r y
  handleLogs !logs (LogMsg !m) k = k (force (logs Seq.:|> m)) ()

-- | Throw away all log messages.
ignoreLogs :: forall message r a . Eff (Logs message ': r) a -> Eff r a
ignoreLogs = Eff.handle_relay return handleLogs
 where
  handleLogs :: Logs m x -> Arr r x y -> Eff r y
  handleLogs (LogMsg _) k = k ()

-- | Handle a 'Logs' effect with a message that has a 'Show' instance by
-- **re-logging** each message applied to 'show'.
relogAsString
  :: forall m e a
   . (Show m, Member (Logs String) e)
  => Eff (Logs m ': e) a
  -> Eff e a
relogAsString = handleLogsWith (logMsg . show)

-- | Apply a function that returns an effect to each log message.
handleLogsWith
  :: forall message e a
   . (message -> Eff e ())
  -> Eff (Logs message ': e) a
  -> Eff e a
handleLogsWith h = handle_relay return $ \(LogMsg m) k -> h m >>= k

-- | Handle the 'Logs' effect with a monadic call back function (strict).
handleLogsLifted
  :: forall m r message a
   . (NFData message, Monad m, SetMember Eff.Lift (Eff.Lift m) r)
  => (message -> m ())
  -> Eff (Logs message ': r) a
  -> Eff r a
handleLogsLifted logMessageHandler = handleLogsWith go
 where
  go :: message -> Eff r ()
  go m = Eff.lift (logMessageHandler (force m))

-- | Handle the 'Logs' effect using 'Log.LoggingT' 'Log.Handler's.
handleLogsWithLoggingTHandler
  :: forall m r message a
   . (Monad m, SetMember Eff.Lift (Eff.Lift m) r)
  => (forall b . (Log.Handler m message -> m b) -> m b)
  -> Eff (Logs message ': r) a
  -> Eff r a
handleLogsWithLoggingTHandler foldHandler =
  handleLogsWith (Eff.lift . foldHandler . flip ($!))
