{-# LANGUAGE UndecidableInstances #-}
-- | A logging effect based on 'Control.Monad.Log.MonadLog'.
module Control.Eff.Log.Handler
  ( logMsg
  , mapLogMessages
  , filterLogMessages
  , ignoreLogs
  , traceLogs
  , Logs()
  , withLogMessageHandler
  , askLogFilter
  , runLogsFiltered
  )
where

import           Control.DeepSeq
import           Control.Eff                   as Eff
import           Control.Eff.Extend
import qualified Control.Exception.Safe        as Safe
import           Data.Function                  ( fix )
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Trans.Control    ( MonadBaseControl
                                                  ( restoreM
                                                  , liftBaseWith
                                                  , StM
                                                  )
                                                )
import           Control.Monad.Base             ( MonadBase() )
import           Debug.Trace

-- * Message Logging Effect

-- | Log a message. The message is reduced to normal form (strict).
logMsg :: (NFData m, Member (Logs m) e) => m -> Eff e ()
logMsg (force -> msg) = rnf msg `seq` send (SendLogMessage msg)

-- ** Filter and Transform Log Messages

-- | Map a pure function over log messages.
mapLogMessages
  :: forall m r b
   . (NFData m, Member (Logs m) r)
  => (m -> m)
  -> Eff r b
  -> Eff r b
mapLogMessages f = localLogFilter (Just . f)

-- | Keep only those messages, for which a predicate holds.
--
-- E.g. to keep only messages which begin with @"OMG"@:
--
-- > filterLogMessages (\msg -> case msg of
-- >                             'O':'M':'G':_ -> True
-- >                              _            -> False)
-- >                   (do logMsg "this message will not be logged"
-- >                       logMsg "OMG logged")
filterLogMessages
  :: forall m r b
   . (NFData m, Member (Logs m) r)
  => (m -> Bool)
  -> Eff r b
  -> Eff r b
filterLogMessages predicate logEff  = do
  old <- send AskLogFilter
  localLogFilter (\m -> if predicate m then old m else Nothing) logEff


-- * Handle Log Messages

-- | Trace all log messages using 'traceM'. The message value is
-- converted to 'String' using the given function.
traceLogs
  :: forall message r a
   . NFData message
  => (message -> String)
  -> Eff (Logs message ': r) a
  -> Eff r a
traceLogs toString = runLogsFiltered Just . withLogMessageHandler go
 where
  go :: (Monad m) => message -> m ()
  go = traceM . toString


-- ** Log Message Writer Creation

-- ** Low-Level Log Message Sending

-- | This effect sends log messages.
-- The logs are not sent one-by-one, but always in batches of
-- containers that must be 'Traversable' and 'MonadPlus' instances.
-- Log messages are consumed by 'LogWriter's installed via
-- 'ignoreLogs' or more high level functions like 'writeLogs'.
data Logs m v where
  AskLogFilter
    :: (NFData m) => Logs m (m -> Maybe m)
  SendLogMessage
    :: (NFData m) => m -> Logs m ()

-- | Drops all messages
instance Handle (Logs m) e a ((m -> Maybe m) -> k) where
  handle handleNext handleResponseAndContinue request logMessageFilter =
    case request of
      AskLogFilter ->
        handleNext (handleResponseAndContinue ^$ logMessageFilter) logMessageFilter
      SendLogMessage _ ->
        handleNext
          (handleResponseAndContinue ^$ ())
          logMessageFilter

-- | Get the current 'Logs' filter/transformer function.
askLogFilter :: (NFData m, Member (Logs m) e) => Eff e (m -> Maybe m)
askLogFilter = send AskLogFilter

-- | Replace the current log filter.
localLogFilter
  :: forall m e b
   . (NFData m, Member (Logs m) e)
  => (m -> Maybe m)
  -> Eff e b
  -> Eff e b
localLogFilter lf actionThatLogs =
  fix
     (respond_relay'
        (\step co (req :: Logs m v) lf2 ->
            case req of
              AskLogFilter -> step (co ^$ lf2) lf2
              SendLogMessage ms -> logMsg ms >> step (co ^$ ()) lf2
        )
        (\a _lf -> return a)
     )
     actionThatLogs
     lf

-- | Handle the 'Logs' effect.
--
-- The 'Logs' effect contains two elements:
--
-- * A 'Reader' for the log filter/transformation function
--
-- * A log message writer interface
--
-- The first argument is used as filter/transformation function.
--
-- All log messages received by this function are silently dropped.
-- Log message writing is a task delegated to e.g. 'withLogMessageHandler'
-- or to 'withLogWriter'.
runLogsFiltered :: (m -> Maybe m) -> Eff (Logs m ': e) b -> Eff e b
runLogsFiltered logMessageFilter logClient =
  fix (handle_relay (\a _f -> return a)) logClient logMessageFilter

-- | Handle the 'Logs' effect, start with a filter that discards all messages.
--
-- This function has the semantics of @'runLogsFiltered' (const Nothing)@.
ignoreLogs :: forall m r a . Eff (Logs m ': r) a -> Eff r a
ignoreLogs = runLogsFiltered (const Nothing)

-- | Apply a callback to log messages, that pass the current log message filter (see: 'askLogFilter').
withLogMessageHandler
  :: forall m e b
   . (NFData m, SetMember Logs (Logs m) e)
  => (m -> Eff e ())
  -> Eff e b
  -> Eff e b
withLogMessageHandler lw actionThatLogs =
  (askLogFilter :: Eff e (m -> Maybe m))
  >>=
  fix
     (respond_relay'
        (\step co (req :: Logs m v) lf2 ->
            case req of
              AskLogFilter      -> step (co ^$ lf2)                 lf2
              SendLogMessage ms -> step (lw (force ms) >>= qApp co) lf2
        )
        (\a _lf -> return a)
     )
     actionThatLogs

-- | This instance allows lifting to the 'Logs' effect.
instance (MonadBase m m, LiftedBase m r, NFData l) => MonadBaseControl m (Eff (Logs l ': r)) where

    type StM (Eff (Logs l ': r)) a =  StM (Eff r) a

    liftBaseWith f = do
      lf <- askLogFilter
      raise (liftBaseWith (\runInBase -> f (runInBase . runLogsFiltered lf)))

    restoreM = raise . restoreM

instance (NFData l, LiftedBase m e, Catch.MonadThrow (Eff e))
  => Catch.MonadThrow (Eff (Logs l ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance (NFData l, Applicative m, LiftedBase m e, Catch.MonadCatch (Eff e))
  => Catch.MonadCatch (Eff (Logs l ': e)) where
  catch effect handler = do
    logFilter <- askLogFilter
    let lower                   = runLogsFiltered logFilter
        nestedEffects           = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (NFData l, Applicative m, LiftedBase m e, Catch.MonadMask (Eff e))
  => Catch.MonadMask (Eff (Logs l ': e)) where
  mask maskedEffect = do
    logFilter <- askLogFilter
    let
      lower :: Eff (Logs l ': e) a -> Eff e a
      lower = runLogsFiltered logFilter
    raise
        (Catch.mask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  uninterruptibleMask maskedEffect = do
    logFilter <- askLogFilter
    let
      lower :: Eff (Logs l ': e) a -> Eff e a
      lower = runLogsFiltered logFilter
    raise
        (Catch.uninterruptibleMask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  generalBracket acquire release use = do
    logFilter <- askLogFilter
    let
      lower :: Eff (Logs l ': e) a -> Eff e a
      lower = runLogsFiltered logFilter
    raise
        (Catch.generalBracket
          (lower acquire)
          (((.).(.)) lower release)
          (lower . use)
      )
