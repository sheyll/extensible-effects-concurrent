{-# LANGUAGE UndecidableInstances #-}
-- | A logging effect.
--
-- There is just one log message type: 'LogMessage' and it is written using 'logMsg' and
-- the functions built on top of it.
--
-- Example:
--
-- > main =
-- >     runLift
-- >   $ runLogWriterReader (makeIoLogWriter printLogMessage)
-- >   $ runLogs
-- >   $ logToReader
-- >   $ do
-- >       logDebug "test 1.1"
-- >       logDebug "test 1.2"
-- >       setThreadIdAndTimestamp
-- >        $ do
-- >             logTo traceLogMessages
-- >              $ filterLogMessages (\m -> (view lmMessage m) /= "not logged")
-- >              $ do
-- >                   logDebug "not logged"
-- >                   logDebug "test 2.1"
-- >             logDebug "test 2.2"
-- >       logDebug "test 1.3"
module Control.Eff.Log.Handler
  ( logMsg
  , respondToLogMessage
  , censorLogs
  , censorLogsM
  , filterLogMessages
  , Logs()
  , runLogsFiltered
  , runLogs
  , ignoreLogs
  , askLogFilter
  , localLogFilter
  , logWithSeverity
  , logEmergency
  , logAlert
  , logCritical
  , logError
  , logWarning
  , logNotice
  , logInfo
  , logDebug
  , logTo
  , logToReader
  , LoggingAndIo
  )
where

import           Control.DeepSeq
import           Control.Eff                   as Eff
import           Control.Eff.Extend
import           Control.Eff.Log.Message
import           Control.Eff.Log.Writer
import qualified Control.Exception.Safe        as Safe
import           Control.Lens
import           Control.Monad                   ( when, (>=>) )
import           Control.Monad.Base              ( MonadBase() )
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Trans.Control     ( MonadBaseControl
                                                   ( restoreM
                                                   , liftBaseWith
                                                   , StM
                                                   )
                                                 )
import           Data.Default
import           Data.Function                  ( fix )
import           GHC.Stack                      ( HasCallStack
                                                , callStack
                                                , withFrozenCallStack
                                                )


-- * Message Logging Effect


-- | This effect sends log messages.
-- There are two messages for the 'Logs' effect, that have these purposes:
--
-- 1. Retrieving a (global, pre-writing) 'LogPredicate'
-- 2. Writing a 'LogMessage'
--
-- To log a message use 'logMsg'. To store or display log messages, use 'logTo'.
data Logs v where
  AskLogFilter
    :: Logs LogPredicate
  WriteLogMessage
    :: LogMessage -> Logs ()

instance forall e a k. Handle Logs e a (LogPredicate -> k) where
  handle h q AskLogFilter p         = h (q ^$ p) p
  handle h q (WriteLogMessage _) p  = h (q ^$ ()) p

-- | This instance allows lifting to the 'Logs' effect, they do, however, need a
-- 'LogWriter' in the base monad, in order to be able to handle 'WriteLogMessage'.
--
-- The 'LogWriterReader' effect is must be available to get to the 'LogWriter'.
--
-- Otherwise there is no way to preserve to log messages.
instance forall m r. (MonadBase m m, LiftedBase m r, LiftsLogWriter m (Logs ': r), Member (LogWriterReader m) r)
  => MonadBaseControl m (Eff (Logs ': r)) where
    type StM (Eff (Logs ': r)) a =  StM (Eff r) a
    liftBaseWith f = do
      lf <- askLogFilter
      raise (liftBaseWith (\runInBase -> f (runInBase . runLogsFiltered lf . logToReader @m)))
    restoreM = raise . restoreM

instance (LiftedBase m e, Catch.MonadThrow (Eff e))
  => Catch.MonadThrow (Eff (Logs ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance (Applicative m, LiftedBase m e, Catch.MonadCatch (Eff e), LiftsLogWriter m (Logs ': e), Member (LogWriterReader m) e)
  => Catch.MonadCatch (Eff (Logs ': e)) where
  catch effect handler = do
    lf <- askLogFilter
    let lower                   = runLogsFiltered lf . logToReader @m
        nestedEffects           = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (Applicative m, LiftedBase m e, Catch.MonadMask (Eff e), LiftsLogWriter m (Logs ': e), Member (LogWriterReader m) e)
  => Catch.MonadMask (Eff (Logs ': e)) where
  mask maskedEffect = do
    lf <- askLogFilter
    let
      lower :: Eff (Logs ': e) a -> Eff e a
      lower = runLogsFiltered lf . logToReader @m
    raise
        (Catch.mask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  uninterruptibleMask maskedEffect = do
    lf <- askLogFilter
    let
      lower :: Eff (Logs ': e) a -> Eff e a
      lower = runLogsFiltered lf . logToReader @m
    raise
        (Catch.uninterruptibleMask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  generalBracket acquire release useIt = do
    lf <- askLogFilter
    let
      lower :: Eff (Logs ': e) a -> Eff e a
      lower = runLogsFiltered lf . logToReader @m
    raise
        (Catch.generalBracket
          (lower acquire)
          (((.).(.)) lower release)
          (lower . useIt)
      )

-- | Handle the 'Logs' effect.
runLogsFiltered :: LogPredicate -> Eff (Logs ': e) b -> Eff e b
runLogsFiltered p m = fix (handle_relay (\a _ -> return a)) m p

-- | Handle the 'Logs' effect, like 'runLogsFiltered' does, but start with a
-- permissive log filter that removes no log messages.
runLogs :: Eff (Logs ': e) b -> Eff e b
runLogs = runLogsFiltered (const True)

-- | Handle the 'Logs' effect, start with a filter that discards all messages.
--
-- This function has the semantics of @'runLogsFiltered' (const Nothing)@.
ignoreLogs :: forall r a . Eff (Logs ': r) a -> Eff r a
ignoreLogs = runLogsFiltered (const False)

-- * Modify the 'LogPredicate'

-- | Get the current 'Logs' filter/transformer function.
askLogFilter :: forall e . (Member Logs e) => Eff e LogPredicate
askLogFilter = send @Logs AskLogFilter

-- | Keep only those messages, for which a predicate holds.
--
-- E.g. to keep only messages which begin with @"OMG"@:
--
-- > filterLogMessages (\msg -> case lmMessage msg of
-- >                             'O':'M':'G':_ -> True
-- >                              _            -> False)
-- >                   (do logMsg "this message will not be logged"
-- >                       logMsg "OMG logged")
--
-- In order to also delegate to the previous predicate, use 'localLogFilter'
filterLogMessages
  :: forall r b
   . (Member Logs r, HasCallStack)
  => LogPredicate
  -> Eff r b
  -> Eff r b
filterLogMessages = localLogFilter . const

-- | Change the 'LogPredicate'.
--
-- Other than 'filterLogMessages' this function allows to include the previous predicate, too.
--
-- For to discard all messages currently no satisfieing the predicate and also all messages
-- that are to long:
--
-- @
-- > localLogFilter (\previousPredicate msg -> previousPredicate msg && length (lmMessage msg) < 29 )
-- >                (do logMsg "this message will not be logged"
-- >                    logMsg "this message might be logged")
-- @
localLogFilter
  :: forall e b
   . (Member Logs e, HasCallStack)
  => (LogPredicate -> LogPredicate)
  -> Eff e b
  -> Eff e b
localLogFilter lpIn e = askLogFilter >>= fix step e . lpIn
  where
    ret x _ = return x
    step :: (Eff e b -> LogPredicate -> Eff e b) ->  Eff e b -> LogPredicate -> Eff e b
    step k (E q (prj -> Just (WriteLogMessage !l))) lp = do
      logMsg l
      respond_relay @Logs ret k (q ^$ ()) lp
    step k m lp = respond_relay @Logs ret k m lp

-- * Transform Written Log Messages

-- | Respond to the 'WriteLogMessage' command.
--
-- NOTE: The effects of this function are **lost** when using
-- 'MonadBaseControl', 'MonadMask', 'MonadCatch' and 'MonadThrow'.
--
-- It's better to use the functions based on modifying the 'LogWriter' in a 'LogWriterReader',
-- since the instances of the above mentioned classes use 'logToReader' internally.
respondToLogMessage
  :: forall r b
   . (Member Logs r)
  => (LogMessage -> Eff r ())
  -> Eff r b
  -> Eff r b
respondToLogMessage f e = askLogFilter >>= fix step e
  where
    step :: (Eff r b -> LogPredicate -> Eff r b) ->  Eff r b -> LogPredicate -> Eff r b
    step k (E q (prj -> Just (WriteLogMessage !l))) lp = do
      f l
      respond_relay @Logs ret k (q ^$ ()) lp
    step k m lp = respond_relay @Logs ret k m lp
    ret x _lf = return x

-- | Change the 'LogMessages' logged using 'logMsg' or 'WriteLogMessage'. See 'censorLogsM'
--
-- NOTE: The effects of this function are **lost** when using
-- 'MonadBaseControl', 'MonadMask', 'MonadCatch' and 'MonadThrow'.
--
-- It's better to use the functions based on modifying the 'LogWriter' in a 'LogWriterReader',
-- since the instances of the above mentioned classes use 'logToReader' internally.
censorLogs
  :: forall r b
   . (Member Logs r)
  => (LogMessage -> LogMessage)
  -> Eff r b
  -> Eff r b
censorLogs f = respondToLogMessage (logMsg . f)

-- | Change the 'LogMessages' using an effectful function. See 'censorLogs'.
--
-- NOTE: The effects of this function are **lost** when using
-- 'MonadBaseControl', 'MonadMask', 'MonadCatch' and 'MonadThrow'.
--
-- It's better to use the functions based on modifying the 'LogWriter' in a 'LogWriterReader',
-- since the instances of the above mentioned classes use 'logToReader' internally.
censorLogsM
  :: forall r b
   . (Member Logs r)
  => (LogMessage -> Eff r LogMessage)
  -> Eff r b
  -> Eff r b
censorLogsM f = respondToLogMessage (f >=> logMsg)

-- ** LogMessage enrichment

-- * Logging functions

-- | Log a message. The message is reduced to normal form (strict).
logMsg :: forall e m . (Member Logs e, ToLogMessage m) => m -> Eff e ()
logMsg (force . toLogMessage -> msgIn) = do
  lf <- askLogFilter
  when (lf msgIn) (send @Logs (WriteLogMessage msgIn))

-- | Log a 'String' as 'LogMessage' with a given 'Severity'.
logWithSeverity
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Severity
  -> String
  -> Eff e ()
logWithSeverity !s =
  withFrozenCallStack
    $ logMsg
    . setCallStack callStack
    . set lmSeverity s
    . flip (set lmMessage) def

-- | Log a 'String' as 'emergencySeverity'.
logEmergency
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logEmergency = withFrozenCallStack (logWithSeverity emergencySeverity)

-- | Log a message with 'alertSeverity'.
logAlert
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logAlert = withFrozenCallStack (logWithSeverity alertSeverity)

-- | Log a 'criticalSeverity' message.
logCritical
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logCritical = withFrozenCallStack (logWithSeverity criticalSeverity)

-- | Log a 'errorSeverity' message.
logError
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logError = withFrozenCallStack (logWithSeverity errorSeverity)

-- | Log a 'warningSeverity' message.
logWarning
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logWarning = withFrozenCallStack (logWithSeverity warningSeverity)

-- | Log a 'noticeSeverity' message.
logNotice
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logNotice = withFrozenCallStack (logWithSeverity noticeSeverity)

-- | Log a 'informationalSeverity' message.
logInfo
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logInfo = withFrozenCallStack (logWithSeverity informationalSeverity)

-- | Log a 'debugSeverity' message.
logDebug
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logDebug = withFrozenCallStack (logWithSeverity debugSeverity)

-- * 'LogWriter' integration

-- | Relay all logs to a single 'LogWriter'.
-- There can be multiple, stacked log writers:
--
-- >>> runLift $ runLogs $ logTo traceLogMessages $ logTo traceLogMessages $ logMsg "this message is logged twice"
-- INFO      this message is logged twice                                                     Message.hs line 170
-- INFO      this message is logged twice                                                     Message.hs line 170
--
-- NOTE: The effects of 'logTo' are **lost** when using
-- 'MonadBaseControl', 'MonadMask', 'MonadCatch' and 'MonadThrow'.
--
-- It's better to use the functions based on modifying the 'LogWriter' in a 'LogWriterReader',
-- since the instances of the above mentioned classes use 'logToReader' internally.
logTo
  :: forall h e a. (Member Logs e, LiftsLogWriter h e)
  => LogWriter h -> Eff e a -> Eff e a
logTo lw = respondToLogMessage go
  where
    go m = do
      liftLogWriter @h lw m
      logMsg m

-- | Relay all logs to a 'LogWriter'.
-- There can be multiple log writers.
logToReader
  :: forall h e a. (Member Logs e, Member (LogWriterReader h) e, LiftsLogWriter h e)
  => Eff e a -> Eff e a
logToReader = censorLogsM go
  where go msg = do
          lw <- askLogWriter @h
          liftLogWriter lw msg
          return msg

-- | The concrete list of 'Eff'ects for logging with an IO based 'LogWriter', and a 'LogWriterReader'
-- with these effects it is possible to use the 'MonadBaseControl', 'MonadMask', 'MonadCatch' and 'MonadThrow'
-- instances.
type LoggingAndIo = '[Logs, LogWriterReader IO, Lift IO]
