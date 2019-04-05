{-# LANGUAGE UndecidableInstances #-}
-- | A logging effect.
--
-- There is just one log message type: 'LogMessage' and it is written using 'logMsg' and
-- the functions built on top of it.
--
-- The 'Logs' effect is tightly coupled with the 'LogWriterReader' effect.
-- When using the 'MonadBaseControl' instance, the underlying monad of the 'LogWriter',
-- that is expected to be present through the respective 'LogWriterReader', is
-- constrained to be the base monad itself, e.g. 'IO'.
--
-- The log message type is fixed to 'LogMessage', and there is a type class for
-- converting to that, call 'ToLogMessage'.
--
-- There is a single global 'LogPredicate' that can be used to suppress logs directly
-- at the point where they are sent, in the 'logMsg' function.
--
-- Note that all logging is eventually done via 'logMsg'; 'logMsg' is the __only__ place where
-- log filtering should happen.
--
-- Also, 'LogMessage's are evaluated using 'deepseq', __after__ they pass the 'LogPredicate', also inside 'logMsg'.
--
-- Example:
--
-- > main =
-- >     runLift
-- >   $ runLogWriterReader (ioLogWriter printLogMessage)
-- >   $ runLogs
-- >   $ do
-- >       logDebug "test 1.1"
-- >       logDebug "test 1.2"
-- >       setThreadIdAndTimestamp
-- >        $ do
-- >             logTo debugTraceLogWriter
-- >              $ setLogPredicate (\m -> (view lmMessage m) /= "not logged")
-- >              $ do
-- >                   logDebug "not logged"
-- >                   logDebug "test 2.1"
-- >             logDebug "test 2.2"
-- >       logDebug "test 1.3"
module Control.Eff.Log.Handler
  ( logMsg
  , respondToLogMessage
  , interceptLogMessages
  , Logs()
  , runLogsFiltered
  , runLogs
  , ignoreLogs
  , askLogPredicate
  , setLogPredicate
  , modifyLogPredicate
  , logWithSeverity
  , logEmergency
  , logAlert
  , logCritical
  , logError
  , logWarning
  , logNotice
  , logInfo
  , logDebug
  , LogsTo
  , LoggingAndIo
  , LogWriterReader
  , runLogWriterReader
  , askLogWriter
  , modifyLogWriter
  , setLogWriter
  , addLogWriter
  , censorLogs
  , censorLogsM
  , filterLogs
  , setThreadIdAndTimestamp
  , increaseLogMessageDistance
  , dropDistantLogMessages
  , withLogFileAppender
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
                                                   , liftBaseOp
                                                 )
import           Data.Default
import           Data.Function                  ( fix )
import           GHC.Stack                      ( HasCallStack
                                                , callStack
                                                , withFrozenCallStack
                                                )
import qualified System.IO                     as IO
import           Control.Lens                   ( (^.), over )
import           System.Directory               ( canonicalizePath
                                                , createDirectoryIfMissing
                                                )
import           System.FilePath                ( takeDirectory )


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
  handle h q AskLogFilter p         = h (q ^$ p ) p
  handle h q (WriteLogMessage _) p  = h (q ^$ ()) p

-- | This instance allows lifting to the 'Logs' effect, they do, however, need a
-- 'LogWriter' in the base monad, in order to be able to handle 'WriteLogMessage'.
--
-- The 'LogWriterReader' effect is must be available to get to the 'LogWriter'.
--
-- Otherwise there is no way to preserve to log messages.
instance forall m e. (MonadBase m m, LiftedBase m e, LiftsLogWriter m (Logs ': e), SetMember LogWriterReader (LogWriterReader m) (Logs ': e))
  => MonadBaseControl m (Eff (Logs ': e)) where
    type StM (Eff (Logs ': e)) a =  StM (Eff e) a
    liftBaseWith f = do
      lf <- askLogPredicate
      raise (liftBaseWith (\runInBase -> f (runInBase . runLogsFiltered @m lf)))
    restoreM = raise . restoreM

instance (LiftedBase m e, Catch.MonadThrow (Eff e))
  => Catch.MonadThrow (Eff (Logs ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance (Applicative m, LiftedBase m e, Catch.MonadCatch (Eff e), LiftsLogWriter m (Logs ': e), SetMember LogWriterReader (LogWriterReader m) (Logs ': e))
  => Catch.MonadCatch (Eff (Logs ': e)) where
  catch effect handler = do
    lf <- askLogPredicate
    let lower                   = runLogsFiltered @m lf
        nestedEffects           = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (Applicative m, LiftedBase m e, Catch.MonadMask (Eff e), LiftsLogWriter m (Logs ': e), SetMember LogWriterReader (LogWriterReader m) (Logs ': e))
  => Catch.MonadMask (Eff (Logs ': e)) where
  mask maskedEffect = do
    lf <- askLogPredicate
    let
      lower :: Eff (Logs ': e) a -> Eff e a
      lower = runLogsFiltered @m lf
    raise
        (Catch.mask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  uninterruptibleMask maskedEffect = do
    lf <- askLogPredicate
    let
      lower :: Eff (Logs ': e) a -> Eff e a
      lower = runLogsFiltered @m lf
    raise
        (Catch.uninterruptibleMask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  generalBracket acquire release useIt = do
    lf <- askLogPredicate
    let
      lower :: Eff (Logs ': e) a -> Eff e a
      lower = runLogsFiltered @m lf
    raise
        (Catch.generalBracket
          (lower acquire)
          (((.).(.)) lower release)
          (lower . useIt)
      )

-- | Handle the 'Logs' effect.
runLogsFiltered
  :: forall h e b .
     (LogsTo h (Logs ': e), LiftsLogWriter h (Logs ': e))
  => LogPredicate
  -> Eff (Logs ': e) b
  -> Eff e b
runLogsFiltered p m =
  fix (handle_relay (\a _ -> return a)) (sendLogMessageToLogWriter  m) p

-- | Handle the 'Logs' effect, like 'runLogsFiltered' does, but start with a
-- permissive log filter that removes no log messages.
runLogs
  :: forall h e b .
     (LogsTo h (Logs ': e), LiftsLogWriter h (Logs ': e))
  => Eff (Logs ': e) b
  -> Eff e b
runLogs = runLogsFiltered (const True)

-- | Handle the 'Logs' effect, start with a filter that discards all messages.
--
-- This function has the semantics of @'runLogsFiltered' (const Nothing)@.
ignoreLogs
  :: forall h e b .
     (LogsTo h (Logs ': e), LiftsLogWriter h (Logs ': e))
  => Eff (Logs ': e) b
  -> Eff e b
ignoreLogs = runLogsFiltered  (const False)

-- * Logging functions

-- | Log a message.
--
-- All logging goes through this function.
--
-- This function is the only place where the 'LogPredicate' is applied.
--
-- Also, 'LogMessage's are evaluated using 'deepseq', __after__ they pass the 'LogPredicate'.
logMsg :: forall e m . (Member Logs e, ToLogMessage m) => m -> Eff e ()
logMsg (force . toLogMessage -> msgIn) = do
  lf <- askLogPredicate
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

-- * Misc Type Aliases

-- | The concrete list of 'Eff'ects for logging with an IO based 'LogWriter', and a 'LogWriterReader'
-- with these effects it is possible to use the 'MonadBaseControl', 'MonadMask', 'MonadCatch' and 'MonadThrow'
-- instances.
type LoggingAndIo = '[Logs, LogWriterReader IO, Lift IO]


-- * Modify the 'LogPredicate'

-- | Get the current 'Logs' filter/transformer function.
askLogPredicate :: forall e . (Member Logs e) => Eff e LogPredicate
askLogPredicate = send @Logs AskLogFilter

-- | Keep only those messages, for which a predicate holds.
--
-- E.g. to keep only messages which begin with @"OMG"@:
--
-- > setLogPredicate (\msg -> case lmMessage msg of
-- >                             'O':'M':'G':_ -> True
-- >                              _            -> False)
-- >                   (do logMsg "this message will not be logged"
-- >                       logMsg "OMG logged")
--
-- In order to also delegate to the previous predicate, use 'modifyLogPredicate'
setLogPredicate
  :: forall r b
   . (Member Logs r, HasCallStack)
  => LogPredicate
  -> Eff r b
  -> Eff r b
setLogPredicate = modifyLogPredicate . const

-- | Change the 'LogPredicate'.
--
-- Other than 'setLogPredicate' this function allows to include the previous predicate, too.
--
-- For to discard all messages currently no satisfying the predicate and also all messages
-- that are to long:
--
-- @
-- > modifyLogPredicate (\previousPredicate msg -> previousPredicate msg && length (lmMessage msg) < 29 )
-- >                (do logMsg "this message will not be logged"
-- >                    logMsg "this message might be logged")
-- @
modifyLogPredicate
  :: forall e b
   . (Member Logs e, HasCallStack)
  => (LogPredicate -> LogPredicate)
  -> Eff e b
  -> Eff e b
modifyLogPredicate lpIn e = askLogPredicate >>= fix step e . lpIn
  where
    ret x _ = return x
    step :: (Eff e b -> LogPredicate -> Eff e b) ->  Eff e b -> LogPredicate -> Eff e b
    step k (E q (prj -> Just (WriteLogMessage !l))) lp = do
      logMsg l
      respond_relay @Logs ret k (q ^$ ()) lp
    step k m lp = respond_relay @Logs ret k m lp

-- * Advanced Unintuitive Functions


-- | Consume log messages.
--
-- Respond to all 'LogMessage's logged from the given action,
-- up to any 'MonadBaseControl' liftings.
--
-- Note that all logging is done through 'logMsg' and that means
-- only messages passing the 'LogPredicate' are received.
--
-- The 'LogMessage's are __consumed__ once they are passed to the
-- given callback function, previous 'respondToLogMessage' invocations
-- further up in the call stack will not get the messages anymore.
--
-- Use 'interceptLogMessages' if the messages shall be passed
-- any previous handler.
--
-- NOTE: The effects of this function are __lost__ when using
-- 'MonadBaseControl', 'MonadMask', 'MonadCatch' and 'MonadThrow'.
--
-- In contrast the functions based on modifying the 'LogWriter',
-- such as 'logTo' or 'censorLogs', are save to use in combination
-- with the aforementioned liftings.
respondToLogMessage
  :: forall r b
   . (Member Logs r)
  => (LogMessage -> Eff r ())
  -> Eff r b
  -> Eff r b
respondToLogMessage f e = askLogPredicate >>= fix step e
  where
    step :: (Eff r b -> LogPredicate -> Eff r b) ->  Eff r b -> LogPredicate -> Eff r b
    step k (E q (prj -> Just (WriteLogMessage !l))) lp = do
      f l
      respond_relay @Logs ret k (q ^$ ()) lp
    step k m lp = respond_relay @Logs ret k m lp
    ret x _lf = return x

-- | Change the 'LogMessages' using an effectful function.
--
-- This differs from 'respondToLogMessage' in that the intercepted messages will be
-- written either way, albeit in altered form.
--
-- NOTE: The effects of this function are __lost__ when using
-- 'MonadBaseControl', 'MonadMask', 'MonadCatch' and 'MonadThrow'.
--
-- In contrast the functions based on modifying the 'LogWriter',
-- such as 'logTo' or 'censorLogs', are save to use in combination
-- with the aforementioned liftings.
interceptLogMessages
  :: forall r b
   . (Member Logs r)
  => (LogMessage -> Eff r LogMessage)
  -> Eff r b
  -> Eff r b
interceptLogMessages f = respondToLogMessage (f >=> logMsg)

-- | Internal function.
sendLogMessageToLogWriter
  :: forall h e b .
     (LogsTo h e, LiftsLogWriter h e, Member Logs e)
  => Eff e b -> Eff e b
sendLogMessageToLogWriter = respondToLogMessage messageCallback
  where
    messageCallback msg = do
      lw <- askLogWriter
      liftLogWriter lw msg

-- * 'LogWriter' 'Reader'

-- | A 'Reader' for the 'LogWriter'
data LogWriterReader h v where
  AskLogWriter :: LogWriterReader h (LogWriter h)

instance Handle (LogWriterReader h) e a (LogWriter h -> k) where
  handle k q AskLogWriter lw = k (q ^$ lw) lw

instance forall h m r. (MonadBase m m, LiftedBase m r)
  => MonadBaseControl m (Eff (LogWriterReader h ': r)) where
    type StM (Eff (LogWriterReader h ': r)) a =  StM (Eff r) a
    liftBaseWith f = do
      lf <- askLogWriter
      raise (liftBaseWith (\runInBase -> f (runInBase . runLogWriterReader  lf)))
    restoreM = raise . restoreM

instance (LiftedBase m e, Catch.MonadThrow (Eff e))
  => Catch.MonadThrow (Eff (LogWriterReader h ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance (Applicative m, LiftedBase m e, Catch.MonadCatch (Eff e))
  => Catch.MonadCatch (Eff (LogWriterReader h ': e)) where
  catch effect handler = do
    lf <- askLogWriter
    let lower                   = runLogWriterReader  lf
        nestedEffects           = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (Applicative m, LiftedBase m e, Catch.MonadMask (Eff e))
  => Catch.MonadMask (Eff (LogWriterReader h ': e)) where
  mask maskedEffect = do
    lf <- askLogWriter
    let
      lower :: Eff (LogWriterReader h ': e) a -> Eff e a
      lower = runLogWriterReader  lf
    raise
        (Catch.mask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  uninterruptibleMask maskedEffect = do
    lf <- askLogWriter
    let
      lower :: Eff (LogWriterReader h ': e) a -> Eff e a
      lower = runLogWriterReader  lf
    raise
        (Catch.uninterruptibleMask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  generalBracket acquire release useIt = do
    lf <- askLogWriter
    let
      lower :: Eff (LogWriterReader h ': e) a -> Eff e a
      lower = runLogWriterReader  lf
    raise
        (Catch.generalBracket
          (lower acquire)
          (((.).(.)) lower release)
          (lower . useIt)
      )

-- | A constraint alias for effects that requires a 'LogWriterReader', as well as that the
-- contained 'LogWriterReader' has a 'LiftsLogWriter' instance.
type LogsTo h e = (Member Logs e, LiftsLogWriter h e, SetMember LogWriterReader (LogWriterReader h) e)

-- | Provide the 'LogWriter'
runLogWriterReader :: LogWriter h -> Eff (LogWriterReader h ': e) a -> Eff e a
runLogWriterReader e m = fix (handle_relay (\x _ -> return x)) m e


-- | Get the current 'LogWriter'.
askLogWriter :: SetMember LogWriterReader (LogWriterReader h) e => Eff e (LogWriter h)
askLogWriter = send AskLogWriter


-- | Change the current 'LogWriter'.
modifyLogWriter
  :: forall h e a. LogsTo h e => (LogWriter h -> LogWriter h) -> Eff e a -> Eff e a
modifyLogWriter f = localLogWriterReader . sendLogMessageToLogWriter
  where
    localLogWriterReader m =
      f <$> askLogWriter >>= fix (respond_relay @(LogWriterReader h) (\x _ -> return x)) m


-- | Replace the current 'LogWriter'.
setLogWriter :: forall h e a. LogsTo h e => LogWriter h -> Eff e a -> Eff e a
setLogWriter = modifyLogWriter  . const

-- | Modify the the 'LogMessage's written in the given sub-expression.
--
-- Note: This is equivalent to @'modifyLogWriter' . 'mappingLogWriter'@
censorLogs :: LogsTo h e => (LogMessage -> LogMessage) -> Eff e a -> Eff e a
censorLogs = modifyLogWriter . mappingLogWriter

-- | Modify the the 'LogMessage's written in the given sub-expression, as in 'censorLogs'
-- but with a effectful function.
--
-- Note: This is equivalent to @'modifyLogWriter' . 'mappingLogWriterM'@
censorLogsM
  :: (LogsTo h e, Monad h)
  => (LogMessage -> h LogMessage) -> Eff e a -> Eff e a
censorLogsM = modifyLogWriter . mappingLogWriterM

-- | Combine the effects of a given 'LogWriter' and the existing one.
--
-- @runLogWriterReader w1 . addLogWriter w2  == runLogWriterReader (\m -> w1 m >> w2 m)@
addLogWriter :: forall h e a .
     (HasCallStack, LogsTo h e, Monad h)
  => LogWriter h -> Eff e a -> Eff e a
addLogWriter lw2 = modifyLogWriter (\lw1 -> MkLogWriter (\m -> runLogWriter lw1 m >> runLogWriter lw2 m))

-- | Filter the 'LogMessage's written in the given sub-expression with the given
--   'LogPredicate'.
--
-- Note: This is equivalent to @'modifyLogWriter' . 'filteringLogWriter'@
filterLogs :: forall h e a .
     (HasCallStack, LogsTo h e, Monad h)
  => LogPredicate -> Eff e a -> Eff e a
filterLogs = modifyLogWriter . filteringLogWriter

-- ** Automatic thread id and timestamp setting

-- | Set the current thread id and time of each 'LogMessage' (at the moment the message is logged).
setThreadIdAndTimestamp
  :: (HasCallStack, LogsTo IO e, Lifted IO e)
  => Eff e a -> Eff e a
setThreadIdAndTimestamp =
  censorLogsM (setLogMessageThreadId >=> setLogMessageTimestamp)

-- ** Distance Based Log Message Filtering

-- | Increase the /distance/ of log messages by one.
-- Logs can be filtered by their distance with 'dropDistantLogMessages'
increaseLogMessageDistance
  :: forall h e a . (HasCallStack, LogsTo h e)
  => Eff e a -> Eff e a
increaseLogMessageDistance = censorLogs  (over lmDistance (+ 1))

-- | Drop all log messages with an 'lmDistance' greater than the given
-- value.
dropDistantLogMessages
  :: forall h e a . (HasCallStack, Monad h, LogsTo h e)
  => Int -> Eff e a -> Eff e a
dropDistantLogMessages maxDistance =
  filterLogs  (\lm -> lm ^. lmDistance <= maxDistance)

-- ** 'IO' based 'LogWriter's

-- | Open a file and add the 'LogWriter' in the 'LogWriterReader' tha appends the log messages to it.
withLogFileAppender
  :: ( Lifted IO e
     , LogsTo IO e
     , MonadBaseControl IO (Eff e)
     )
  => FilePath
  -> Eff e b
  -> Eff e b
withLogFileAppender fnIn e = liftBaseOp withOpenedLogFile (`addLogWriter` e)
  where
    withOpenedLogFile
      :: HasCallStack
      => (LogWriter IO -> IO a)
      -> IO a
    withOpenedLogFile ioE =
      Safe.bracket
          (do
            fnCanon <- canonicalizePath fnIn
            createDirectoryIfMissing True (takeDirectory fnCanon)
            h <- IO.openFile fnCanon IO.AppendMode
            IO.hSetBuffering h (IO.BlockBuffering (Just 1024))
            return h
          )
          (\h -> Safe.try @IO @Catch.SomeException (IO.hFlush h) >> IO.hClose h)
          (\h -> ioE (ioHandleLogWriter h))
