{-# LANGUAGE UndecidableInstances #-}
-- | A logging effect based on 'Control.Monad.Log.MonadLog'.
module Control.Eff.Log.Handler
  ( logMsg
  , mapLogMessages
  , filterLogMessages
  , traverseLogMessages
  , Logs()
  , HasLogging
  , runLogsFiltered
  , runLogs
  , askLogFilter
  , askLogWriter
  , localLogFilter
  , localLogWriter
  , increaseLogMessageDistance
  , dropDistantLogMessages
  , logWithSeverity
  , logEmergency
  , logAlert
  , logCritical
  , logError
  , logWarning
  , logNotice
  , logInfo
  , logDebug
  , ignoreLogs
  , traceLogs
  , LoggingAndIo
  , ioLogMessageHandler
  , withReopeningLogFileAppender
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
import           Control.Monad                   ( (>=>) )
import           Control.Monad.Base              ( MonadBase() )
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Trans.Control     ( MonadBaseControl
                                                   ( restoreM
                                                   , liftBaseWith
                                                   , StM
                                                   )
                                                 )
import           Data.Default
import           Data.Foldable                  ( traverse_ )
import           Data.Function                  ( fix )
import           GHC.Stack                      ( HasCallStack
                                                , callStack
                                                , withFrozenCallStack
                                                )
import           System.IO                      ( withFile
                                                , IOMode(AppendMode)
                                                , hPutStrLn
                                                , hSetBuffering
                                                , hClose
                                                , hFlush
                                                , openFile
                                                , BufferMode(BlockBuffering)
                                                )
import           System.Directory
import           System.FilePath
-- * Message Logging Effect


-- | This effect sends log messages.
-- There are three messages for the 'Logs' effect, that have these purposes:
--
-- 1. Retreiving a log filter function
-- 2. Retreiving the 'LogWriter'
-- 3. Writing a 'LogMessage'
--
-- These three parts are just loosely coupled; the 'Handle' instance
-- does not apply any filters, neither does the 'LogWriter'
data Logs h v where
  AskLogFilter
    :: Logs h (LogMessage -> Maybe LogMessage)
  AskLogWriter
    :: Logs h (LogWriter h)
  WriteLogMessage
    :: LogMessage -> Logs h ()

instance (LiftsLogWriter h e) => Handle (Logs h) e a ((LogMessage -> Maybe LogMessage) -> LogWriter h -> k) where
  handle handleNext handleResponseAndContinue request logMessageFilter logMessageWriter =
    case request of
      AskLogFilter ->
        handleNext (handleResponseAndContinue ^$ logMessageFilter) logMessageFilter logMessageWriter
      AskLogWriter ->
        handleNext (handleResponseAndContinue ^$ logMessageWriter) logMessageFilter logMessageWriter
      WriteLogMessage m ->
        handleNext (liftLogWriter logMessageWriter m >> handleResponseAndContinue ^$ ()) logMessageFilter logMessageWriter

-- | This instance allows lifting to the 'Logs' effect.
instance (MonadBase m m, LiftedBase m r, LiftsLogWriter m (Logs m ': r)) => MonadBaseControl m (Eff (Logs m ': r)) where
    type StM (Eff (Logs m ': r)) a =  StM (Eff r) a
    liftBaseWith f = do
      lf <- askLogFilter @m
      lw <- askLogWriter @m
      raise (liftBaseWith (\runInBase -> f (runInBase . runLogsFiltered lf lw)))
    restoreM = raise . restoreM

instance (LiftedBase m e, Catch.MonadThrow (Eff e))
  => Catch.MonadThrow (Eff (Logs m ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance (Applicative m, LiftedBase m e, Catch.MonadCatch (Eff e), LiftsLogWriter m (Logs m ': e))
  => Catch.MonadCatch (Eff (Logs m ': e)) where
  catch effect handler = do
    lf <- askLogFilter @m
    lw <- askLogWriter @m
    let lower                   = runLogsFiltered lf lw
        nestedEffects           = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (Applicative m, LiftedBase m e, Catch.MonadMask (Eff e), LiftsLogWriter m (Logs m ': e))
  => Catch.MonadMask (Eff (Logs m ': e)) where
  mask maskedEffect = do
    lf <- askLogFilter @m
    lw <- askLogWriter @m
    let
      lower :: Eff (Logs m ': e) a -> Eff e a
      lower = runLogsFiltered lf lw
    raise
        (Catch.mask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  uninterruptibleMask maskedEffect = do
    lf <- askLogFilter @m
    lw <- askLogWriter @m
    let
      lower :: Eff (Logs m ': e) a -> Eff e a
      lower = runLogsFiltered lf lw
    raise
        (Catch.uninterruptibleMask
          (\nestedUnmask -> lower
            (maskedEffect
              ( raise . nestedUnmask . lower )
            )
          )
        )
  generalBracket acquire release useIt = do
    lf <- askLogFilter @m
    lw <- askLogWriter @m
    let
      lower :: Eff (Logs m ': e) a -> Eff e a
      lower = runLogsFiltered lf lw
    raise
        (Catch.generalBracket
          (lower acquire)
          (((.).(.)) lower release)
          (lower . useIt)
      )

-- | A convenient alias.
type HasLogging h e = SetMember Logs (Logs h) e

-- | Get the current 'Logs' filter/transformer function.
askLogFilter :: forall h e . (Member (Logs h) e) => Eff e (LogMessage -> Maybe LogMessage)
askLogFilter = send @(Logs h) AskLogFilter

-- | Get the current 'Logs' writer function.
askLogWriter :: forall h e . (Member (Logs h) e) => Eff e (LogWriter h)
askLogWriter = send AskLogWriter

-- | Replace the current log filter.
localLogFilter
  :: forall h e b
   . (LiftsLogWriter h e,  Member (Logs h) e)
  => (LogMessage -> Maybe LogMessage)
  -> Eff e b
  -> Eff e b
localLogFilter lfIn actionThatLogs = do
  lwIn <- askLogWriter @h
  (fix (respond_relay @(Logs h) @e (\x _ _ -> return x))) actionThatLogs lfIn lwIn

-- | Replace the current 'LogWriter'.
localLogWriter
  :: forall h e b
   . (LiftsLogWriter h e, Member (Logs h) e)
  => LogWriter h
  -> Eff e b
  -> Eff e b
localLogWriter lwIn actionThatLogs = do
  lfIn <- askLogFilter  @h
  (fix (respond_relay @(Logs h) @e (\x _ _ -> return x))) actionThatLogs lfIn lwIn

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
-- The second argument is the 'LogWriter' to send message to.
runLogsFiltered :: LiftsLogWriter h (Logs h ': e) => (LogMessage -> Maybe LogMessage) -> LogWriter h -> Eff (Logs h ': e) b -> Eff e b
runLogsFiltered logMessageFilter logMessageWriter logClient = do
  (res, logEff) <- fix (handle_relay (\a _f _w -> return (a, return ()))) logClient logMessageFilter logMessageWriter
  logEff
  return res

-- | Handle the 'Logs' effect, like 'runLogsFiltered' does, but start with a permissive log filter
-- that removes no log messages.
runLogs :: LiftsLogWriter h (Logs h ': e) => LogWriter h -> Eff (Logs h ': e) b -> Eff e b
runLogs = runLogsFiltered Just

-- ** Filter and Transform Log Messages

-- | Map a pure function over log messages.
mapLogMessages
  :: forall h r b
   . (LiftsLogWriter h r, Member (Logs h) r)
  => (LogMessage -> LogMessage)
  -> Eff r b
  -> Eff r b
mapLogMessages f = localLogFilter @h (Just . f)

-- * Filtering

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
  :: forall h r b
   . (LiftsLogWriter h r, Member (Logs h) r)
  => (LogMessage -> Bool)
  -> Eff r b
  -> Eff r b
filterLogMessages predicate logEff  = do
  old <- askLogFilter @h
  localLogFilter @h (\m -> if predicate m then old m else Nothing) logEff

-- | Traverse over log messages.
traverseLogMessages
  :: forall h r b
   . (Monad h, LiftsLogWriter h r, Member (Logs h) r)
  => (LogMessage -> h LogMessage)
  -> Eff r b
  -> Eff r b
traverseLogMessages f e = do
  previousWriter <- askLogWriter
  logFilter <- askLogFilter @h
  localLogWriter (MkLogWriter (traverse_ (f >=> runLogWriter previousWriter) . logFilter)) e

-- ** Distance Based Log Message Filtering

-- | Increase the /distance/ of log messages by one.
-- Logs can be filtered by their distance with 'dropDistantLogMessages'
increaseLogMessageDistance
  :: forall h e a . (HasCallStack, LiftsLogWriter h e, Member (Logs h) e) => Eff e a -> Eff e a
increaseLogMessageDistance = mapLogMessages @h (over lmDistance (+ 1))

-- | Drop all log messages with an 'lmDistance' greater than the given
-- value.
dropDistantLogMessages :: forall h e a . (HasCallStack, LiftsLogWriter h e, Member (Logs h) e) => Int -> Eff e a -> Eff e a
dropDistantLogMessages maxDistance =
  filterLogMessages @h (\lm -> lm ^. lmDistance <= maxDistance)


-- * Logging functions

-- | Log a message. The message is reduced to normal form (strict).
logMsg :: forall h e m . (Member (Logs h) e, ToLogMessage m) => m -> Eff e ()
logMsg (force . toLogMessage -> msgIn) = do
  lf <- askLogFilter @h
  traverse_
    (send @(Logs h) . WriteLogMessage)
    (lf msgIn)

-- | Log a 'String' as 'LogMessage' with a given 'Severity'.
logWithSeverity
  :: forall m e .
     ( HasCallStack
     , SetMember Logs (Logs m) e
     )
  => Severity
  -> String
  -> Eff e ()
logWithSeverity !s =
  withFrozenCallStack
    $ logMsg @m
    . setCallStack callStack
    . set lmSeverity s
    . flip (set lmMessage) def

-- | Log a 'String' as 'emergencySeverity'.
logEmergency
  :: forall m e .
     ( HasCallStack
     , SetMember Logs (Logs m) e
     )
  => String
  -> Eff e ()
logEmergency = withFrozenCallStack (logWithSeverity @m emergencySeverity)

-- | Log a message with 'alertSeverity'.
logAlert
  :: forall m e .
     ( HasCallStack
     , SetMember Logs (Logs m) e
     )
  => String
  -> Eff e ()
logAlert = withFrozenCallStack (logWithSeverity @m alertSeverity)

-- | Log a 'criticalSeverity' message.
logCritical
  :: forall m e .
     ( HasCallStack
     , SetMember Logs (Logs m) e
     )
  => String
  -> Eff e ()
logCritical = withFrozenCallStack (logWithSeverity @m criticalSeverity)

-- | Log a 'errorSeverity' message.
logError
  :: forall m e .
     ( HasCallStack
     , SetMember Logs (Logs m) e
     )
  => String
  -> Eff e ()
logError = withFrozenCallStack (logWithSeverity @m errorSeverity)

-- | Log a 'warningSeverity' message.
logWarning
  :: forall m e .
     ( HasCallStack
     , SetMember Logs (Logs m) e
     )
  => String
  -> Eff e ()
logWarning = withFrozenCallStack (logWithSeverity @m warningSeverity)

-- | Log a 'noticeSeverity' message.
logNotice
  :: forall m e .
     ( HasCallStack
     , SetMember Logs (Logs m) e
     )
  => String
  -> Eff e ()
logNotice = withFrozenCallStack (logWithSeverity @m noticeSeverity)

-- | Log a 'informationalSeverity' message.
logInfo
  :: forall m e .
     ( HasCallStack
     , SetMember Logs (Logs m) e
     )
  => String
  -> Eff e ()
logInfo = withFrozenCallStack (logWithSeverity @m informationalSeverity)

-- | Log a 'debugSeverity' message.
logDebug
  :: forall m e .
     ( HasCallStack
     , SetMember Logs (Logs m) e
     )
  => String
  -> Eff e ()
logDebug = withFrozenCallStack (logWithSeverity @m debugSeverity)

-- * Log Messages Writing

-- | Handle the 'Logs' effect, start with a filter that discards all messages.
--
-- This function has the semantics of @'runLogsFiltered' (const Nothing)@.
ignoreLogs :: forall r a . Eff (Logs DiscardLogs ': r) a -> Eff r a
ignoreLogs = runLogsFiltered (const Nothing) discardLogMessages

-- | Trace all log messages using 'traceM'. The message value is
-- converted to 'String' using the given function.
traceLogs :: Eff (Logs TraceLogs ': r) a -> Eff r a
traceLogs = runLogsFiltered Just traceLogMessages

-- ** Writing LogMessages to IO

-- | The concrete list of 'Eff'ects for logging with an IO based 'LogWriter'.
type LoggingAndIo = '[Logs IO, Lift IO]

-- | Use 'ioLogMessageWriter' to /handle/ logging using 'handleLogs'.
ioLogMessageHandler
  :: (HasCallStack, Lifted IO e)
  => (String -> IO ())
  -> Eff (Logs IO ': e) a
  -> Eff e a
ioLogMessageHandler delegatee =
  runLogs (ioLogMessageWriter delegatee)

-- ** File Based Logging

withLogFileAppender
  :: HasCallStack
  => FilePath
  -> (LogWriter IO -> IO a)
  -> IO a
withLogFileAppender fnIn e =
  Safe.bracket
      (do
        fnCanon <- canonicalizePath fnIn
        createDirectoryIfMissing True (takeDirectory fnCanon)
        h <- openFile fnCanon AppendMode
        hSetBuffering h (BlockBuffering (Just 1024))
        return h
      )
      (\h -> Safe.try @IO @Catch.SomeException (hFlush h) >> hClose h)
      (\h -> e (ioLogMessageWriter (hPutStrLn h)))

withReopeningLogFileAppender
  :: (Lifted IO e, Member (Logs IO) e)
  => FilePath
  -> Eff e a
  -> Eff e a
withReopeningLogFileAppender fnIn =
  localLogWriter (ioLogMessageWriter fileAppenderSimple)
 where
  fileAppenderSimple = \msg -> withFile fnIn AppendMode (`hPutStrLn` msg)

