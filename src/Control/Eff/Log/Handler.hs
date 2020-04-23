{-# LANGUAGE UndecidableInstances #-}

-- | A memory efficient, streaming, logging effect with support for
-- efficiently not logging when no logs are required.
--
-- Good support for logging to a file or to the network,
-- as well as asynchronous logging in another thread.
module Control.Eff.Log.Handler
  ( -- * FilteredLogging API

    -- ** Sending Log Messages
    Logs (..),
    LogEventSender (..),
    logEmergency,
    logAlert,
    logCritical,
    logError,
    logWarning,
    logNotice,
    logInfo,
    logDebug,
    logCallStack,
    logMultiLine,

    -- ** Log Message Pre-Filtering #LogPredicate#
    whitelistLogEvents,
    blacklistLogEvents,
    setLogPredicate,
    modifyLogPredicate,
    askLogPredicate,

    -- * Log Handling API

    -- ** Writing Logs
    setLogWriter,
    addLogWriter,
    modifyLogWriter,

    -- *** Log Message Modification
    censorLogs,
    censorLogsIo,

    -- ** 'Logs' Effect Handling
    FilteredLogging,
    IoLogging,
    LoggingAndIo,
    withLogging,
    withoutLogging,

    -- ** Low-Level API for Custom Extensions

    -- *** Log Message Interception
    runLogs,
    runLogsWithoutLogging,
    respondToLogEvent,
    interceptLogMessages,
  )
where

import Control.DeepSeq
import Control.Eff as Eff
import Control.Eff.Extend
import Control.Eff.Log.Message
import Control.Eff.Log.Writer
import qualified Control.Exception.Safe as Safe
import Control.Lens
import Control.Monad ((>=>), when)
import Control.Monad.Base (MonadBase ())
import qualified Control.Monad.Catch as Catch
import Control.Monad.Trans.Control
  ( MonadBaseControl
      ( StM,
        liftBaseWith,
        restoreM
      ),
  )
import Data.Default
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.Hashable
import Data.Text as T
import GHC.Stack
  ( HasCallStack,
    callStack,
    prettyCallStack,
    withFrozenCallStack,
  )
import Text.Printf (printf)

-- | Something that consumes a 'LogEvent'.
--
-- This type class seems overly general, but it has to be,
-- in order to allow writing log statements with many items
-- that should be part of a log message simply like this:
--
-- >>> sendLogEvent (debugMessage "started: ") myPid " after receiving: " lastMsg " threshold is: " currentThresh
--
-- Which of course is sugar-coated by  functions like 'logInfo', 'logDebug', 'logError', ... to be:
--
-- >>> logDebug "started: " myPid " after receiving: " lastMsg " threshold is: " currentThresh
--
-- @since 1.0.0
class LogEventSender a where
  -- | Log a 'LogEvent'.
  --
  -- Dispatch 'LogEvent's that match the 'LogPredicate'.
  --
  -- The 'LogEvent's are evaluated using 'deepseq', __after__ they pass the 'LogPredicate'.
  sendLogEvent :: HasCallStack => LogEvent -> a

-- | Log a 'LogEvent'.
--
-- Dispatch 'LogEvent's that match the 'LogPredicate'.
--
-- The 'LogEvent's are evaluated using 'deepseq', __after__ they pass the 'LogPredicate'.
instance (a ~ (), Member Logs e) => LogEventSender (Eff e a) where
  sendLogEvent = withFrozenCallStack $ \msgIn -> do
    lf <- send AskLogFilter
    when (lf msgIn) $
      msgIn `deepseq` send @Logs (WriteLogMessage msgIn)

instance (ToLogMsg x, LogEventSender a) => LogEventSender (x -> a) where
  sendLogEvent =
    withFrozenCallStack $ \logEvt x ->
      sendLogEvent (logEvt & logEventMessage %~ (<> toLogMsg x))

-- | This effect sends 'LogEvent's and is a reader for a 'LogPredicate'.
--
-- Logs are sent via 'sendLogEvent';
-- for more information about log predicates, see "Control.Eff.Log#LogPredicate"
--
-- This effect is handled via 'withLogging'.
data Logs v where
  AskLogFilter ::
    Logs
      LogPredicate
  WriteLogMessage ::
    !LogEvent ->
    Logs ()

instance forall e a k. Handle Logs e a (LogPredicate -> k) where
  handle h q AskLogFilter p = h (q ^$ p) p
  handle h q (WriteLogMessage _) p = h (q ^$ ()) p

-- | Compose and dispatch a 'LogEvent' with 'emergencySeverity'.
--
-- @since 1.0.0
logEmergency :: (HasCallStack, LogEventSender a) => a
logEmergency = withFrozenCallStack $ sendLogEvent emptyLogMsg
  where
    emptyLogMsg = def & logEventSeverity .~ emergencySeverity

-- | Compose and dispatch a 'LogEvent' with 'alertSeverity'.
--
-- @since 1.0.0
logAlert :: (HasCallStack, LogEventSender a) => a
logAlert = withFrozenCallStack $ sendLogEvent emptyLogMsg
  where
    emptyLogMsg = def & logEventSeverity .~ alertSeverity

-- | Compose and dispatch a 'LogEvent' with 'criticalSeverity'.
--
-- @since 1.0.0
logCritical :: (HasCallStack, LogEventSender a) => a
logCritical = withFrozenCallStack $ sendLogEvent emptyLogMsg
  where
    emptyLogMsg = def & logEventSeverity .~ criticalSeverity

-- | Compose and dispatch a 'LogEvent' with 'errorSeverity'.
--
-- @since 1.0.0
logError :: (HasCallStack, LogEventSender a) => a
logError = withFrozenCallStack $ sendLogEvent emptyLogMsg
  where
    emptyLogMsg = def & logEventSeverity .~ errorSeverity

-- | Compose and dispatch a 'LogEvent' with 'warningSeverity'.
--
-- @since 1.0.0
logWarning :: (HasCallStack, LogEventSender a) => a
logWarning = withFrozenCallStack $ sendLogEvent emptyLogMsg
  where
    emptyLogMsg = def & logEventSeverity .~ warningSeverity

-- | Compose and dispatch a 'LogEvent' with 'noticeSeverity'.
--
-- @since 1.0.0
logNotice :: (HasCallStack, LogEventSender a) => a
logNotice = withFrozenCallStack $ sendLogEvent emptyLogMsg
  where
    emptyLogMsg = def & logEventSeverity .~ noticeSeverity

-- | Compose and dispatch a 'LogEvent' with 'informationalSeverity'.
--
-- @since 1.0.0
logInfo :: (HasCallStack, LogEventSender a) => a
logInfo = withFrozenCallStack $ sendLogEvent emptyLogMsg
  where
    emptyLogMsg = def & logEventSeverity .~ informationalSeverity

-- | Compose and dispatch a 'LogEvent' with 'debugSeverity'.
--
-- @since 1.0.0
logDebug :: (HasCallStack, LogEventSender a) => a
logDebug = withFrozenCallStack $ sendLogEvent emptyLogMsg
  where
    emptyLogMsg = def & logEventSeverity .~ debugSeverity

-- $PredefinedPredicates
-- == Log Message Predicates
--
-- These are the predefined 'LogPredicate's:
--
--  * 'allLogEvents'
--  * 'noLogEvents'
--  * 'logEventSeverityIsAtLeast'
--  * 'logEventSeverityIs'
--  * 'logEventMessageStartsWith'
--  * 'discriminateByAppName'
--
-- To find out how to use these predicates,
-- goto "Control.Eff.Log#LogPredicate"

-- | This instance allows lifting the 'Logs' effect into a base monad, e.g. 'IO'.
-- This instance needs a 'LogWriterReader' in the base monad,
-- that is capable to handle 'sendLogEvent' invocations.
instance
  forall m e.
  (MonadBase m IO, MonadBaseControl IO (Eff e), LiftedBase m e, Lifted IO e, IoLogging (Logs ': e)) =>
  MonadBaseControl m (Eff (Logs ': e))
  where
  type StM (Eff (Logs ': e)) a = StM (Eff e) a
  liftBaseWith f = do
    lf <- askLogPredicate
    raise (liftBaseWith (\runInBase -> f (runInBase . runLogs lf)))
  restoreM = raise . restoreM

instance
  (LiftedBase m e, Catch.MonadThrow (Eff e)) =>
  Catch.MonadThrow (Eff (Logs ': e))
  where
  throwM exception = raise (Catch.throwM exception)

instance
  (Applicative m, MonadBaseControl IO (Eff e), LiftedBase m e, Catch.MonadCatch (Eff e), IoLogging (Logs ': e), Lifted IO e) =>
  Catch.MonadCatch (Eff (Logs ': e))
  where
  catch effect handler = do
    lf <- askLogPredicate
    let lower = runLogs lf
        nestedEffects = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance
  (Applicative m, MonadBaseControl IO (Eff e), LiftedBase m e, Catch.MonadMask (Eff e), IoLogging (Logs ': e), Lifted IO e) =>
  Catch.MonadMask (Eff (Logs ': e))
  where
  mask maskedEffect = do
    lf <- askLogPredicate
    let lower :: Eff (Logs ': e) a -> Eff e a
        lower = runLogs lf
    raise
      ( Catch.mask
          ( \nestedUnmask ->
              lower
                ( maskedEffect
                    (raise . nestedUnmask . lower)
                )
          )
      )
  uninterruptibleMask maskedEffect = do
    lf <- askLogPredicate
    let lower :: Eff (Logs ': e) a -> Eff e a
        lower = runLogs lf
    raise
      ( Catch.uninterruptibleMask
          ( \nestedUnmask ->
              lower
                ( maskedEffect
                    (raise . nestedUnmask . lower)
                )
          )
      )
  generalBracket acquire release useIt = do
    lf <- askLogPredicate
    let lower :: Eff (Logs ': e) a -> Eff e a
        lower = runLogs lf
    raise
      ( Catch.generalBracket
          (lower acquire)
          (((.) . (.)) lower release)
          (lower . useIt)
      )

-- | A constraint that requires @'Logs' e@ and @'Lifted' 'IO' e@.
--
-- Provided by 'withLogging' and 'runLogs'.
--
-- It contains 'FilteredLogging' and allows in addition:
--
-- * censorLogsIo
--
-- Don't infect everything with 'IO', if you can fall back to
-- 'FilteredLogging'.
--
-- @since 0.24.0
type IoLogging e = (FilteredLogging e, Lifted IO e)

-- | A constraint that requires 'Logs' and 'LogWriterReader',
-- and hence supports the functions to filter and modify
-- logs:
--
-- * setLogWriter
-- * addLogWriter
-- * modifyLogWriter
-- * censorLogs
--
-- Provided by 'withLogging', 'runLogs', and also
-- 'withoutLogging' and 'runLogsWithoutLogging'.
--
-- @since 0.31.0
type FilteredLogging e = (Member Logs e, Member LogWriterReader e)

-- | The concrete list of 'Eff'ects for logging with a
-- 'LogWriter', and a 'LogWriterReader'.
--
-- This also provides both 'IoLogging' and 'FilteredLogging'.
type LoggingAndIo = '[Logs, LogWriterReader, Lift IO]

-- | Handle the 'Logs' and 'LogWriterReader' effects.
--
-- It installs the given 'LogWriter', which determines the underlying
-- 'LogWriter' type parameter.
--
-- Example:
--
-- > exampleWithLogging :: IO ()
-- > exampleWithLogging =
-- >     runLift
-- >   $ withLogging consoleLogWriter
-- >   $ logDebug "Oh, hi there"
--
-- This provides the 'IoLogging' and 'FilteredLogging' effects.
--
-- See also 'runLogs'.
withLogging :: Lifted IO e => LogWriter -> Eff (Logs ': LogWriterReader ': e) a -> Eff e a
withLogging lw = runLogWriterReader lw . runLogs allLogEvents

-- | Handles the 'Logs' and 'LogWriterReader' effects, while not invoking the 'LogWriter' at all.
--
-- There is no way to get log output when this logger is used.
--
-- Example:
--
-- > exampleWithSomeLogging :: ()
-- > exampleWithSomeLogging =
-- >     run
-- >   $ withoutLogging
-- >   $ logDebug "Oh, hi there" -- Nothing written
--
-- This provides the 'FilteredLogging' effect.
--
-- See also 'runLogsWithoutLogging'.
withoutLogging :: Eff (Logs ': LogWriterReader ': e) a -> Eff e a
withoutLogging = runLogWriterReader mempty . runLogsWithoutLogging noLogEvents

-- | Raw handling of the 'Logs' effect.
-- Exposed for custom extensions, if in doubt use 'withLogging'.
runLogs ::
  forall e b.
  ( Member LogWriterReader (Logs ': e),
    Lifted IO e
  ) =>
  LogPredicate ->
  Eff (Logs ': e) b ->
  Eff e b
runLogs p m =
  fix (handle_relay (\a _ -> return a)) (sendLogMessageToLogWriter m) p

-- | Raw handling of the 'Logs' effect.
-- Exposed for custom extensions, if in doubt use 'withoutLogging'.
runLogsWithoutLogging ::
  forall e b.
  ( Member LogWriterReader (Logs ': e)
  ) =>
  LogPredicate ->
  Eff (Logs ': e) b ->
  Eff e b
runLogsWithoutLogging p m =
  fix (handle_relay (\a _ -> return a)) m p

-- | Log the current 'callStack' using the given 'Severity'.
--
-- @since 0.30.0
logCallStack :: forall e. (HasCallStack, Member Logs e) => Severity -> Eff e ()
logCallStack =
  withFrozenCallStack $ \s ->
    let stackTraceLines = MkLogMsg <$> T.lines (pack (prettyCallStack callStack))
     in logMultiLine s stackTraceLines

-- | Issue a log statement for each item in the list prefixed with a line number and a message hash.
--
-- When several concurrent processes issue log statements, multi line log statements are often
-- interleaved.
--
-- In order to make the logs easier to read, this function will count the items and calculate a unique
-- hash and prefix each message, so a user can grep to get all the lines of an interleaved,
-- multi-line log message.
--
-- @since 0.30.0
logMultiLine ::
  forall e.
  ( HasCallStack,
    Member Logs e
  ) =>
  Severity ->
  [LogMsg] ->
  Eff e ()
logMultiLine =
  withFrozenCallStack $ \s messageLines -> do
    let msgHash = T.pack $ printf "%06X" $ hash messageLines `mod` 0x1000000
        messageLinesWithLineNum =
          let messageLineCount = Prelude.length messageLines
              messageLineCountString = packLogMsg (show messageLineCount)
              printLineNum i =
                let !i' = packLogMsg (show i)
                 in MkLogMsg msgHash <> packLogMsg ": " <> i' <> packLogMsg "/" <> messageLineCountString <> packLogMsg ":  "
           in Prelude.zipWith (<>) (printLineNum <$> [1 :: Int ..]) messageLines
    traverse_ (sendLogEvent . set logEventSeverity s . infoMessage) messageLinesWithLineNum

-- | Get the current 'Logs' filter/transformer function.
--
-- See "Control.Eff.Log#LogPredicate"
askLogPredicate :: forall e. (Member Logs e) => Eff e LogPredicate
askLogPredicate = send @Logs AskLogFilter

-- | Keep only those messages, for which a predicate holds.
--
-- E.g. to keep only messages which begin with @"OMG"@:
--
-- > exampleSetLogWriter :: IO Int
-- > exampleSetLogWriter =
-- >     runLift
-- >   $ withLogging consoleLogWriter
-- >   $ do sendLogEvent "test"
-- >        setLogPredicate (\ msg -> case view logEventMessage msg of
-- >                                   'O':'M':'G':_ -> True
-- >                                   _             -> False)
-- >                          (do sendLogEvent "this message will not be logged"
-- >                              sendLogEvent "OMG logged"
-- >                              return 42)
--
-- In order to also delegate to the previous predicate, use 'modifyLogPredicate'
--
-- See "Control.Eff.Log#LogPredicate"
setLogPredicate ::
  forall r b.
  (Member Logs r, HasCallStack) =>
  LogPredicate ->
  Eff r b ->
  Eff r b
setLogPredicate = modifyLogPredicate . const

-- | Change the 'LogPredicate'.
--
-- Other than 'setLogPredicate' this function allows to include the previous predicate, too.
--
-- For to discard all messages currently no satisfying the predicate and also all messages
-- that are to long:
--
-- @
-- modifyLogPredicate (\previousPredicate msg -> previousPredicate msg && length (logEventMessage msg) < 29 )
--                    (do sendLogEvent "this message will not be logged"
--                        sendLogEvent "this message might be logged")
-- @
--
-- See "Control.Eff.Log#LogPredicate"
modifyLogPredicate ::
  forall e b.
  (Member Logs e, HasCallStack) =>
  (LogPredicate -> LogPredicate) ->
  Eff e b ->
  Eff e b
modifyLogPredicate lpIn e = askLogPredicate >>= fix step e . lpIn
  where
    ret x _ = return x
    step :: (Eff e b -> LogPredicate -> Eff e b) -> Eff e b -> LogPredicate -> Eff e b
    step k (E q (prj -> Just (WriteLogMessage !l))) lp = do
      sendLogEvent l
      respond_relay @Logs ret k (q ^$ ()) lp
    step k m lp = respond_relay @Logs ret k m lp

-- | Include 'LogEvent's that match a 'LogPredicate'.
--
-- @whitelistLogEvents p@ allows log message to be logged if @p m@
--
-- Although it is enough if the previous predicate holds.
-- See 'blacklistLogEvents' and 'modifyLogPredicate'.
--
-- See "Control.Eff.Log#LogPredicate"
whitelistLogEvents ::
  forall e a.
  (Member Logs e) =>
  LogPredicate ->
  Eff e a ->
  Eff e a
whitelistLogEvents p = modifyLogPredicate (\p' m -> p' m || p m)

-- | Exclude 'LogEvent's that match a 'LogPredicate'.
--
-- @blacklistLogEvents p@ discards logs if @p m@
--
-- Also the previous predicate must also hold for a
-- message to be logged.
-- See 'blacklistLogEvents' and 'modifyLogPredicate'.
--
-- See "Control.Eff.Log#LogPredicate"
blacklistLogEvents ::
  forall e a.
  (Member Logs e) =>
  LogPredicate ->
  Eff e a ->
  Eff e a
blacklistLogEvents p = modifyLogPredicate (\p' m -> not (p m) && p' m)

-- | Consume log messages.
--
-- Exposed for custom extensions, if in doubt use 'withLogging'.
--
-- Respond to all 'LogEvent's logged from the given action,
-- up to any 'MonadBaseControl' liftings.
--
-- Note that all logging is done through 'sendLogEvent' and that means
-- only messages passing the 'LogPredicate' are received.
--
-- The 'LogEvent's are __consumed__ once they are passed to the
-- given callback function, previous 'respondToLogEvent' invocations
-- further up in the call stack will not get the messages anymore.
--
-- Use 'interceptLogMessages' if the messages shall be passed
-- any previous handler.
--
-- NOTE: The effects of this function are __lost__ when using
-- 'MonadBaseControl', 'Catch.MonadMask', 'Catch.MonadCatch' and 'Catch.MonadThrow'.
--
-- In contrast the functions based on modifying the 'LogWriter',
-- such as 'addLogWriter' or 'censorLogs', are save to use in combination
-- with the aforementioned liftings.
respondToLogEvent ::
  forall r b.
  (Member Logs r) =>
  (LogEvent -> Eff r ()) ->
  Eff r b ->
  Eff r b
respondToLogEvent f e = askLogPredicate >>= fix step e
  where
    step :: (Eff r b -> LogPredicate -> Eff r b) -> Eff r b -> LogPredicate -> Eff r b
    step k (E q (prj -> Just (WriteLogMessage !l))) lp = do
      f l
      respond_relay @Logs ret k (q ^$ ()) lp
    step k m lp = respond_relay @Logs ret k m lp
    ret x _lf = return x

-- | Change the 'LogEvent's using an effectful function.
--
-- Exposed for custom extensions, if in doubt use 'withLogging'.
--
-- This differs from 'respondToLogEvent' in that the intercepted messages will be
-- written either way, albeit in altered form.
--
-- NOTE: The effects of this function are __lost__ when using
-- 'MonadBaseControl', 'Catch.MonadMask', 'Catch.MonadCatch' and 'Catch.MonadThrow'.
--
-- In contrast the functions based on modifying the 'LogWriter',
-- such as 'addLogWriter' or 'censorLogs', are save to use in combination
-- with the aforementioned liftings.
interceptLogMessages ::
  forall r b.
  (Member Logs r) =>
  (LogEvent -> Eff r LogEvent) ->
  Eff r b ->
  Eff r b
interceptLogMessages f = respondToLogEvent (f >=> sendLogEvent)

-- | Internal function.
sendLogMessageToLogWriter :: IoLogging e => Eff e b -> Eff e b
sendLogMessageToLogWriter = respondToLogEvent liftWriteLogMessage

-- | Change the current 'LogWriter'.
modifyLogWriter :: IoLogging e => (LogWriter -> LogWriter) -> Eff e a -> Eff e a
modifyLogWriter f = localLogWriterReader f . sendLogMessageToLogWriter

-- | Replace the current 'LogWriter'.
-- To add an additional log message consumer use 'addLogWriter'
setLogWriter :: IoLogging e => LogWriter -> Eff e a -> Eff e a
setLogWriter = modifyLogWriter . const

-- | Modify the the 'LogEvent's written in the given sub-expression.
--
-- Note: This is equivalent to @'modifyLogWriter' . 'mappingLogWriter'@
censorLogs :: IoLogging e => (LogEvent -> LogEvent) -> Eff e a -> Eff e a
censorLogs = modifyLogWriter . mappingLogWriter

-- | Modify the the 'LogEvent's written in the given sub-expression, as in 'censorLogs'
-- but with a effectful function.
--
-- Note: This is equivalent to @'modifyLogWriter' . 'mappingLogWriterIO'@
censorLogsIo :: IoLogging e => (LogEvent -> IO LogEvent) -> Eff e a -> Eff e a
censorLogsIo = modifyLogWriter . mappingLogWriterIO

-- | Combine the effects of a given 'LogWriter' and the existing one.
--
-- > import Data.Text    as T
-- > import Data.Text.IO as T
-- >
-- > exampleAddLogWriter :: IO ()
-- > exampleAddLogWriter = go >>= T.putStrLn
-- >  where go = fmap (unlines . map renderLogEventConsoleLog . snd)
-- >               $  runLift
-- >               $  runCaptureLogWriter
-- >               $  withLogging captureLogWriter
-- >               $  addLogWriter (mappingLogWriter (logEventMessage %~ ("CAPTURED "++)) captureLogWriter)
-- >               $  addLogWriter (filteringLogWriter severeMessages (mappingLogWriter (logEventMessage %~ ("TRACED "++)) debugTraceLogWriter))
-- >               $  do
-- >                     logEmergency "test emergencySeverity 1"
-- >                     logCritical "test criticalSeverity 2"
-- >                     logAlert "test alertSeverity 3"
-- >                     logError "test errorSeverity 4"
-- >                     logWarning "test warningSeverity 5"
-- >                     logInfo "test informationalSeverity 6"
-- >                     logDebug "test debugSeverity 7"
-- >        severeMessages = view (logEventSeverity . to (<= errorSeverity))
-- >
addLogWriter :: IoLogging e => LogWriter -> Eff e a -> Eff e a
addLogWriter lw2 = modifyLogWriter (\lw1 -> MkLogWriter (\m -> runLogWriter lw1 m >> runLogWriter lw2 m))
