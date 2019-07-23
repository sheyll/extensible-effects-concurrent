{-# LANGUAGE UndecidableInstances #-}
-- | A memory efficient, streaming, logging effect with support for
-- efficiently not logging when no logs are required.
--
-- Good support for logging to a file or to the network,
-- as well as asynchronous logging in another thread.
module Control.Eff.Log.Handler
  ( -- * FilteredLogging API
    -- ** Sending Log Messages
    logMsg
  , logWithSeverity
  , logWithSeverity'
  , logEmergency
  , logEmergency'
  , logAlert
  , logAlert'
  , logCritical
  , logCritical'
  , logError
  , logError'
  , logWarning
  , logWarning'
  , logNotice
  , logNotice'
  , logInfo
  , logInfo'
  , logDebug
  , logDebug'
  , logCallStack
  , logMultiLine
  , logMultiLine'

  -- ** Log Message Pre-Filtering #LogPredicate#
  , includeLogMessages
  , excludeLogMessages
  , setLogPredicate
  , modifyLogPredicate
  , askLogPredicate

  -- * Log Handling API

  -- ** Writing Logs
  , setLogWriter
  , addLogWriter
  , modifyLogWriter

  -- *** Log Message Modification
  , censorLogs
  , censorLogsIo

  -- ** 'Logs' Effect Handling
  , Logs()
  , FilteredLogging
  , IoLogging
  , LoggingAndIo
  , withLogging
  , withoutLogging

  -- ** Low-Level API for Custom Extensions
  -- *** Log Message Interception
  , runLogs
  , runLogsWithoutLogging
  , respondToLogMessage
  , interceptLogMessages

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
import           Data.Hashable
import           Data.Text                     as T
import           GHC.Stack                      ( HasCallStack
                                                , callStack
                                                , withFrozenCallStack
                                                , prettyCallStack
                                                )
import Data.Foldable                           ( traverse_ )
import Text.Printf                             ( printf )

-- | This effect sends 'LogMessage's and is a reader for a 'LogPredicate'.
--
-- Logs are sent via 'logMsg';
-- for more information about log predicates, see "Control.Eff.Log#LogPredicate"
--
-- This effect is handled via 'withLogging'.
data Logs v where
  AskLogFilter
    :: Logs LogPredicate
  WriteLogMessage
    :: !LogMessage -> Logs ()

instance forall e a k. Handle Logs e a (LogPredicate -> k) where
  handle h q AskLogFilter p         = h (q ^$ p ) p
  handle h q (WriteLogMessage _) p  = h (q ^$ ()) p

-- | This instance allows lifting the 'Logs' effect into a base monad, e.g. 'IO'.
-- This instance needs a 'LogWriterReader' in the base monad,
-- that is capable to handle 'logMsg' invocations.
instance forall m e. (MonadBase m IO, MonadBaseControl IO (Eff e), LiftedBase m e, Lifted IO e, IoLogging (Logs ': e))
  => MonadBaseControl m (Eff (Logs ': e)) where
    type StM (Eff (Logs ': e)) a =  StM (Eff e) a
    liftBaseWith f = do
      lf <- askLogPredicate
      raise (liftBaseWith (\runInBase -> f (runInBase . runLogs lf)))
    restoreM = raise . restoreM

instance (LiftedBase m e, Catch.MonadThrow (Eff e))
  => Catch.MonadThrow (Eff (Logs ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance (Applicative m, MonadBaseControl IO (Eff e), LiftedBase m e, Catch.MonadCatch (Eff e), IoLogging (Logs ': e), Lifted IO e)
  => Catch.MonadCatch (Eff (Logs ': e)) where
  catch effect handler = do
    lf <- askLogPredicate
    let lower                   = runLogs lf
        nestedEffects           = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (Applicative m, MonadBaseControl IO (Eff e), LiftedBase m e, Catch.MonadMask (Eff e), IoLogging (Logs ': e), Lifted IO e)
  => Catch.MonadMask (Eff (Logs ': e)) where
  mask maskedEffect = do
    lf <- askLogPredicate
    let
      lower :: Eff (Logs ': e) a -> Eff e a
      lower = runLogs lf
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
      lower = runLogs lf
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
      lower = runLogs lf
    raise
        (Catch.generalBracket
          (lower acquire)
          (((.).(.)) lower release)
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
--
withLogging :: Lifted IO e => LogWriter -> Eff (Logs ': LogWriterReader ': e) a -> Eff e a
withLogging lw = runLogWriterReader lw . runLogs allLogMessages

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
--
withoutLogging :: Eff (Logs ': LogWriterReader ': e) a -> Eff e a
withoutLogging = runLogWriterReader mempty . runLogsWithoutLogging noLogMessages

-- | Raw handling of the 'Logs' effect.
-- Exposed for custom extensions, if in doubt use 'withLogging'.
runLogs
  :: forall e b .
     ( Member LogWriterReader (Logs ': e)
     , Lifted IO e
     )
  => LogPredicate
  -> Eff (Logs ': e) b
  -> Eff e b
runLogs p m =
  fix (handle_relay (\a _ -> return a)) (sendLogMessageToLogWriter  m) p

-- | Raw handling of the 'Logs' effect.
-- Exposed for custom extensions, if in doubt use 'withoutLogging'.
runLogsWithoutLogging
  :: forall e b .
     ( Member LogWriterReader (Logs ': e)
     )
  => LogPredicate
  -> Eff (Logs ': e) b
  -> Eff e b
runLogsWithoutLogging p m =
  fix (handle_relay (\a _ -> return a)) m p

-- | Log a message.
--
-- All logging goes through this function.
--
-- This function is the only place where the 'LogPredicate' is applied.
--
-- Also, 'LogMessage's are evaluated using 'deepseq', __after__ they pass the 'LogPredicate'.
logMsg :: forall e . (HasCallStack, Member Logs e) => LogMessage -> Eff e ()
logMsg = withFrozenCallStack $ \msgIn -> do
    lf <- askLogPredicate
    when (lf msgIn) $
      msgIn `deepseq` send @Logs (WriteLogMessage msgIn)

-- | Log a 'T.Text' as 'LogMessage' with a given 'Severity'.
logWithSeverity
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Severity
  -> Text
  -> Eff e ()
logWithSeverity = withFrozenCallStack $ \s ->
    logMsg
    . setCallStack callStack
    . set lmSeverity s
    . flip (set lmMessage) def

-- | Log a 'T.Text' as 'LogMessage' with a given 'Severity'.
logWithSeverity'
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Severity
  -> String
  -> Eff e ()
logWithSeverity' = withFrozenCallStack
  (\s m ->
       logMsg
     $ setCallStack callStack
     $ set lmSeverity s
     $ ( def & lmMessage .~ T.pack m))

-- | Log a 'String' as 'emergencySeverity'.
logEmergency
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Text
  -> Eff e ()
logEmergency = withFrozenCallStack (logWithSeverity emergencySeverity)

-- | Log a message with 'alertSeverity'.
logAlert
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Text
  -> Eff e ()
logAlert = withFrozenCallStack (logWithSeverity alertSeverity)

-- | Log a 'criticalSeverity' message.
logCritical
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Text
  -> Eff e ()
logCritical = withFrozenCallStack (logWithSeverity criticalSeverity)

-- | Log a 'errorSeverity' message.
logError
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Text
  -> Eff e ()
logError = withFrozenCallStack (logWithSeverity errorSeverity)

-- | Log a 'warningSeverity' message.
logWarning
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Text
  -> Eff e ()
logWarning = withFrozenCallStack (logWithSeverity warningSeverity)

-- | Log a 'noticeSeverity' message.
logNotice
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Text
  -> Eff e ()
logNotice = withFrozenCallStack (logWithSeverity noticeSeverity)

-- | Log a 'informationalSeverity' message.
logInfo
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Text
  -> Eff e ()
logInfo = withFrozenCallStack (logWithSeverity informationalSeverity)

-- | Log a 'debugSeverity' message.
logDebug
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => Text
  -> Eff e ()
logDebug = withFrozenCallStack (logWithSeverity debugSeverity)

-- | Log a 'String' as 'emergencySeverity'.
logEmergency'
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logEmergency' = withFrozenCallStack (logWithSeverity' emergencySeverity)

-- | Log a message with 'alertSeverity'.
logAlert'
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logAlert' = withFrozenCallStack (logWithSeverity' alertSeverity)

-- | Log a 'criticalSeverity' message.
logCritical'
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logCritical' = withFrozenCallStack (logWithSeverity' criticalSeverity)

-- | Log a 'errorSeverity' message.
logError'
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logError' = withFrozenCallStack (logWithSeverity' errorSeverity)

-- | Log a 'warningSeverity' message.
logWarning'
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logWarning' = withFrozenCallStack (logWithSeverity' warningSeverity)

-- | Log a 'noticeSeverity' message.
logNotice'
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logNotice' = withFrozenCallStack (logWithSeverity' noticeSeverity)

-- | Log a 'informationalSeverity' message.
logInfo'
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logInfo' = withFrozenCallStack (logWithSeverity' informationalSeverity)

-- | Log a 'debugSeverity' message.
logDebug'
  :: forall e .
     ( HasCallStack
     , Member Logs e
     )
  => String
  -> Eff e ()
logDebug' = withFrozenCallStack (logWithSeverity' debugSeverity)

-- | Log the current 'callStack' using the given 'Severity'.
--
-- @since 0.30.0
logCallStack :: forall e . (HasCallStack, Member Logs e) => Severity -> Eff e ()
logCallStack =
  withFrozenCallStack $ \s ->
    let stackTraceLines = T.lines (pack (prettyCallStack callStack))
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
logMultiLine
  :: forall e
    . ( HasCallStack
    , Member Logs e
    )
    => Severity
    -> [Text]
    -> Eff e ()
logMultiLine =
  withFrozenCallStack $ \s messageLines -> do
    let msgHash = T.pack $ printf "multi-line message %06X" $ hash messageLines `mod` 0x1000000
        messageLinesWithLineNum =
          let messageLineCount = Prelude.length messageLines
              messageLineCountString = T.pack (show messageLineCount)
              messageLineCountStringLen = T.length messageLineCountString
              printLineNum i =
                let i' = T.pack (show i)
                    padding = messageLineCountStringLen - T.length i'
                in msgHash <> " line " <> T.replicate padding " " <> i' <> " of " <> messageLineCountString <> ":    "
          in Prelude.zipWith (<>) (printLineNum <$> [1 :: Int ..])  messageLines
    traverse_ (logWithSeverity s) messageLinesWithLineNum


-- | Issue a log statement for each item in the list prefixed with a line number and a message hash.
--
-- When several concurrent processes issue log statements, multiline log statements are often
-- interleaved.
--
-- In order to make the logs easier to read, this function will count the items and calculate a unique
-- hash and prefix each message, so a user can grep to get all the lines of an interleaved,
-- multi-line log message.
--
-- This function takes a list of 'String's as opposed to 'logMultiLine'.
--
-- @since 0.30.0
logMultiLine'
  :: forall e
    . ( HasCallStack
    , Member Logs e
    )
    => Severity
    -> [String]
    -> Eff e ()
logMultiLine' s = logMultiLine s . fmap pack


-- | Get the current 'Logs' filter/transformer function.
--
-- See "Control.Eff.Log#LogPredicate"
askLogPredicate :: forall e . (Member Logs e) => Eff e LogPredicate
askLogPredicate = send @Logs AskLogFilter

-- | Keep only those messages, for which a predicate holds.
--
-- E.g. to keep only messages which begin with @"OMG"@:
--
-- > exampleSetLogWriter :: IO Int
-- > exampleSetLogWriter =
-- >     runLift
-- >   $ withLogging consoleLogWriter
-- >   $ do logMsg "test"
-- >        setLogPredicate (\ msg -> case view lmMessage msg of
-- >                                   'O':'M':'G':_ -> True
-- >                                   _             -> False)
-- >                          (do logMsg "this message will not be logged"
-- >                              logMsg "OMG logged"
-- >                              return 42)
--
-- In order to also delegate to the previous predicate, use 'modifyLogPredicate'
--
-- See "Control.Eff.Log#LogPredicate"
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
-- modifyLogPredicate (\previousPredicate msg -> previousPredicate msg && length (lmMessage msg) < 29 )
--                    (do logMsg "this message will not be logged"
--                        logMsg "this message might be logged")
-- @
--
-- See "Control.Eff.Log#LogPredicate"
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

-- | Include 'LogMessage's that match a 'LogPredicate'.
--
-- @includeLogMessages p@ allows log message to be logged if @p m@
--
-- Although it is enough if the previous predicate holds.
-- See 'excludeLogMessages' and 'modifyLogPredicate'.
--
-- See "Control.Eff.Log#LogPredicate"
includeLogMessages
  :: forall e a . (Member Logs e)
  => LogPredicate -> Eff e a -> Eff e a
includeLogMessages p = modifyLogPredicate (\p' m -> p' m || p m)

-- | Exclude 'LogMessage's that match a 'LogPredicate'.
--
-- @excludeLogMessages p@ discards logs if @p m@
--
-- Also the previous predicate must also hold for a
-- message to be logged.
-- See 'excludeLogMessages' and 'modifyLogPredicate'.
--
-- See "Control.Eff.Log#LogPredicate"
excludeLogMessages
  :: forall e a . (Member Logs e)
  => LogPredicate -> Eff e a -> Eff e a
excludeLogMessages p = modifyLogPredicate (\p' m -> not (p m) && p' m)

-- | Consume log messages.
--
-- Exposed for custom extensions, if in doubt use 'withLogging'.
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
-- 'MonadBaseControl', 'Catch.MonadMask', 'Catch.MonadCatch' and 'Catch.MonadThrow'.
--
-- In contrast the functions based on modifying the 'LogWriter',
-- such as 'addLogWriter' or 'censorLogs', are save to use in combination
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

-- | Change the 'LogMessage's using an effectful function.
--
-- Exposed for custom extensions, if in doubt use 'withLogging'.
--
-- This differs from 'respondToLogMessage' in that the intercepted messages will be
-- written either way, albeit in altered form.
--
-- NOTE: The effects of this function are __lost__ when using
-- 'MonadBaseControl', 'Catch.MonadMask', 'Catch.MonadCatch' and 'Catch.MonadThrow'.
--
-- In contrast the functions based on modifying the 'LogWriter',
-- such as 'addLogWriter' or 'censorLogs', are save to use in combination
-- with the aforementioned liftings.
interceptLogMessages
  :: forall r b
   . (Member Logs r)
  => (LogMessage -> Eff r LogMessage)
  -> Eff r b
  -> Eff r b
interceptLogMessages f = respondToLogMessage (f >=> logMsg)

-- | Internal function.
sendLogMessageToLogWriter :: IoLogging e => Eff e b -> Eff e b
sendLogMessageToLogWriter = respondToLogMessage liftWriteLogMessage

-- | Change the current 'LogWriter'.
modifyLogWriter :: IoLogging e => (LogWriter -> LogWriter) -> Eff e a -> Eff e a
modifyLogWriter f = localLogWriterReader f . sendLogMessageToLogWriter

-- | Replace the current 'LogWriter'.
-- To add an additional log message consumer use 'addLogWriter'
setLogWriter :: IoLogging e => LogWriter -> Eff e a -> Eff e a
setLogWriter = modifyLogWriter  . const

-- | Modify the the 'LogMessage's written in the given sub-expression.
--
-- Note: This is equivalent to @'modifyLogWriter' . 'mappingLogWriter'@
censorLogs :: IoLogging e => (LogMessage -> LogMessage) -> Eff e a -> Eff e a
censorLogs = modifyLogWriter . mappingLogWriter

-- | Modify the the 'LogMessage's written in the given sub-expression, as in 'censorLogs'
-- but with a effectful function.
--
-- Note: This is equivalent to @'modifyLogWriter' . 'mappingLogWriterIO'@
censorLogsIo :: IoLogging e => (LogMessage -> IO LogMessage) -> Eff e a -> Eff e a
censorLogsIo = modifyLogWriter . mappingLogWriterIO

-- | Combine the effects of a given 'LogWriter' and the existing one.
--
-- > import Data.Text    as T
-- > import Data.Text.IO as T
-- >
-- > exampleAddLogWriter :: IO ()
-- > exampleAddLogWriter = go >>= T.putStrLn
-- >  where go = fmap (unlines . map renderLogMessageConsoleLog . snd)
-- >               $  runLift
-- >               $  runCaptureLogWriter
-- >               $  withLogging captureLogWriter
-- >               $  addLogWriter (mappingLogWriter (lmMessage %~ ("CAPTURED "++)) captureLogWriter)
-- >               $  addLogWriter (filteringLogWriter severeMessages (mappingLogWriter (lmMessage %~ ("TRACED "++)) debugTraceLogWriter))
-- >               $  do
-- >                     logEmergency "test emergencySeverity 1"
-- >                     logCritical "test criticalSeverity 2"
-- >                     logAlert "test alertSeverity 3"
-- >                     logError "test errorSeverity 4"
-- >                     logWarning "test warningSeverity 5"
-- >                     logInfo "test informationalSeverity 6"
-- >                     logDebug "test debugSeverity 7"
-- >        severeMessages = view (lmSeverity . to (<= errorSeverity))
-- >
--
addLogWriter :: IoLogging e => LogWriter -> Eff e a -> Eff e a
addLogWriter lw2 = modifyLogWriter (\lw1 -> MkLogWriter (\m -> runLogWriter lw1 m >> runLogWriter lw2 m))
