{-# LANGUAGE UndecidableInstances #-}
-- | A memory efficient, streaming, logging effect with support for
-- efficiently not logging when no logs are required.
--
-- Good support for logging to a file or to the network,
-- as well as asynchronous logging in another thread.
module Control.Eff.Log.Handler
  ( -- * Logging API
    -- ** Sending Log Messages
    logMsg
  , logWithSeverity
  , logEmergency
  , logAlert
  , logCritical
  , logError
  , logWarning
  , logNotice
  , logInfo
  , logDebug

  -- ** Log Message Pre-Filtering #LogPredicate#
  -- $LogPredicate
  , includeLogMessages
  , excludeLogMessages
  , setLogPredicate
  , modifyLogPredicate
  , askLogPredicate

  -- * Log Handling API

  -- ** Writing Logs
  , LogWriterReader
  , setLogWriter
  , addLogWriter
  -- *** Log Message Modification
  , withLogFileAppender
  , censorLogs
  , censorLogsM
  , askLogWriter
  , modifyLogWriter

  -- ** 'Logs' Effect Handling
  , Logs()
  , LogsTo
  , withConsoleLogging
  , withIoLogging
  , withLogging
  , withSomeLogging
  , LoggingAndIo
  -- ** Low-Level API for Custom Extensions
  -- *** Log Message Interception
  , runLogs
  , respondToLogMessage
  , interceptLogMessages
  -- *** LogWriter Handling
  , runLogWriterReader

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
import           System.Directory               ( canonicalizePath
                                                , createDirectoryIfMissing
                                                )
import           System.FilePath                ( takeDirectory )


-- | This effect sends 'LogMessage's and is a reader for a 'LogPredicate'.
--
-- Logs are sent via 'logMsg';
-- for more information about log predicates, see "Control.Eff.Log.Handler#LogPredicate"
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

-- | This instance allows lifting to the 'Logs' effect, they do, however, need a
-- 'LogWriter' in the base monad, in order to be able to handle 'logMsg' invocations.
--
-- The 'LogWriterReader' effect is must be available to get to the 'LogWriter'.
--
-- Otherwise there is no way to preserve to log messages.
instance forall m e. (MonadBase m m, LiftedBase m e, SupportsLogger m (Logs ': e), SetMember LogWriterReader (LogWriterReader m) (Logs ': e))
  => MonadBaseControl m (Eff (Logs ': e)) where
    type StM (Eff (Logs ': e)) a =  StM (Eff e) a
    liftBaseWith f = do
      lf <- askLogPredicate
      raise (liftBaseWith (\runInBase -> f (runInBase . runLogs @m lf)))
    restoreM = raise . restoreM

instance (LiftedBase m e, Catch.MonadThrow (Eff e))
  => Catch.MonadThrow (Eff (Logs ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance (Applicative m, LiftedBase m e, Catch.MonadCatch (Eff e), SupportsLogger m (Logs ': e), SetMember LogWriterReader (LogWriterReader m) (Logs ': e))
  => Catch.MonadCatch (Eff (Logs ': e)) where
  catch effect handler = do
    lf <- askLogPredicate
    let lower                   = runLogs @m lf
        nestedEffects           = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (Applicative m, LiftedBase m e, Catch.MonadMask (Eff e), SupportsLogger m (Logs ': e), SetMember LogWriterReader (LogWriterReader m) (Logs ': e))
  => Catch.MonadMask (Eff (Logs ': e)) where
  mask maskedEffect = do
    lf <- askLogPredicate
    let
      lower :: Eff (Logs ': e) a -> Eff e a
      lower = runLogs @m lf
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
      lower = runLogs @m lf
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
      lower = runLogs @m lf
    raise
        (Catch.generalBracket
          (lower acquire)
          (((.).(.)) lower release)
          (lower . useIt)
      )


-- | A constraint alias for effects that requires a 'LogWriterReader', as well as that the
-- contained 'LogWriterReader' has a 'SupportsLogger' instance.
--
-- The requirements of this constraint are provided by:
-- * 'withStdOutLogging'
-- * 'withIoLogging'
-- * 'withLogging'
-- * 'withSomeLogging'
--
type LogsTo h e = (Member Logs e, SupportsLogger h e, SetMember LogWriterReader (LogWriterReader h) e)

-- | Enable logging to @stdout@ using the 'defaultIoLogWriter' in combination with
-- the 'consoleLogWriter'.
--
-- Example:
--
-- > exampleWithConsoleLogging :: IO ()
-- > exampleWithConsoleLogging =
-- >     runLift
-- >   $ withConsoleLogging "my-app" local7 allLogMessages
-- >   $ logInfo "Oh, hi there"
--
-- To vary the 'LogWriter' use 'withIoLogging'.
withConsoleLogging
  :: SetMember Lift (Lift IO) e
  => String
  -> Facility
  -> LogPredicate
  -> Eff (Logs : LogWriterReader IO : e) a
  -> Eff e a
withConsoleLogging = withIoLogging consoleLogWriter

-- | Enable logging to IO using the 'defaultIoLogWriter'.
--
-- To log to the console (standard output), one can use 'withConsoleLogging'.
--
-- Example:
--
-- > exampleWithIoLogging :: IO ()
-- > exampleWithIoLogging =
-- >     runLift
-- >   $ withIoLogging consoleLogWriter
--                     "my-app"
--                     local7
--                     (lmSeverityIsAtLeast informationalSeverity)
-- >   $ logInfo "Oh, hi there"
--
withIoLogging
  :: SetMember Lift (Lift IO) e
  => LogWriter IO
  -> String
  -> Facility
  -> LogPredicate
  -> Eff (Logs : LogWriterReader IO : e) a
  -> Eff e a
withIoLogging lw appName facility defaultPredicate =
    withLogging (defaultIoLogWriter appName facility lw)
  . setLogPredicate defaultPredicate

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
withLogging ::
     forall h e a. (Applicative h, LogsTo h (Logs ': LogWriterReader h ': e))
  => LogWriter h -> Eff (Logs ': LogWriterReader h ': e) a -> Eff e a
withLogging lw = runLogWriterReader lw . runLogs allLogMessages

-- | Handles the 'Logs' and 'LogWriterReader' effects.
--
-- By default it uses the 'noOpLogWriter', but using 'setLogWriter' the
-- 'LogWriter' can be replaced.
--
-- This is like 'withLogging' applied to 'noOpLogWriter'
--
-- Example:
--
-- > exampleWithSomeLogging :: ()
-- > exampleWithSomeLogging =
-- >     run
-- >   $ withSomeLogging @PureLogWriter
-- >   $ logDebug "Oh, hi there"
--
withSomeLogging ::
     forall h e a.
     (Applicative h, LogsTo h (Logs ': LogWriterReader h ': e))
  => Eff (Logs ': LogWriterReader h ': e) a
  -> Eff e a
withSomeLogging = withLogging (noOpLogWriter @h)

-- | The concrete list of 'Eff'ects for logging with an IO based 'LogWriter', and a 'LogWriterReader'.
type LoggingAndIo = '[Logs, LogWriterReader IO, Lift IO]

-- | Raw handling of the 'Logs' effect.
-- Exposed for custom extensions, if in doubt use 'withLogging'.
runLogs
  :: forall h e b .
     (LogsTo h (Logs ': e), SupportsLogger h (Logs ': e))
  => LogPredicate
  -> Eff (Logs ': e) b
  -> Eff e b
runLogs p m =
  fix (handle_relay (\a _ -> return a)) (sendLogMessageToLogWriter  m) p

-- | Log a message.
--
-- All logging goes through this function.
--
-- This function is the only place where the 'LogPredicate' is applied.
--
-- Also, 'LogMessage's are evaluated using 'deepseq', __after__ they pass the 'LogPredicate'.
logMsg :: forall e m . (HasCallStack, Member Logs e, ToLogMessage m) => m -> Eff e ()
logMsg (toLogMessage -> msgIn) =
  withFrozenCallStack $ do
    lf <- askLogPredicate
    when (lf msgIn) $
      msgIn `deepseq` send @Logs (WriteLogMessage msgIn)

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

-- $LogPredicate
--
-- Ways to change the 'LogPredicate' are:
--
--  * 'setLogPredicate'.
--  * 'modifyLogPredicate'.
--  * 'includeLogMessages'
--  * 'excludeLogMessages'
--
-- The current predicate is retrieved via 'askLogPredicate'.
--
-- Some pre-defined 'LogPredicate's can be found here: "Control.Eff.Log.Message#PredefinedPredicates"

-- | Get the current 'Logs' filter/transformer function.
--
-- See "Control.Eff.Log.Handler#LogPredicate"
askLogPredicate :: forall e . (Member Logs e) => Eff e LogPredicate
askLogPredicate = send @Logs AskLogFilter

-- | Keep only those messages, for which a predicate holds.
--
-- E.g. to keep only messages which begin with @"OMG"@:
--
-- > exampleLogPredicate :: IO Int
-- > exampleLogPredicate =
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
-- See "Control.Eff.Log.Handler#LogPredicate"
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
-- See "Control.Eff.Log.Handler#LogPredicate"
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
-- @excludeLogMessages p@ allows log message to be logged if @p m@
--
-- Although it is enough if the previous predicate holds.
-- See 'excludeLogMessages' and 'modifyLogPredicate'.
--
-- See "Control.Eff.Log.Handler#LogPredicate"
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
-- See "Control.Eff.Log.Handler#LogPredicate"
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
sendLogMessageToLogWriter
  :: forall h e b .
     (LogsTo h e, SupportsLogger h e, Member Logs e)
  => Eff e b -> Eff e b
sendLogMessageToLogWriter = respondToLogMessage messageCallback
  where
    messageCallback msg = do
      lw <- askLogWriter
      liftLogWriter lw msg

-- | A Reader specialized for 'LogWriter's
--
-- The existing @Reader@ couldn't be used together with 'SetMember', so this
-- lazy reader was written, specialized to reading 'LogWriter'.
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

-- | Provide the 'LogWriter'
--
-- Exposed for custom extensions, if in doubt use 'withLogging'.
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
-- To add an additional log message consumer use 'addLogWriter'
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
-- > 
-- > exampleAddLogWriter :: IO ()
-- > exampleAddLogWriter = go >>= putStrLn
-- >  where go = fmap (unlines . map renderLogMessage . snd)
-- >               $  runLift
-- >               $  runCapturedLogsWriter
-- >               $  withLogging listLogWriter
-- >               $  addLogWriter (mappingLogWriter (lmMessage %~ ("CAPTURED "++)) listLogWriter)
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
addLogWriter :: forall h e a .
     (HasCallStack, LogsTo h e, Monad h)
  => LogWriter h -> Eff e a -> Eff e a
addLogWriter lw2 = modifyLogWriter (\lw1 -> MkLogWriter (\m -> runLogWriter lw1 m >> runLogWriter lw2 m))

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
