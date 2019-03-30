{-# LANGUAGE UndecidableInstances #-}
-- | A logging effect based on 'Control.Monad.Log.MonadLog'.
module Control.Eff.Log.Handler
  ( logMsg
  , logMsgs
  , HasLogWriter
  , mapLogMessages
  , filterLogMessages
  , traverseLogMessages
  , changeLogWriter
  , ignoreLogs
  , traceLogs
  , LogWriter()
  , LogWriterReader
  , foldingLogWriter
  , writeAllLogMessages
  , singleMessageLogWriter
  , multiMessageLogWriter
  , askLogWriter
  , Logs()
  , writeLogs
  , withLogMessageHandler
  )
where

import           Control.DeepSeq
import           Control.Eff                   as Eff
import           Control.Eff.Extend
import           Control.Eff.Reader.Strict
import qualified Control.Exception.Safe        as Safe
import           Data.Default
import           Data.Foldable                  ( traverse_ )
import           Data.Function                  ( fix )
import           Data.Functor.Compose           ( Compose( Compose ) )
import           Control.Monad
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
logMsg (force -> msg) = logMsgs [msg]

-- | Log a bunch of messages. This might be more efficient than calling 'logMsg'
-- multiple times.
-- The messages are reduced to normal form (strict).
logMsgs
  :: ( Traversable f
     , NFData1 f
     , NFData (f m)
     , NFData m
     , Member (Logs m) e
     )
  => f m
  -> Eff e ()
logMsgs !msgs = rnf1 msgs `seq` do
  lf <- send AskLogFilter
  send (LogMsgs (Compose (traverse (fmap force . lf) msgs) ))

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

-- ** Filter and Transform Log Messages effectfully

-- | Map an 'Eff'ectful function over every bunch of log messages.
--
-- For example, to attach the current time to each log message:
--
-- > appendTimestamp
-- >  :: ( Member (Logs String) e
-- >     , Lifted IO e)
-- >  => Eff e a
-- >  -> Eff e a
-- > appendTimestamp = traverseLogMessages $ \ms -> do
-- >   now <- getCurrentTime
-- >   return (fmap (show now ++) ms)
traverseLogMessages
  :: forall m r h b
   . ( Member (Logs m) r
     , Member (Reader (LogWriter m h)) r
     , Monad h
     , Lifted h r
     )
  => (forall f . (Traversable f, NFData1 f) => f m -> h (f m))
  -> Eff r b
  -> Eff r b
traverseLogMessages f = changeLogWriter
  (\msgs -> do
    lw    <- ask
    msgs' <- lift (f msgs)
    lift (runLogWriter lw msgs')
  )

-- ** Change the Log Writer

-- | Change the way log messages are *written*.
-- Replaces the existing 'LogWriter' by a new one. The new 'LogWriter'
-- is constructed from a function that gets a /bunch/ of messages and
-- returns an 'Eff'ect.
-- That effect has a 'Reader' for the previous 'LogWriter' and 'Lift's
-- the log writer base monad.
changeLogWriter
  :: forall r m h a
   . (Monad h, Lifted h r, Member (Reader (LogWriter m h)) r)
  => (  forall f
      . (Traversable f, NFData1 f, MonadPlus f)
     => f m
     -> Eff '[Reader (LogWriter m h), Lift h] ()
     )
  -> Eff r a
  -> Eff r a
changeLogWriter interceptor =
  let replaceWriter old = LogWriter (runLift . runReader old . interceptor)
  in  local replaceWriter

-- * Handle Log Messages

-- | Throw away all log messages.
ignoreLogs :: forall m r a . Eff (Logs m ': r) a -> Eff r a
ignoreLogs = runLogsFiltered (const Nothing)

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
  go :: (Monad m, Foldable f) => f message -> m ()
  go = traverse_ (traceM . toString)


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
  LogMsgs
    :: (Traversable f, NFData1 f, NFData m, NFData (f m))
    => f m -> Logs m ()

-- | Drops all messages
instance Handle (Logs m) e a ((m -> Maybe m) -> k) where
  handle handleNext handleResponseAndContinue request logMessageFilter =
    case request of
      AskLogFilter ->
        handleNext (handleResponseAndContinue ^$ logMessageFilter) logMessageFilter
      LogMsgs _ ->
        handleNext
          (handleResponseAndContinue ^$ ())
          logMessageFilter


-- | Replace the current log filter. --TODO write localLogFilter test
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
              LogMsgs ms   -> send (LogMsgs ms) >> step (co ^$ ()) lf2
        )
        (\a _lf -> return a)
     )
     actionThatLogs
     lf

-- | Install 'Logs' handler that 'ask's for a 'LogWriter' for the
-- message type and applies the log writer to the messages.
--
-- This functions will provide a filter/transformation function for log messages.
--
-- All log messages received by this function are silently dropped.
--
-- Log message writing is a task delegated to e.g. 'withLogMessageHandler'
-- or to 'withLogWriter'.
runLogsFiltered :: (m -> Maybe m) -> Eff (Logs m ': e) b -> Eff e b
runLogsFiltered logMessageFilter actionThatLogs =
  fix (handle_relay (\a _f -> return a)) actionThatLogs logMessageFilter

-- | Apply a callback to log messages, that pass the filter.
withLogMessageHandler
  :: forall m e b
   . (NFData m, SetMember Logs (Logs m) e)
  => (forall f . (Traversable f, NFData1 f, NFData m, NFData (f m)) => f m -> Eff e ())
  -> Eff e b
  -> Eff e b
withLogMessageHandler lw actionThatLogs =
  send (AskLogFilter :: Logs m (m -> Maybe m))
  >>=
  fix
     (respond_relay'
        (\step co (req :: Logs m v) lf2 ->
            case req of
              AskLogFilter -> step (co ^$ lf2) lf2
              LogMsgs ms ->
                step (do
                         lw (force (Compose (lf2 <$> ms)))
                         co ^$ ()
                     )
                     lf2
        )
        (\a _lf -> return a)
     )
     actionThatLogs

-- | Apply a 'LogWriter' to those log messages, that pass the filter.
withLogWriter
  :: forall m h e b
   . (NFData m, Applicative h, Lifted h e, SetMember Logs (Logs m) e)
  => LogWriter m h
  -> Eff e b
  -> Eff e b
withLogWriter lw actionThatLogs =
  send (AskLogFilter :: Logs m (m -> Maybe m))
  >>=
  fix
     (respond_relay'
        (\step co (req :: Logs m v) lf2 ->
            case req of
              AskLogFilter -> step (co ^$ lf2) lf2
              LogMsgs ms ->
                step (do
                         lift (traverse_ (runLogWriter lw . lf2) ms)
                         co ^$ ()
                     )
                     lf2
        )
        (\a _lf -> return a)
     )
     actionThatLogs

-- | Handle log message effects by a monadic action, e.g. an IO action
-- to send logs to the console output or a log-server.
-- The monadic log writer action is wrapped in a newtype called
-- 'LogWriter'.
--
-- Use the smart constructors below to create them, e.g.
-- 'foldingLogWriter', 'singleMessageLogWriter' or
-- 'mulitMessageLogWriter'.
writeLogs
  :: forall message writerM r a
   . (Applicative writerM, Lifted writerM r, NFData message)
  => LogWriter message writerM
  -> Eff (Logs message ': r) a
  -> Eff r a
writeLogs  = writeLogsFiltered Just

-- | Handle log message effects by a monadic action, e.g. an IO action
-- to send logs to the console output or a log-server.
-- The monadic log writer action is wrapped in a newtype called
-- 'LogWriter'.
--
-- Use the smart constructors below to create them, e.g.
-- 'foldingLogWriter', 'singleMessageLogWriter' or
-- 'mulitMessageLogWriter'.
writeLogsFiltered
  :: forall message writerM r a
   . (Applicative writerM, Lifted writerM r, NFData message)
  => (message -> Maybe message)
  -> LogWriter message writerM
  -> Eff (Logs message ': r) a
  -> Eff r a
writeLogsFiltered f w = runLogsFiltered f . withLogWriter w

-- | This instance allows liftings of the 'Logs' effect.
instance (MonadBase m m, LiftedBase m r, NFData l) => MonadBaseControl m (Eff (Logs l ': r)) where

    type StM (Eff (Logs l ': r)) a =  StM (Eff r) a

    liftBaseWith f = do
      lf <- send AskLogFilter
      raise (liftBaseWith (\runInBase -> f (runInBase . runLogsFiltered lf)))

    restoreM = raise . restoreM

instance (NFData l, LiftedBase m e, Catch.MonadThrow (Eff e))
  => Catch.MonadThrow (Eff (Logs l ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance (NFData l, Applicative m, LiftedBase m e, Catch.MonadCatch (Eff e))
  => Catch.MonadCatch (Eff (Logs l ': e)) where
  catch effect handler = do
    logFilter <- send AskLogFilter
    let lower                   = runLogsFiltered logFilter
        nestedEffects           = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (NFData l, Applicative m, LiftedBase m e, Catch.MonadMask (Eff e))
  => Catch.MonadMask (Eff (Logs l ': e)) where
  mask maskedEffect = do
    logFilter <- send AskLogFilter
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
    logFilter <- send AskLogFilter
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
    logFilter <- send AskLogFilter
    let
      lower :: Eff (Logs l ': e) a -> Eff e a
      lower = runLogsFiltered logFilter
    raise
        (Catch.generalBracket
          (lower acquire)
          (((.).(.)) lower release)
          (lower . use)
      )
