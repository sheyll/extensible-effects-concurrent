{-# LANGUAGE UndecidableInstances #-}

-- | A small process to capture and _share_ observation's by enqueueing them into an STM 'TBQueue'.
module Control.Eff.Concurrent.Protocol.Observer.Queue
  ( ObservationQueue (..),
    Reader,
    observe,
    await,
    tryRead,
    flush,
  )
where

import Control.Concurrent.STM
import Control.Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Observer
import Control.Eff.Concurrent.Protocol.StatefulServer
import Control.Eff.ExceptionExtra ()
import Control.Eff.Log
import qualified Control.Eff.Reader.Strict as Eff
import Control.Exception.Safe as Safe
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Data.Default (Default)
import Data.Proxy
import GHC.Stack

-- | Contains a 'TBQueue' capturing observations.
-- See 'observe'.
newtype ObservationQueue a = ObservationQueue (TBQueue a)

-- | A 'Reader' for an 'ObservationQueue'.
type Reader a = Eff.Reader (ObservationQueue a)

instance ToTypeLogMsg event => ToTypeLogMsg (ObservationQueue event) where
  toTypeLogMsg _ = toTypeLogMsg (Proxy @event) <> packLogMsg "_observations"

instance ToTypeLogMsg event => ToLogMsg (ObservationQueue event) where
  toLogMsg _ = toTypeLogMsg (Proxy @(ObservationQueue event))

-- | Read queued observations captured and enqueued in the shared 'ObservationQueue' by 'observe'.
--
-- This blocks until something was captured or an interrupt or exceptions was thrown. For a non-blocking
-- variant use 'tryRead' or 'flush'.
--
-- @since 0.28.0
await ::
  forall event r.
  ( Member (Reader event) r,
    HasCallStack,
    MonadIO (Eff r)
  ) =>
  Eff r event
await = do
  ObservationQueue q <- Eff.ask @(ObservationQueue event)
  liftIO (atomically (readTBQueue q))

-- | Read queued observations captured and enqueued in the shared 'ObservationQueue' by 'observe'.
--
-- Return the oldest enqueued observation immediately or 'Nothing' if the queue is empty.
-- Use 'await' to block until an observation is observerRegistryNotify.
--
-- @since 0.28.0
tryRead ::
  forall event r.
  ( Member (Reader event) r,
    HasCallStack,
    MonadIO (Eff r)
  ) =>
  Eff r (Maybe event)
tryRead = do
  ObservationQueue q <- Eff.ask @(ObservationQueue event)
  liftIO (atomically (tryReadTBQueue q))

-- | Read at once all currently queued observations captured and enqueued
-- in the shared 'ObservationQueue' by 'observe'.
--
-- This returns immediately all currently enqueued observations.
-- For a blocking variant use 'await'.
--
-- @since 0.28.0
flush ::
  forall event r.
  ( Member (Reader event) r,
    HasCallStack,
    MonadIO (Eff r)
  ) =>
  Eff r [event]
flush = do
  ObservationQueue q <- Eff.ask @(ObservationQueue event)
  liftIO (atomically (flushTBQueue q))

-- | Listen to, and capture observations in an 'ObservationQueue'.
--
-- Fork an 'Observer' process that runs only while the body expression is executed.
-- Register the observer to the observable process passed to this function.
--
-- The captured observations can be obtained by 'await',
-- 'tryRead' and 'flush'.
--
-- The queue size is limited to the given number.
--
-- ==== __Example__
--
-- @
--
-- import qualified Control.Eff.Concurrent.Observer.Queue as OQ
--
-- foo =
--   do
--     observed <- startLink SomeObservable
--     OQ.observe 100 observed $ do
--       ...
--       cast observed DoSomething
--       evt <- OQ.await \@TestEvent
--       ...
-- @
--
-- @since 0.28.0
observe ::
  forall event eventSource e q len b.
  ( HasCallStack,
    HasProcesses e q,
    FilteredLogging e,
    FilteredLogging q,
    FilteredLogging (Processes q),
    Lifted IO e,
    Lifted IO q,
    IsObservable eventSource event,
    Integral len,
    TangiblePdu eventSource 'Asynchronous
  ) =>
  len ->
  Endpoint eventSource ->
  Eff (Reader event ': e) b ->
  Eff e b
observe queueLimit eventSource e =
  withObservationQueue queueLimit (withWriter @event eventSource e)

withObservationQueue ::
  forall event b e len.
  ( HasCallStack,
    ToLogMsg event,
    ToTypeLogMsg event,
    Member Logs e,
    Lifted IO e,
    Integral len,
    Member Interrupts e
  ) =>
  len ->
  Eff (Reader event ': e) b ->
  Eff e b
withObservationQueue queueLimit e = do
  q <- lift (newTBQueueIO (fromIntegral queueLimit))
  let oq = ObservationQueue q
  res <-
    handleInterrupts
      (return . Left)
      (Right <$> Eff.runReader oq e)
  rest <- lift (atomically (flushTBQueue q))
  unless
    (null rest)
    (mapM_ (logDebug oq . LABEL "unread observation") rest)
  either
    ( \em ->
        do
          logError em
          lift (throwIO (MkUnhandledProcessInterrupt em))
    )
    return
    res

-- | Spawn a process that can be used as an 'Observer' that enqueues the observations into an
--   'ObservationQueue'. See 'withObservationQueue' for an example.
--
-- The observations can be obtained by 'await'. All observations are captured up to
-- the queue size limit, such that the first message received will be first message
-- returned by 'await'.
--
-- @since 0.28.0
spawnWriter ::
  forall event r q.
  ( FilteredLogging (Processes q),
    HasProcesses r q,
    Typeable event,
    HasCallStack,
    Server (ObservationQueue event) (Processes q),
    ToTypeLogMsg event
  ) =>
  ObservationQueue event ->
  Eff r (Endpoint (Observer event))
spawnWriter q =
  startLink @_ @r @q (MkObservationQueue q)

-- | Spawn a process that can be used as an 'Observer' that enqueues the observations into an
--   'ObservationQueue'. See 'withObservationQueue'.
--
-- The observations can be obtained by 'await'. All observations are captured up to
-- the queue size limit, such that the first message received will be first message
-- returned by 'await'.
--
-- @since 0.28.0
withWriter ::
  forall event eventSource e q b.
  ( HasCallStack,
    HasProcesses e q,
    Lifted IO q,
    FilteredLogging (Processes q),
    Member Logs q,
    IsObservable eventSource event,
    Member (Reader event) e,
    TangiblePdu eventSource 'Asynchronous
  ) =>
  Endpoint eventSource ->
  Eff e b ->
  Eff e b
withWriter eventSource e = do
  q <- Eff.ask @(ObservationQueue event)
  w <- spawnWriter @event q
  registerObserver @event eventSource w
  res <- e
  forgetObserver @event eventSource w
  sendShutdown (w ^. fromEndpoint) ExitNormally
  pure res

instance (Typeable event, Lifted IO q, Member Logs q, ToTypeLogMsg event) => Server (ObservationQueue event) (Processes q) where
  type Protocol (ObservationQueue event) = Observer event

  data StartArgument (ObservationQueue event)
    = MkObservationQueue (ObservationQueue event)

  newtype Model (ObservationQueue event) = MkObservationQueueModel () deriving (Default)

  update _ (MkObservationQueue (ObservationQueue q)) =
    \case
      OnCast (Observed !event) -> do
        isFull <- lift . atomically $ do
          isFull <- isFullTBQueue q
          unless isFull (writeTBQueue q event)
          pure isFull
        when isFull $
          logWarning (MSG "queue full")
      otherMsg ->
        logError (LABEL "unexpected" otherMsg)

instance ToTypeLogMsg event => ToLogMsg (StartArgument (ObservationQueue event)) where
  toLogMsg (MkObservationQueue q) = toLogMsg q
