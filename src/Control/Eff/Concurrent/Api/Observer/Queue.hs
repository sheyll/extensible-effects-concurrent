-- | A small process to capture and _share_ observation's by enqueueing them into an STM 'TBQueue'.
module Control.Eff.Concurrent.Api.Observer.Queue
  ( ObservationQueue()
  , ObservationQueueReader
  , readObservationQueue
  , tryReadObservationQueue
  , flushObservationQueue
  , withObservationQueue
  , spawnLinkObservationQueueWriter
  )
where

import           Control.Concurrent.STM
import           Control.Eff
import           Control.Eff.ExceptionExtra     ( )
import           Control.Eff.Lift
import           Control.Eff.Concurrent.Process
import           Control.Eff.Log
import           Control.Eff.Concurrent.Api.Observer
import           Control.Eff.Concurrent.Api.Server
import           Control.Eff.Reader.Strict
import           Control.Exception.Safe        as Safe
import           Control.Monad.IO.Class
import           Control.Monad                  ( unless )
import           Data.Typeable
import           GHC.Stack

-- | Contains a 'TBQueue' capturing observations received by 'enqueueObservationsRegistered'
-- or 'spawnLinkObservationQueueWriter'.
newtype ObservationQueue a = ObservationQueue (TBQueue a)

-- | A 'Reader' for an 'ObservationQueue'.
type ObservationQueueReader a = Reader (ObservationQueue a)

logPrefix :: forall o proxy . (HasCallStack, Typeable o) => proxy o -> String
logPrefix px = "observation queue: " ++ show (typeRep px)

-- | Read queued observations captured and enqueued in the shared 'TBQueue' by 'spawnLinkObservationQueueWriter'.
-- This blocks until something was captured or an interrupt or exceptions was thrown. For a non-blocking
-- variant use 'tryReadObservationQueue' or 'flushObservationQueue'.
readObservationQueue
  :: forall o r
   . ( Member (ObservationQueueReader o) r
     , HasCallStack
     , MonadIO (Eff r)
     , Typeable o
     , HasLogging IO r
     )
  => Eff r o
readObservationQueue = do
  ObservationQueue q <- ask @(ObservationQueue o)
  liftIO (atomically (readTBQueue q))

-- | Read queued observations captured and enqueued in the shared 'TBQueue' by 'spawnLinkObservationQueueWriter'.
-- Return the oldest enqueued observation immediately or 'Nothing' if the queue is empty.
-- Use 'readObservationQueue' to block until an observation is observed.
tryReadObservationQueue
  :: forall o r
   . ( Member (ObservationQueueReader o) r
     , HasCallStack
     , MonadIO (Eff r)
     , Typeable o
     , HasLogging IO r
     )
  => Eff r (Maybe o)
tryReadObservationQueue = do
  ObservationQueue q <- ask @(ObservationQueue o)
  liftIO (atomically (tryReadTBQueue q))

-- | Read at once all currently queued observations captured and enqueued in the shared 'TBQueue' by 'spawnLinkObservationQueueWriter'.
-- This returns immediately all currently enqueued 'Observation's. For a blocking
-- variant use 'readObservationQueue'.
flushObservationQueue
  :: forall o r
   . ( Member (ObservationQueueReader o) r
     , HasCallStack
     , MonadIO (Eff r)
     , Typeable o
     , HasLogging IO r
     )
  => Eff r [o]
flushObservationQueue = do
  ObservationQueue q <- ask @(ObservationQueue o)
  liftIO (atomically (flushTBQueue q))

-- | Create a mutable queue for observations. Use 'spawnLinkObservationQueueWriter' for a simple way to get
-- a process that enqueues all observations.
--
-- ==== __Example__ ====
--
-- @
-- withObservationQueue 100 $ do
--   q  <- ask @(ObservationQueueReader TestEvent)
--   wq <- spawnLinkObservationQueueWriter q
--   registerObserver wq testServer
--   ...
--   cast testServer DoSomething
--   evt <- readObservationQueue @TestEvent
--   ...
-- @
--
-- @since 0.18.0
withObservationQueue
  :: forall o b e len
   . ( HasCallStack
     , Typeable o
     , Show o
     , HasLogging IO e
     , Integral len
     , Member Interrupts e
     )
  => len
  -> Eff (ObservationQueueReader o ': e) b
  -> Eff e b
withObservationQueue queueLimit e = do
  q   <- liftIO (newTBQueueIO (fromIntegral queueLimit))
  res <- handleInterrupts (return . Left)
                          (Right <$> runReader (ObservationQueue q) e)
  rest <- liftIO (atomically (flushTBQueue q))
  unless
    (null rest)
    (logNotice (logPrefix (Proxy @o) ++ " unread observations: " ++ show rest))
  either (\em -> logError (show em) >> liftIO (throwIO em)) return res

-- | Spawn a process that can be used as an 'Observer' that enqueues the observations into an
--   'ObservationQueue'. See 'withObservationQueue' for an example.
--
-- The observations can be obtained by 'readObservationQueue'. All observations are captured up to
-- the queue size limit, such that the first message received will be first message
-- returned by 'readObservationQueue'.
--
-- @since 0.18.0
spawnLinkObservationQueueWriter
  :: forall o q
   . (Typeable o, Show o, HasLogging IO q, Lifted IO q, HasCallStack)
  => ObservationQueue o
  -> Eff (InterruptableProcess q) (Observer o)
spawnLinkObservationQueueWriter (ObservationQueue q) = do
  cbo <- spawnLinkApiServer
    (handleObservations
      (\case
        o -> do
          lift (atomically (writeTBQueue q o))
          pure AwaitNext
      )
    )
    stopServerOnInterrupt
  pure (toObserver cbo)
