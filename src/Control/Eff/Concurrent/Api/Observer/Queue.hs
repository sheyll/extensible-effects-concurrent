-- | A small process to capture and _share_ observation's by enqueueing them into an STM 'TBQeueu'.
module Control.Eff.Concurrent.Api.Observer.Queue
  ( ObservationQueue()
  , ObservationQueueReader
  , readObservationQueue
  , tryReadObservationQueue
  , flushObservationQueue
  , spawnLinkObserverationQueue
  )
where

import           Control.Concurrent.STM
import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.ExceptionExtra     ( )
import           Control.Eff.Lift
import           Control.Eff.Concurrent.Process
import           Control.Eff.Log
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Client
import           Control.Eff.Concurrent.Api.Observer
import           Control.Eff.Concurrent.Api.Server2
import           Control.Eff.Reader.Strict
import           Control.Exception.Safe        as Safe
import           Control.Monad.IO.Class
import           Control.Monad                  ( unless )
import           Data.Typeable
import           Text.Printf
import           GHC.Stack

-- | Contains a 'TBQueue' capturing observations received by 'enqueueObservationsRegistered'
-- or 'spawnLinkObserverationQueue'.
newtype ObservationQueue a = ObservationQueue (TBQueue a)

-- | A 'Reader' for an 'ObservationQueue'.
type ObservationQueueReader a = Reader (ObservationQueue a)

logPrefix :: forall o proxy . (HasCallStack, Typeable o) => proxy o -> String
logPrefix px = "observation queue: " ++ show (typeRep px)

-- | Read queued observations captured and enqueued in the shared 'TBQueue' by 'spawnLinkObserverationQueue'.
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

-- | Read queued observations captured and enqueued in the shared 'TBQueue' by 'spawnLinkObserverationQueue'.
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

-- | Read at once all currently queued observations captured and enqueued in the shared 'TBQueue' by 'spawnLinkObserverationQueue'.
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


-- | Capture an observation.
data instance Api (ObservationQueue a) r where
  EnqueueObservation :: a -> Api (ObservationQueue a) 'Asynchronous
  StopObservationQueue :: Api (ObservationQueue a) ('Synchronous ())

-- | Observe a 'Server' that implements an 'Observable' 'Api'. The observations
-- can be obtained by 'readObservationQueue'. All observations are captured up to
-- the queue size limit, such that the first message received will be first message
-- returned by 'readObservationQueue'.
spawnLinkObserverationQueue
  :: forall o q a
   . (Typeable o, Show o, HasLogging IO q, Lifted IO q, HasCallStack)
  => Server (ObserverRegistry o)
  -> Int
  -> Eff (ObservationQueueReader o ': InterruptableProcess q) a
  -> Eff (InterruptableProcess q) a
spawnLinkObserverationQueue oSvr queueLimit k = withQueue
  queueLimit
  (do
    ObservationQueue q <- ask @(ObservationQueue o)
    do
      logDebug
        (printf "%s starting with queue limit: %d"
                (logPrefix (Proxy @o))
                queueLimit
        )
      cbo <- raise
        (spawnLinkApiServer
          (handleCasts
            (\case
              EnqueueObservation o -> do
                lift (atomically (writeTBQueue q o))
                pure AwaitNext
            )
          )
          stopServerOnInterrupt
        )
      let thisObserver = toObserverFor EnqueueObservation cbo
      registerObserver thisObserver oSvr
      res <- k
      forgetObserver thisObserver oSvr
      call SP cbo StopObservationQueue
      logDebug (printf "%s stopped observer process" (logPrefix (Proxy @o)))
      return res
  )

withQueue
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
withQueue queueLimit e = do
  q   <- liftIO (newTBQueueIO (fromIntegral queueLimit))
  res <- handleInterrupts (return . Left)
                          (Right <$> runReader (ObservationQueue q) e)
  rest <- liftIO (atomically (flushTBQueue q))
  unless
    (null rest)
    (logNotice (logPrefix (Proxy @o) ++ " unread observations: " ++ show rest))
  either (\em -> logError (show em) >> liftIO (throwIO em)) return res
