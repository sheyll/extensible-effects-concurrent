-- | Capture 'Observation's and enqueue then into an STM 'TBQeueu'.
module Control.Eff.Concurrent.Api.Observer.Queue
  ( ObservationQueue()
  , ObservationQueueReader
  , readObservationQueue
  , tryReadObservationQueue
  , flushObservationQueue
  , enqueueObservationsRegistered
  , enqueueObservations
  )
where

import           Control.Concurrent.STM
import           Control.Eff
import           Control.Eff.ExceptionExtra
import           Control.Eff.Lift
import           Control.Eff.Concurrent.Process
import           Control.Eff.Log
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Client
import           Control.Eff.Concurrent.Api.Observer
import           Control.Eff.Reader.Strict
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Typeable
import           GHC.Stack

-- | Contains a 'TBQueue' capturing observations received by 'enqueueObservationsRegistered'
-- or 'enqueueObservations'.
newtype ObservationQueue a = ObservationQueue (TBQueue (Observation a))

-- | A 'Reader' for an 'ObservationQueue'.
type ObservationQueueReader a = Reader (ObservationQueue a)

-- | Read queued observations captured by observing a 'Server' that implements
-- an 'Observable' 'Api' using 'enqueueObservationsRegistered' or 'enqueueObservations'.
-- This blocks until the next 'Observation' received. For a non-blocking
-- variant use 'tryReadObservationQueue' or 'flushObservationQueue'.
readObservationQueue
  :: forall o r
   . (Member (ObservationQueueReader o) r, HasCallStack, MonadIO (Eff r))
  => Eff r (Observation o)
readObservationQueue = do
  ObservationQueue q <- ask @(ObservationQueue o)
  liftIO (atomically (readTBQueue q))

-- | Read queued observations captured by observing a 'Server' that implements
-- an 'Observable' 'Api' using 'enqueueObservationsRegistered' or 'enqueueObservations'.
-- Return the next 'Observation' immediately or 'Nothing' if the queue is empty.
-- Use 'readObservationQueue' to block until an observation is observed.
tryReadObservationQueue
  :: forall o r
   . (Member (ObservationQueueReader o) r, HasCallStack, MonadIO (Eff r))
  => Eff r (Maybe (Observation o))
tryReadObservationQueue = do
  ObservationQueue q <- ask @(ObservationQueue o)
  liftIO (atomically (tryReadTBQueue q))

-- | Read all currently queued 'Observation's captured by 'enqueueObservations'.
-- This returns immediately all currently enqueued 'Observation's. For a blocking
-- variant use 'readObservationQueue'.
flushObservationQueue
  :: forall o r
   . (Member (ObservationQueueReader o) r, HasCallStack, MonadIO (Eff r))
  => Eff r [Observation o]
flushObservationQueue = do
  ObservationQueue q <- ask @(ObservationQueue o)
  liftIO (atomically (flushTBQueue q))

-- | Observe a(the) registered 'Server' that implements an 'Observable' 'Api'.
-- Based on 'enqueueObservations'.
enqueueObservationsRegistered
  :: forall o r q a
   . ( ServesApi o r q
     , SetMember Process (Process q) r
     , Typeable o
     , Show (Observation o)
     , Observable o
     , Member (Logs LogMessage) q
     , MonadIO (Eff (Process q ': q))
     , MonadIO (Eff r)
     , SetMember Lift (Lift IO) r
     , Member (Logs LogMessage) r
     , HasCallStack
     )
  => SchedulerProxy q
  -> Int
  -> Eff (ObservationQueueReader o ': r) a
  -> Eff r a
enqueueObservationsRegistered px queueLimit k = do
  oSvr <- whereIsServer @o
  enqueueObservations px oSvr queueLimit k

-- | Observe a 'Server' that implements an 'Observable' 'Api', the 'Observation's
-- can be obtained by 'readObservationQueue'. All observations are captured up to
-- the queue size limit, such that the first message received will be first message
-- returned by 'readObservationQueue'.
--
-- This function captures runtime exceptions and cleans up accordingly.
enqueueObservations
  :: forall o r q a
   . ( SetMember Process (Process q) r
     , Typeable o
     , Show (Observation o)
     , Observable o
     , Member (Logs LogMessage) q
     , MonadIO (Eff (Process q ': q))
     , MonadIO (Eff r)
     , SetMember Lift (Lift IO) r
     , Member (Logs LogMessage) r
     , HasCallStack
     )
  => SchedulerProxy q
  -> Server o
  -> Int
  -> Eff (ObservationQueueReader o ': r) a
  -> Eff r a
enqueueObservations px oSvr queueLimit k = withQueue
  queueLimit
  (do
    ObservationQueue q <- ask @(ObservationQueue o)
    do
      cbo <- spawnCallbackObserver
        px
        (\_from observeration -> do
          liftIO (atomically (writeTBQueue q observeration))
          return True
        )
      registerObserver SchedulerProxy cbo oSvr
      res <- k
      forgetObserver SchedulerProxy cbo oSvr
      sendShutdown px (_fromServer cbo)
      return res
  )

withQueue
  :: ( HasCallStack
     , MonadIO (Eff e)
     , Member (Logs LogMessage) e
     , SetMember Lift (Lift IO) e
     )
  => Int
  -> Eff (ObservationQueueReader a ': e) b
  -> Eff e b
withQueue queueLimit e = do
  q   <- liftIO (newTBQueueIO queueLimit)
  res <- liftTry @SomeException (runReader (ObservationQueue q) e)
  liftIO (atomically (flushTBQueue q))
  either (\e -> logError (show e) >> liftIO (throwIO e)) return res
