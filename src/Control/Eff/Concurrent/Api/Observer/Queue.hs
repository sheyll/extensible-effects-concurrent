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
import           Control.Monad                  ( unless )
import           Data.Typeable
import           Text.Printf
import           GHC.Stack

-- | Contains a 'TBQueue' capturing observations received by 'enqueueObservationsRegistered'
-- or 'enqueueObservations'.
newtype ObservationQueue a = ObservationQueue (TBQueue (Observation a))

-- | A 'Reader' for an 'ObservationQueue'.
type ObservationQueueReader a = Reader (ObservationQueue a)

logPrefix :: forall o proxy . (HasCallStack, Typeable o) => proxy o -> String
logPrefix px = "<observation queue " ++ show (typeRep px) ++ ">"

-- | Read queued observations captured by observing a 'Server' that implements
-- an 'Observable' 'Api' using 'enqueueObservationsRegistered' or 'enqueueObservations'.
-- This blocks until the next 'Observation' received. For a non-blocking
-- variant use 'tryReadObservationQueue' or 'flushObservationQueue'.
readObservationQueue
  :: forall o r
   . ( Member (ObservationQueueReader o) r
     , HasCallStack
     , MonadIO (Eff r)
     , Typeable o
     , Member (Logs LogMessage) r
     )
  => Eff r (Observation o)
readObservationQueue = withFrozenCallStack $ do
  ObservationQueue q <- ask @(ObservationQueue o)
  logDebug ((logPrefix (Proxy @o)) ++ " reading")
  liftIO (atomically (readTBQueue q))

-- | Read queued observations captured by observing a 'Server' that implements
-- an 'Observable' 'Api' using 'enqueueObservationsRegistered' or 'enqueueObservations'.
-- Return the next 'Observation' immediately or 'Nothing' if the queue is empty.
-- Use 'readObservationQueue' to block until an observation is observed.
tryReadObservationQueue
  :: forall o r
   . ( Member (ObservationQueueReader o) r
     , HasCallStack
     , MonadIO (Eff r)
     , Typeable o
     , Member (Logs LogMessage) r
     )
  => Eff r (Maybe (Observation o))
tryReadObservationQueue = withFrozenCallStack $ do
  logDebug ((logPrefix (Proxy @o)) ++ " try reading")
  ObservationQueue q <- ask @(ObservationQueue o)
  liftIO (atomically (tryReadTBQueue q))

-- | Read all currently queued 'Observation's captured by 'enqueueObservations'.
-- This returns immediately all currently enqueued 'Observation's. For a blocking
-- variant use 'readObservationQueue'.
flushObservationQueue
  :: forall o r
   . ( Member (ObservationQueueReader o) r
     , HasCallStack
     , MonadIO (Eff r)
     , Typeable o
     , Member (Logs LogMessage) r
     )
  => Eff r [Observation o]
flushObservationQueue = withFrozenCallStack $ do
  logDebug ((logPrefix (Proxy @o)) ++ " flush")
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
enqueueObservationsRegistered px queueLimit k = withFrozenCallStack $ do
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
enqueueObservations px oSvr queueLimit k = withFrozenCallStack $ withQueue
  queueLimit
  (do
    ObservationQueue q <- ask @(ObservationQueue o)
    do
      logDebug
        (printf "%s starting with queue limit: %d"
                (logPrefix (Proxy @o))
                queueLimit
        )
      cbo <- spawnCallbackObserver
        px
        (\from observation -> do
          logDebug
            (printf "%s enqueue observation %s from %s"
                    (logPrefix (Proxy @o))
                    (show observation)
                    (show from)
            )
          liftIO (atomically (writeTBQueue q observation))
          return True
        )
      logDebug
        (printf "%s started observer process %s"
                (logPrefix (Proxy @o))
                (show cbo)
        )
      registerObserver SchedulerProxy cbo oSvr
      logDebug
        (printf "%s registered at: %s" (logPrefix (Proxy @o)) (show oSvr))
      logDebug (printf "%s running" (logPrefix (Proxy @o)))
      res <- k
      logDebug (printf "%s finished" (logPrefix (Proxy @o)))
      forgetObserver SchedulerProxy cbo oSvr
      logDebug (printf "%s unregistered" (logPrefix (Proxy @o)))
      sendShutdown px (_fromServer cbo)
      logDebug (printf "%s stopped observer process" (logPrefix (Proxy @o)))
      return res
  )

withQueue
  :: forall a b e
   . ( HasCallStack
     , MonadIO (Eff e)
     , Member (Logs LogMessage) e
     , Typeable a
     , Show (Observation a)
     , SetMember Lift (Lift IO) e
     )
  => Int
  -> Eff (ObservationQueueReader a ': e) b
  -> Eff e b
withQueue queueLimit e = do
  q    <- liftIO (newTBQueueIO queueLimit)
  res  <- liftTry @SomeException (runReader (ObservationQueue q) e)
  rest <- liftIO (atomically (flushTBQueue q))
  unless
    (null rest)
    (logNotice (logPrefix (Proxy @a) ++ " unread observations: " ++ show rest))
  either (\em -> logError (show em) >> liftIO (throwIO em)) return res
