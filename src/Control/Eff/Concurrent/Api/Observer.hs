-- | Observer Effects
--
-- This module supports the implementation of observers and observables. One use
-- case is event propagation. The tools in this module are tailored towards
-- 'Api' servers/clients.
module Control.Eff.Concurrent.Api.Observer
  ( -- * Observation API
    Observer(..)
  , Observable(..)
  , notifyObserver
  , registerObserver
  , forgetObserver
  -- ** Generalized observation
  , SomeObserver(..)
  , notifySomeObserver
  , Observers()
  , ObserverState
  , manageObservers
  , addObserver
  , removeObserver
  , notifyObservers
  -- * Callback 'Observer'
  , CallbackObserver
  , spawnCallbackObserver
  , spawnLoggingObserver
  )
where

import           GHC.Stack
import           Data.Dynamic
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Control.Eff
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Client
import           Control.Eff.Concurrent.Api.Server
import           Control.Eff.Log
import           Control.Eff.State.Strict
import           Control.Lens

-- | An 'Api' index that support observation of the
-- another 'Api' that is 'Observable'.
class (Typeable p, Observable o) => Observer p o where
  -- | Wrap the 'Observation' and the 'ProcessId' (i.e. the 'Server')
  -- that caused the observation into an 'Api' value that the
  -- 'Observable' understands.
  observationMessage :: Server o -> Observation o -> Api p 'Asynchronous

-- | An 'Api' index that supports registration and de-registration of
-- 'Observer's.
class (Typeable o, Typeable (Observation o)) => Observable o where
  -- | Type of observations visible on this observable
  data Observation o
  -- | Return the 'Api' value for the 'cast_' that registeres an observer
  registerObserverMessage :: SomeObserver o -> Api o 'Asynchronous
  -- | Return the 'Api' value for the 'cast_' that de-registeres an observer
  forgetObserverMessage :: SomeObserver o -> Api o 'Asynchronous

-- | Send an 'Observation' to an 'Observer'
notifyObserver
  :: (SetMember Process (Process q) r, Observable o, Observer p o, HasCallStack)
  => SchedulerProxy q
  -> Server p
  -> Server o
  -> Observation o
  -> Eff r ()
notifyObserver px observer observed observation =
  cast px observer (observationMessage observed observation)

-- | Send the 'registerObserverMessage'
registerObserver
  :: (SetMember Process (Process q) r, Observable o, Observer p o, HasCallStack)
  => SchedulerProxy q
  -> Server p
  -> Server o
  -> Eff r ()
registerObserver px observer observed =
  cast px observed (registerObserverMessage (SomeObserver observer))

-- | Send the 'forgetObserverMessage'
forgetObserver
  :: (SetMember Process (Process q) r, Observable o, Observer p o)
  => SchedulerProxy q
  -> Server p
  -> Server o
  -> Eff r ()
forgetObserver px observer observed =
  cast px observed (forgetObserverMessage (SomeObserver observer))

-- | An existential wrapper around a 'Server' of an 'Observer'.
-- Needed to support different types of observers to observe the
-- same 'Observable' in a general fashion.
data SomeObserver o where
  SomeObserver :: (Show (Server p), Typeable p, Observer p o) => Server p -> SomeObserver o

deriving instance Show (SomeObserver o)

instance Ord (SomeObserver o) where
  compare (SomeObserver (Server o1)) (SomeObserver (Server o2)) =
    compare o1 o2

instance Eq (SomeObserver o) where
  (==) (SomeObserver (Server o1)) (SomeObserver (Server o2)) =
    o1 == o2

-- | Send an 'Observation' to 'SomeObserver'.
notifySomeObserver
  :: (SetMember Process (Process q) r, Observable o, HasCallStack)
  => SchedulerProxy q
  -> Server o
  -> Observation o
  -> SomeObserver o
  -> Eff r ()
notifySomeObserver px observed observation (SomeObserver observer) =
  notifyObserver px observer observed observation

-- ** Manage 'Observers's

-- | Internal state for 'manageObservers'
data Observers o =
  Observers { _observers :: Set (SomeObserver o) }

-- | Alias for the effect that contains the observers managed by 'manageObservers'
type ObserverState o = State (Observers o)

observers :: Iso' (Observers o) (Set (SomeObserver o))
observers = iso _observers Observers

-- | Keep track of registered 'Observer's Observers can be added and removed,
-- and an 'Observation' can be sent to all registerd observers at once.
manageObservers :: Eff (ObserverState o ': r) a -> Eff r a
manageObservers = evalState (Observers Set.empty)

-- | Add an 'Observer' to the 'Observers' managed by 'manageObservers'.
addObserver
  :: (SetMember Process (Process q) r, Member (ObserverState o) r, Observable o)
  => SomeObserver o
  -> Eff r ()
addObserver = modify . over observers . Set.insert

-- | Delete an 'Observer' from the 'Observers' managed by 'manageObservers'.
removeObserver
  :: (SetMember Process (Process q) r, Member (ObserverState o) r, Observable o)
  => SomeObserver o
  -> Eff r ()
removeObserver = modify . over observers . Set.delete


-- | Send an 'Observation' to all 'SomeObserver's in the 'Observers' state.
notifyObservers
  :: forall o r q
   . ( Observable o
     , SetMember Process (Process q) r
     , Member (ObserverState o) r
     )
  => SchedulerProxy q
  -> Observation o
  -> Eff r ()
notifyObservers px observation = do
  me <- asServer @o <$> self px
  os <- view observers <$> get
  mapM_ (notifySomeObserver px me observation) os

-- | An 'Observer' that schedules the observations to an effectful callback.
data CallbackObserver o
  deriving Typeable

data instance Api (CallbackObserver o) r where
  CbObserved :: (Typeable o, Typeable (Observation o)) =>
             Server o -> Observation o -> Api (CallbackObserver o) 'Asynchronous
  deriving Typeable

deriving instance Show (Observation o) => Show (Api (CallbackObserver o) r)

instance (Observable o) => Observer (CallbackObserver o) o where
  observationMessage = CbObserved

-- | Start a new process for an 'Observer' that schedules
-- all observations to an effectful callback.
spawnCallbackObserver
  :: forall o r q
   . ( SetMember Process (Process q) r
     , Typeable o
     , Show (Observation o)
     , Observable o
     , Member (Logs LogMessage) q
     , HasCallStack
     )
  => SchedulerProxy q
  -> (Server o -> Observation o -> Eff (Process q ': q) Bool)
  -> Eff r (Server (CallbackObserver o))
spawnCallbackObserver px onObserve = spawnServerWithEffects
  px
  (castHandler handleCastCbo)
  id
 where
  handleCastCbo (CbObserved fromSvr v) = do
    continueObservation <- onObserve fromSvr v
    return
      (if continueObservation then HandleNextRequest else StopApiServer Nothing)

-- | Start a new process for an 'Observer' that schedules
-- all observations to an effectful callback.
--
-- @since 0.3.0.0
spawnLoggingObserver
  :: forall o r q
   . ( SetMember Process (Process q) r
     , Typeable o
     , Show (Observation o)
     , Observable o
     , Member (Logs LogMessage) q
     , Member (Logs LogMessage) r
     , HasCallStack
     )
  => SchedulerProxy q
  -> Eff r (Server (CallbackObserver o))
spawnLoggingObserver px = spawnCallbackObserver
  px
  (\s o -> logDebug (show s ++ " OBSERVED: " ++ show o) >> return True)
