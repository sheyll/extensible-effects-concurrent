-- | Observer Effects
--
-- This module supports the implementation of observers and observables. Expected use
-- case is event propagation.
--
-- @since 0.16.0
module Control.Eff.Concurrent.Api.Observer
  ()
where

import           GHC.Stack
import           Data.Dynamic
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Control.Eff
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Client
import           Control.Eff.Concurrent.Api.Server2
import           Control.Eff.Log
import           Control.Eff.State.Strict
import           Data.Foldable
import           Control.Lens

-- | Describes a process that observes another via 'Asynchronous' 'Api' messages.
-- An observer consists of a filter and a process id. The filter converts an observation to
-- a message understood by the observer process, and the 'ProcessId' is used to send the message.
--
-- @since 0.16.0
data Observer o where
  Observer
    :: (Show (Server p), Typeable p, Typeable o)
    => (o -> Maybe (Api p 'Asynchronous)) -> Server p -> Observer o

instance Show (Observer o) where
  showsPrec d (Observer _ p) =
    showParens (d >= 10) (shows (typerep) . showString "")

instance Ord (Observer o) where
  compare (Observer _ s1) (Observer _ s2) =
    compare (s1 ^. fromServer) (s2 ^. fromServer)

instance Eq (Observer o) where
  (==) (Observer _ s1) (Observer _ s2) =
    (==) (s1 ^. fromServer) (s2 ^. fromServer)

-- * External Observer management API

-- | And an 'Observer' to the set of reciepients for all observations reported by 'observed'.
--   Note that the observers are keyed by the observing process, i.e. a previous entry for the process
--   contained in the 'Observer' is overwritten. If you want multiple entries for a single process, just
--   combine several filter functions.
--
-- @since 0.16.0
registerObserver
  :: (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
  => SchedulerProxy q
  -> Observer o
  -> Server (ObserverRegistry o)
  -> Eff r ()
registerObserver px observer observed =
  cast px observed (RegisterObserver observer)

-- | Send the 'forgetObserverMessage'
--
-- @since 0.16.0
forgetObserver
  :: (SetMember Process (Process q) r, HasCallStack, Member Interrupts r)
  => SchedulerProxy q
  -> Observer o
  -> Server (ObserverRegistry o)
  -> Eff r ()
forgetObserver px observer observed =
  cast px observed (ForgetObserver observer)

-- | An 'Api' for managing 'Observer's, encompassing  registration and de-registration of
-- 'Observer's.
--
-- @since 0.16.0
data ObserverRegistry o

-- | Api for managing observers. This can be added to any server for any number of different observation types.
-- The functions 'manageObservers' and 'handleObserverApi' are used to include observer handling;
--
-- @since 0.16.0
data instance Api (ObserverRegistry o) r where
  RegisterObserver :: Observer o -> Api (ObserverRegistry o) 'Asynchronous
  ForgetObserver :: Observer o -> Api (ObserverRegistry o) 'Asynchronous


-- ** Api for integrating 'ObserverRegistry' into processes.

-- | Provide the implementation for the 'Observerd' Api, this handled 'RegisterObserver' and 'ForgetObserver'
-- messages. It also adds the 'ObserverState' constraint to the effect list.
--
-- @since 0.16.0
handleObserverRegistration
  :: forall o q r
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Member (ObserverState o) r
     )
  => MessageCallback (ObserverRegistry o) r
handleObserverRegistration = handleCasts
  (\case
    RegisterObserver ob ->
      get @(Observers o) >>= put . over observers (mappend ob)
    ForgetObserver ob ->
      get @(Observers o) >>= put . over observers (Set.delete ob)
  )


-- | Keep track of registered 'Observer's Observers can be added and removed,
-- and an 'Observation' can be sent to all registerd observers at once.
--
-- @since 0.16.0
manageObservers :: Eff (ObserverState o ': r) a -> Eff r a
manageObservers = evalState (Observers Set.empty)

-- | Internal state for 'manageObservers'
data Observers o =
  Observers { _observers :: Set (Observer o) }

-- | Alias for the effect that contains the observers managed by 'manageObservers'
type ObserverState o = State (Observers o)

observers :: Iso' (Observers o) (Set (Observer o))
observers = iso _observers Observers

-- | Report an observation to all observers.
-- The process needs to 'manageObservers' and to 'handleObserverRegistration'.
--
-- @since 0.16.0
observed
  :: forall o r q
   . ( SetMember Process (Process q) r
     , Member (ObserverState o) r
     , Member Interrupts r
     )
  => o
  -> Eff r ()
observed observation = do
  me <- asServer @o <$> self SP
  os <- view observers <$> get
  mapM_ (notifySomeObserver me) os
 where
  notifySomeObserver me (Observer messageFilter receiver) =
    traverse_ (cast SP receiver) (messageFilter observation)
