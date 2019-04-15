-- | Observer Effects
--
-- This module supports the implementation of observers and observables. Expected use
-- case is event propagation.
--
-- @since 0.16.0
module Control.Eff.Concurrent.Api.Observer
  ( Observer(..)
  , Api(RegisterObserver, ForgetObserver, Observed)
  , registerObserver
  , forgetObserver
  , handleObservations
  , toObserver
  , toObserverFor
  , ObserverRegistry
  , ObserverState
  , handleObserverRegistration
  , manageObservers
  , observed
  )
where

import           Control.Eff
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Client
import           Control.Eff.Concurrent.Api.Server
import           Control.Eff.State.Strict
import           Control.Eff.Log
import           Control.Lens
import           Data.Data                     (typeOf)
import           Data.Dynamic
import           Data.Foldable
import           Data.Proxy
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      (Text, pack)
import           Data.Typeable                  ( typeRep )
import           GHC.Stack

-- * Observers

-- | Describes a process that observes another via 'Asynchronous' 'Api' messages.
--
-- An observer consists of a filter and a process id. The filter converts an observation to
-- a message understood by the observer process, and the 'ProcessId' is used to send the message.
--
-- @since 0.16.0
data Observer o where
  Observer
    :: (Show (Server p), Typeable p, Typeable o)
    => (o -> Maybe (Api p 'Asynchronous)) -> Server p -> Observer o

instance Show (Observer o) where
  showsPrec d (Observer _ p) = showParen
    (d >= 10)
    (shows (typeRep (Proxy :: Proxy o)) . showString " observer: " . shows p)

instance Ord (Observer o) where
  compare (Observer _ s1) (Observer _ s2) =
    compare (s1 ^. fromServer) (s2 ^. fromServer)

instance Eq (Observer o) where
  (==) (Observer _ s1) (Observer _ s2) =
    (==) (s1 ^. fromServer) (s2 ^. fromServer)

-- | And an 'Observer' to the set of recipients for all observations reported by 'observed'.
--   Note that the observers are keyed by the observing process, i.e. a previous entry for the process
--   contained in the 'Observer' is overwritten. If you want multiple entries for a single process, just
--   combine several filter functions.
--
-- @since 0.16.0
registerObserver
  :: ( SetMember Process (Process q) r
     , HasCallStack
     , Member Interrupts r
     , Typeable o
     )
  => Observer o
  -> Server (ObserverRegistry o)
  -> Eff r ()
registerObserver observer observerRegistry =
  cast observerRegistry (RegisterObserver observer)

-- | Send the 'ForgetObserver' message
--
-- @since 0.16.0
forgetObserver
  :: ( SetMember Process (Process q) r
     , HasCallStack
     , Member Interrupts r
     , Typeable o
     )
  => Observer o
  -> Server (ObserverRegistry o)
  -> Eff r ()
forgetObserver observer observerRegistry =
  cast observerRegistry (ForgetObserver observer)

-- ** Observer Support Functions

-- | A minimal Api for handling observations.
-- This is one simple way of receiving observations - of course users can use
-- any other 'Asynchronous' 'Api' message type for receiving observations.
--
-- @since 0.16.0
data instance Api (Observer o) r where
  -- | This message denotes that the given value was 'observed'.
  --
  -- @since 0.16.1
  Observed :: o -> Api (Observer o) 'Asynchronous

-- | Based on the 'Api' instance for 'Observer' this simplified writing
-- a callback handler for observations. In order to register to
-- and 'ObserverRegistry' use 'toObserver'.
--
-- @since 0.16.0
handleObservations
  :: (HasCallStack, Typeable o, SetMember Process (Process q) r)
  => (o -> Eff r CallbackResult)
  -> MessageCallback (Observer o) r
handleObservations k = handleCasts
  (\case
    Observed o -> k o
  )

-- | Use a 'Server' as an 'Observer' for 'handleObservations'.
--
-- @since 0.16.0
toObserver :: Typeable o => Server (Observer o) -> Observer o
toObserver = toObserverFor Observed

-- | Create an 'Observer' that conditionally accepts all observations of the
-- given type and applies the given function to them; the function takes an observation and returns an 'Api'
-- cast that the observer server is compatible to.
--
-- @since 0.16.0
toObserverFor
  :: (Typeable a, Typeable o)
  => (o -> Api a 'Asynchronous)
  -> Server a
  -> Observer o
toObserverFor wrapper = Observer (Just . wrapper)

-- * Managing Observers

-- | An 'Api' for managing 'Observer's, encompassing  registration and de-registration of
-- 'Observer's.
--
-- @since 0.16.0
data ObserverRegistry o

-- | Api for managing observers. This can be added to any server for any number of different observation types.
-- The functions 'manageObservers' and 'handleObserverRegistration' are used to include observer handling;
--
-- @since 0.16.0
data instance Api (ObserverRegistry o) r where
  -- | This message denotes that the given 'Observer' should receive observations until 'ForgetObserver' is
  --   received.
  --
  -- @since 0.16.1
  RegisterObserver :: Observer o -> Api (ObserverRegistry o) 'Asynchronous
  -- | This message denotes that the given 'Observer' should not receive observations anymore.
  --
  -- @since 0.16.1
  ForgetObserver :: Observer o -> Api (ObserverRegistry o) 'Asynchronous


-- ** Api for integrating 'ObserverRegistry' into processes.

-- | Provide the implementation for the 'ObserverRegistry' Api, this handled 'RegisterObserver' and 'ForgetObserver'
-- messages. It also adds the 'ObserverState' constraint to the effect list.
--
-- @since 0.16.0
handleObserverRegistration
  :: forall o q r
   . ( HasCallStack
     , Typeable o
     , SetMember Process (Process q) r
     , Member (ObserverState o) r
     , Member Logs r
     )
  => MessageCallback (ObserverRegistry o) r
handleObserverRegistration = handleCasts
  (\case
    RegisterObserver ob -> do
      os <- get @(Observers o)
      logDebug ("registering "
               <> pack (show (typeOf ob))
               <> " current number of observers: "
               <> pack (show (Set.size (view observers os))))
      put (over observers (Set.insert ob)os)
      pure AwaitNext
    ForgetObserver ob -> do
      os <- get @(Observers o)
      logDebug ("forgetting "
               <> pack (show (typeOf ob))
               <> " current number of observers: "
               <> pack (show (Set.size (view observers os))))
      put (over observers (Set.delete ob) os)
      pure AwaitNext
  )


-- | Keep track of registered 'Observer's.
--
-- Handle the 'ObserverState' introduced by 'handleObserverRegistration'.
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
  os <- view observers <$> get
  mapM_ notifySomeObserver os
 where
  notifySomeObserver (Observer messageFilter receiver) =
    traverse_ (cast receiver) (messageFilter observation)
