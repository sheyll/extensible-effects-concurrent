-- | Observer Effects
--
-- This module supports the implementation of observers and observables. Expected use
-- case is event propagation.
--
-- @since 0.16.0
module Control.Eff.Concurrent.Protocol.Observer
  ( Observer(..)
  , TangibleObserver
  , Pdu(RegisterObserver, ForgetObserver, Observed)
  , registerObserver
  , forgetObserver
  , handleObservations
  , toObserver
  , toObserverFor
  , ObserverRegistry
  , ObserverState
  , Observers()
  , emptyObservers
  , handleObserverRegistration
  , manageObservers
  , observed
  )
where

import           Control.DeepSeq               (NFData(rnf))
import           Control.Eff
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Protocol
import           Control.Eff.Concurrent.Protocol.Client
import           Control.Eff.State.Strict
import           Control.Eff.Log
import           Control.Lens
import           Data.Data                     (typeOf)
import           Data.Dynamic
import           Data.Foldable
import           Data.Kind
import           Data.Proxy
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( pack )
import           Data.Typeable                  ( typeRep )
import           Data.Type.Pretty
import           GHC.Stack

-- * Observers

-- | Describes a process that observes another via 'Asynchronous' 'Pdu' messages.
--
-- An observer consists of a filter and a process id. The filter converts an observation to
-- a message understood by the observer process, and the 'ProcessId' is used to send the message.
--
-- @since 0.16.0
data Observer o where
  Observer
    :: ( Tangible o
       , IsPdu p 'Asynchronous
       , Tangible (Endpoint p)
       , Typeable p
       )
    => (o -> Maybe (Pdu p 'Asynchronous)) -> Endpoint p -> Observer o


-- | The constraints on the type parameters to an 'Observer'
--
-- @since 0.24.0
type TangibleObserver o =
  ( Tangible o, IsPdu (Observer o) 'Asynchronous)

type instance ToPretty (Observer o) =
  PrettyParens ("observing" <:> ToPretty o)

instance (NFData o) => NFData (Observer o) where
  rnf (Observer k s) = rnf k `seq` rnf s

instance Show (Observer o) where
  showsPrec d (Observer _ p) = showParen
    (d >= 10)
    (shows (typeRep (Proxy :: Proxy o)) . showString " observer: " . shows p)

instance Ord (Observer o) where
  compare (Observer _ s1) (Observer _ s2) =
    compare (s1 ^. fromEndpoint) (s2 ^. fromEndpoint)

instance Eq (Observer o) where
  (==) (Observer _ s1) (Observer _ s2) =
    (==) (s1 ^. fromEndpoint) (s2 ^. fromEndpoint)

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
     , TangibleObserver o
     , EmbedProtocol x (ObserverRegistry o)
     , IsPdu x 'Asynchronous
     )
  => Observer o
  -> Endpoint x
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
     , NFData o
     , EmbedProtocol x (ObserverRegistry o)
     , IsPdu x 'Asynchronous
     )
  => Observer o
  -> Endpoint x
  -> Eff r ()
forgetObserver observer observerRegistry =
  cast observerRegistry (ForgetObserver observer)

-- ** Observer Support Functions

-- | A minimal Protocol for handling observations.
-- This is one simple way of receiving observations - of course users can use
-- any other 'Asynchronous' 'Pdu' message type for receiving observations.
--
-- @since 0.16.0
instance (NFData o, Show o, Typeable o, Typeable r) => IsPdu (Observer o) r where
  data instance Pdu (Observer o) r where
    -- | This message denotes that the given value was 'observed'.
    --
    -- @since 0.16.1
    Observed :: o -> Pdu (Observer o) 'Asynchronous
    deriving Typeable

instance NFData o => NFData (Pdu (Observer o) r) where
  rnf (Observed o) = rnf o

instance Show o => Show (Pdu (Observer o) r) where
  showsPrec d (Observed o) = showParen (d>=10) (showString "observered: " . shows o)

-- | Based on the 'Pdu' instance for 'Observer' this simplified writing
-- a callback handler for observations. In order to register to
-- and 'ObserverRegistry' use 'toObserver'.
--
-- @since 0.16.0
handleObservations
  :: (HasCallStack, Typeable o, SetMember Process (Process q) r, NFData (Observer o))
  => (o -> Eff r ())
  -> Pdu (Observer o) 'Asynchronous -> Eff r ()
handleObservations k (Observed r) = k r

-- | Use a 'Endpoint' as an 'Observer' for 'handleObservations'.
--
-- @since 0.16.0
toObserver
  :: forall o p
  . ( IsPdu p 'Asynchronous
    , EmbedProtocol p (Observer o)
    , TangibleObserver o
    )
  => Endpoint p
  -> Observer o
toObserver = toObserverFor (embedPdu @p . Observed)

-- | Create an 'Observer' that conditionally accepts all observations of the
-- given type and applies the given function to them; the function takes an observation and returns an 'Pdu'
-- cast that the observer server is compatible to.
--
-- @since 0.16.0
toObserverFor
  :: (TangibleObserver o, Typeable a, IsPdu a 'Asynchronous)
  => (o -> Pdu a 'Asynchronous)
  -> Endpoint a
  -> Observer o
toObserverFor wrapper = Observer  (Just . wrapper)

-- * Managing Observers

-- | A protocol for managing 'Observer's, encompassing  registration and de-registration of
-- 'Observer's.
--
-- @since 0.16.0
data ObserverRegistry (o :: Type)
  deriving Typeable

type instance ToPretty (ObserverRegistry o) =
  PrettyParens ("observer registry" <:> ToPretty o)

instance (Typeable o, Typeable r) => IsPdu (ObserverRegistry o) r where
  -- | Protocol for managing observers. This can be added to any server for any number of different observation types.
  -- The functions 'manageObservers' and 'handleObserverRegistration' are used to include observer handling;
  --
  -- @since 0.16.0
  data instance Pdu (ObserverRegistry o) r where
    -- | This message denotes that the given 'Observer' should receive observations until 'ForgetObserver' is
    --   received.
    --
    -- @since 0.16.1
    RegisterObserver :: NFData o => Observer o -> Pdu (ObserverRegistry o) 'Asynchronous
    -- | This message denotes that the given 'Observer' should not receive observations anymore.
    --
    -- @since 0.16.1
    ForgetObserver :: NFData o => Observer o -> Pdu (ObserverRegistry o) 'Asynchronous
    deriving Typeable

instance NFData (Pdu (ObserverRegistry o) r) where
  rnf (RegisterObserver o) = rnf o
  rnf (ForgetObserver o) = rnf o

instance Show (Pdu (ObserverRegistry o) r) where
  showsPrec d (RegisterObserver o) = showParen (d >= 10) (showString "register observer: " . showsPrec 11 o)
  showsPrec d (ForgetObserver o) = showParen (d >= 10) (showString "forget observer: " . showsPrec 11 o)

-- ** Protocol for integrating 'ObserverRegistry' into processes.

-- | Provide the implementation for the 'ObserverRegistry' Protocol, this handled 'RegisterObserver' and 'ForgetObserver'
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
  => Pdu (ObserverRegistry o) 'Asynchronous -> Eff r ()
handleObserverRegistration = \case
    RegisterObserver ob -> do
      os <- get @(Observers o)
      logDebug ("registering "
               <> pack (show (typeOf ob))
               <> " current number of observers: "
               <> pack (show (Set.size (view observers os))))
      put (over observers (Set.insert ob) os)

    ForgetObserver ob -> do
      os <- get @(Observers o)
      logDebug ("forgetting "
               <> pack (show (typeOf ob))
               <> " current number of observers: "
               <> pack (show (Set.size (view observers os))))
      put (over observers (Set.delete ob) os)

-- | Keep track of registered 'Observer's.
--
-- Handle the 'ObserverState' introduced by 'handleObserverRegistration'.
--
-- @since 0.16.0
manageObservers :: Eff (ObserverState o ': r) a -> Eff r a
manageObservers = evalState (Observers Set.empty)

-- | The empty 'ObserverState'
--
-- @since 0.24.0
emptyObservers :: Observers o
emptyObservers = Observers Set.empty

-- | Internal state for 'manageObservers'
newtype Observers o =
  Observers { _observers :: Set (Observer o) }
   deriving (Eq, Ord, Typeable, Show, NFData)

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
     , TangibleObserver o
     )
  => o
  -> Eff r ()
observed observation = do
  os <- view observers <$> get
  mapM_ notifySomeObserver os
 where
  notifySomeObserver (Observer messageFilter receiver) =
    traverse_ (cast receiver) (messageFilter observation)
