-- | Observer Effects
--
-- This module supports the implementation of observers and observables. Expected use
-- case is event propagation.
--
-- @since 0.16.0
module Control.Eff.Concurrent.Protocol.Observer
  ( Observer(..)
  , Pdu(RegisterObserver, ForgetObserver, Observed)
  , registerObserver
  , forgetObserver
  , ObserverRegistry
  , ObserverState
  , Observers()
  , emptyObservers
  , handleObserverRegistration
  , manageObservers
  , observed
  )
where

import           Control.DeepSeq               ( NFData(rnf) )
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
import           Data.Typeable                 hiding ( cast )
import           Data.Type.Pretty
import           GHC.Generics
import           GHC.Stack

-- * Observers

-- ** Observables

-- | A /protocol/ to communicate 'Observed' events from a sources to many sinks.
--
-- A sink is any process that serves a protocol with a 'Pdu' instance that embeds
-- the 'Observer' Pdu via an 'EmbedProtocol' instance.
--
-- @since 0.28.0
data Observer event =
  MkObserver { _observerSerializer :: Serializer event
             , _observerProcessId  :: ProcessId
             , _observerMonitorReference :: MonitorReference
             }
  deriving (Generic, Typeable)

instance NFData event => NFData (Observer event)

type instance ToPretty (Observer event) =
  PrettyParens ("observing" <:> ToPretty event)

instance (Typeable r, Tangible event) => HasPdu (Observer event) (r :: Synchronicity) where
  data Pdu (Observer event) r where
    Observed :: event -> Pdu (Observer event) 'Asynchronous
    deriving Typeable

instance NFData event => NFData (Pdu (Observer event) r) where
  rnf (Observed event) = rnf event

instance Show event => Show (Pdu (Observer event) r) where
  showsPrec d (Observed event) =
    showParen (d >= 10) (showString "observered: " . shows event)

type IsObservable eventSource event =
  ( Tangible event
  , EmbedProtocol eventSource (ObserverRegistry event) 'Asynchronous
  )

type IsObserver eventSink event =
  ( Tangible event
  , EmbedProtocol eventSink (Observer event) 'Asynchronous
  )

-- | And an 'Observer' to the set of recipients for all observations reported by 'observed'.
--   Note that the observers are keyed by the observing process, i.e. a previous entry for the process
--   contained in the 'Observer' is overwritten. If you want multiple entries for a single process, just
--   combine several filter functions.
--
-- @since 0.16.0
registerObserver
  :: forall eventSink eventSource event q r .
     ( HasCallStack
     , HasProcesses r q
     , EmbedProtocol eventSource (ObserverRegistry event) 'Asynchronous
     , EmbedProtocol eventSink (Observer event) 'Asynchronous
     )
  => Endpoint eventSource
  -> Endpoint eventSink
  -> Eff r ()
registerObserver eventSource eventSink = do
   monRef <- monitor eventSink'
   cast eventSource (RegisterObserver (MkObserver serializer eventSink' monRef))
    where
       serializer = MkSerializer (toStrictDynamic . embedPdu @eventSink @(Observer event))
       eventSink' = eventSink ^. fromEndpoint


-- | Send the 'ForgetObserver' message
--
-- @since 0.16.0
forgetObserver
  :: forall eventSink eventSource event q r .
     ( HasProcesses r q
     , HasCallStack
     , EmbedProtocol eventSource (ObserverRegistry event) 'Asynchronous
     , EmbedProtocol eventSink (Observer event) 'Asynchronous
     )
  => Endpoint eventSource
  -> Endpoint eventSink
  -> Eff r ()
forgetObserver eventSource eventSink =
  cast eventSource (ForgetObserver eventSink)

-- ** Observer Support Functions

-- * Managing Observers

-- | A protocol for managing 'Observer's, encompassing  registration and de-registration of
-- 'Observer's.
--
-- @since 0.16.0
data ObserverRegistry (event :: Type)
  deriving Typeable

type instance ToPretty (ObserverRegistry event) =
  PrettyParens ("observer registry" <:> ToPretty event)

instance (Tangible event, Typeable r) => HasPdu (ObserverRegistry event) r where
  -- | Protocol for managing observers. This can be added to any server for any number of different observation types.
  -- The functions 'manageObservers' and 'handleObserverRegistration' are used to include observer handling;
  --
  -- @since 0.16.0
  data instance Pdu (ObserverRegistry event) r where
    -- | This message denotes that the given 'Observer' should receive observations until 'ForgetObserver' is
    --   received.
    --
    -- @since 0.16.1
    RegisterObserver :: Observer event -> Pdu (ObserverRegistry event) 'Asynchronous
    -- | This message denotes that the given 'Observer' should not receive observations anymore.
    --
    -- @since 0.16.1
    ForgetObserver ::  Observer event -> Pdu (ObserverRegistry event) 'Asynchronous
--    -- | This message denotes that a monitored process died
--    --
--    -- @since 0.28.0
--    ObserverMightBeDown :: MonitorReference -> Pdu (ObserverRegistry event) ( 'Synchronous Bool)
    deriving Typeable

instance NFData (Pdu (ObserverRegistry event) r) where
  rnf (RegisterObserver event) = rnf event
  rnf (ForgetObserver event) = rnf event

instance Show (Pdu (ObserverRegistry event) r) where
  showsPrec d (RegisterObserver event) = showParen (d >= 10) (showString "register observer: " . showsPrec 11 event)
  showsPrec d (ForgetObserver event) = showParen (d >= 10) (showString "forget observer: " . showsPrec 11 event)

-- ** Protocol for integrating 'ObserverRegistry' into processes.

-- | Provide the implementation for the 'ObserverRegistry' Protocol, this handled 'RegisterObserver' and 'ForgetObserver'
-- messages. It also adds the 'ObserverState' constraint to the effect list.
--
-- @since 0.16.0
handleObserverRegistration
  :: forall event q r
   . ( HasCallStack
     , Typeable event
     , HasSafeProcesses r q
     , Member (ObserverState event) r
     , Member Logs r
     )
  => Pdu (ObserverRegistry event) 'Asynchronous -> Eff r ()
handleObserverRegistration = \case
    RegisterObserver ob -> do
      os <- get @(Observers event)
      logDebug ( "registering "
               <> pack (show (typeOf ob))
               <> " current number of observers: "  -- TODO put this info into the process details
               <> pack (show (Set.size (view observers os))))
      put (over observers (Set.insert ob) os)

    ForgetObserver ob -> do
      os <- get @(Observers event)
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
manageObservers :: HasCallStack => Eff (ObserverState event ': r) a -> Eff r a
manageObservers = evalState (Observers Set.empty)

-- | The empty 'ObserverState'
--
-- @since 0.24.0
emptyObservers :: Observers event
emptyObservers = Observers Set.empty

-- | Internal state for 'manageObservers'
newtype Observers event =
  Observers { _observers :: Set (Observer event) }
   deriving (Eq, Ord, Typeable, Show, NFData)

-- | Alias for the effect that contains the observers managed by 'manageObservers'
type ObserverState event = State (Observers event)

observers :: Iso' (Observers event) (Set (Observer event))
observers = iso _observers Observers

-- | Report an observation to all observers.
-- The process needs to 'manageObservers' and to 'handleObserverRegistration'.
--
-- @since 0.16.0
observed
  :: forall event r q
   . ( HasProcesses r q
     , Member (ObserverState event) r
     , Tangible event
     , HasCallStack
     )
  => event
  -> Eff r ()
observed observation = do
  os <- view observers <$> get
  mapM_ notifySomeObserver os
 where
  notifySomeObserver (MkObserver serializer destination _) =
    traverse_ (cast receiver) (messageFilter observation)
