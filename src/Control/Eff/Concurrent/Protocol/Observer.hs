-- | Observer Effects
--
-- This module supports the implementation of observers and observables. Expected use
-- case is event propagation.
--
-- @since 0.16.0
module Control.Eff.Concurrent.Protocol.Observer
  ( Observer(..)
  , ObservationSink(..)
  , IsObservable
  , CanObserve
  , Pdu(RegisterObserver, ForgetObserver, Observed)
  , registerObserver
  , forgetObserver
  , forgetObserverUnsafe
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
import           Control.Eff.Concurrent.Misc
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Protocol
import           Control.Eff.Concurrent.Protocol.Client
import           Control.Eff.State.Strict
import           Control.Eff.Log
import           Control.Lens
import           Data.Dynamic
import           Data.Foldable
import           Data.Kind
import           Data.Semigroup
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( pack )
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
-- This type has /dual use/, for one it serves as type-index for 'Pdu', i.e.
-- 'HasPdu' respectively, and secondly it contains an 'ObservationSink' and
-- a 'MonitorReference'.
--
-- The 'ObservationSink' is used to serialize and send the 'Observed' events,
-- while the 'ProcessId' serves as key for internal maps.
--
-- @since 0.28.0
newtype Observer event =
  MkObserver (Arg ProcessId (ObservationSink event))
  deriving (Eq, Ord, Typeable)

instance NFData (Observer event) where
  rnf (MkObserver (Arg x y)) = rnf x `seq` rnf y

instance Typeable event => Show (Observer event) where
  showsPrec d (MkObserver (Arg x (MkObservationSink _ m))) =
    showParen (d>=10) (showSTypeable @event . showString "-observer: " . shows x . showChar ' ' . shows m )

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

-- | The Information necessary to wrap an 'Observed' event to a process specific
-- message, e.g. the embedded 'Observer' 'Pdu' instance, and the 'MonitorReference' of
-- the destination process.
--
-- @since 0.28.0
data ObservationSink event =
  MkObservationSink
    { _observerSerializer :: Serializer (Pdu (Observer event) 'Asynchronous)
    , _observerMonitorReference  :: MonitorReference
    }
  deriving (Generic, Typeable)

instance NFData (ObservationSink event) where
  rnf (MkObservationSink s p) = s `seq` rnf p

-- | Convenience type alias.
--
-- @since 0.28.0
type IsObservable eventSource event =
  ( Tangible event
  , EmbedProtocol eventSource (ObserverRegistry event) 'Asynchronous
  )

-- | Convenience type alias.
--
-- @since 0.28.0
type CanObserve eventSink event =
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
  :: forall event eventSink eventSource r q .
     ( HasCallStack
     , HasProcesses r q
     , IsObservable eventSource event
     , CanObserve eventSink event
     )
  => Endpoint eventSource
  -> Endpoint eventSink
  -> Eff r ()
registerObserver eventSource eventSink = do
   monRef <- monitor eventSink'
   cast eventSource (RegisterObserver (MkObserver (Arg eventSink' (MkObservationSink serializer monRef))))
    where
       serializer = MkSerializer (toStrictDynamic . embedPdu @eventSink @(Observer event) @( 'Asynchronous ))
       eventSink' = eventSink ^. fromEndpoint


-- | Send the 'ForgetObserver' message
--
-- @since 0.16.0
forgetObserver
  :: forall event eventSink eventSource r q .
     ( HasProcesses r q
     , HasCallStack
     , IsObservable eventSource event
     , CanObserve eventSink event
     )
  => Endpoint eventSource
  -> Endpoint eventSink
  -> Eff r ()
forgetObserver eventSource eventSink =
  forgetObserverUnsafe @event @eventSource eventSource (eventSink ^. fromEndpoint)

-- | Send the 'ForgetObserver' message, use a raw 'ProcessId' as parameter.
--
-- @since 0.28.0
forgetObserverUnsafe
  :: forall event eventSource r q .
     ( HasProcesses r q
     , HasCallStack
     , IsObservable eventSource event
     )
  => Endpoint eventSource
  -> ProcessId
  -> Eff r ()
forgetObserverUnsafe eventSource eventSink =
  cast eventSource (ForgetObserver @event eventSink)

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
    ForgetObserver ::  ProcessId -> Pdu (ObserverRegistry event) 'Asynchronous
--    -- | This message denotes that a monitored process died
--    --
--    -- @since 0.28.0
--    ObserverMightBeDown :: MonitorReference -> Pdu (ObserverRegistry event) ( 'Synchronous Bool)
    deriving Typeable

instance NFData (Pdu (ObserverRegistry event) r) where
  rnf (RegisterObserver event) = rnf event
  rnf (ForgetObserver event) = rnf event

instance Typeable event => Show (Pdu (ObserverRegistry event) r) where
  showsPrec d (RegisterObserver observer) = showParen (d >= 10) (showString "register observer: " . showsPrec 11 observer)
  showsPrec d (ForgetObserver p) = showParen (d >= 10) (showString "forget observer: " . showsPrec 11 p)

-- ** Protocol for integrating 'ObserverRegistry' into processes.

-- | Provide the implementation for the 'ObserverRegistry' Protocol, this handled 'RegisterObserver' and 'ForgetObserver'
-- messages. It also adds the 'ObserverState' constraint to the effect list.
--
-- @since 0.16.0
handleObserverRegistration
  :: forall event q r
   . ( HasCallStack
     , Typeable event
     , HasProcesses r q
     , Member (ObserverState event) r
     , Member Logs r
     )
  => Pdu (ObserverRegistry event) 'Asynchronous -> Eff r ()
handleObserverRegistration = \case
    RegisterObserver observer@(MkObserver (Arg pid sink)) -> do
      modify @(Observers event) (over observers (Map.insert pid sink))
      os <- get @(Observers event)
      logDebug ( "registered "
               <> pack (show observer)
               <> " current number of observers: "  -- TODO put this info into the process details
               <> pack (show (Map.size (view observers os))))

    ForgetObserver ob -> do
      mSink <- view (observers . at ob) <$> get @(Observers event)
      modify @(Observers event) (observers . at ob .~ Nothing)
      os <- get @(Observers event)
      logDebug ("forgot "
               <> (maybe
                      (pack (show ("unknown observer " ++ show ob)))
                      (pack . show . MkObserver . Arg ob)
                      mSink
                    )
               <> " current number of observers: "
               <> pack (show (Map.size (view observers os))))
      traverse_ (\ (MkObservationSink _ monRef) -> demonitor monRef) mSink
-- | Keep track of registered 'Observer's.
--
-- Handle the 'ObserverState' introduced by 'handleObserverRegistration'.
--
-- @since 0.16.0
manageObservers :: HasCallStack => Eff (ObserverState event ': r) a -> Eff r a
manageObservers = evalState (Observers Map.empty)

-- | The empty 'ObserverState'
--
-- @since 0.24.0
emptyObservers :: Observers event
emptyObservers = Observers Map.empty

-- | Internal state for 'manageObservers'
newtype Observers event = Observers { _observers :: Map ProcessId (ObservationSink event) }

-- | Alias for the effect that contains the observers managed by 'manageObservers'
type ObserverState event = State (Observers event)

observers :: Iso' (Observers event) (Map ProcessId (ObservationSink event))
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
  mapM_ notifySomeObserver (Map.assocs os)
 where
  notifySomeObserver (destination,  (MkObservationSink serializer _)) =
    sendAnyMessage destination (runSerializer serializer (Observed observation))
