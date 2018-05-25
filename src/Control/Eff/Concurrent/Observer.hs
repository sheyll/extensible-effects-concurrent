-- | Observer Effects
--
-- This module supports the implementation of observers and observables. One
-- more concrete perspective might be to understand observers as event listeners
-- and observables as event sources. The tools in this module are tailored
-- towards 'Control.Eff.Concurrent.GenServer.Api' endpoints
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Control.Eff.Concurrent.Observer
  ( Observer(..)
  , Observable(..)
  , notifyObserver
  , registerObserver
  , forgetObserver
  , SomeObserver(..)
  , notifySomeObserver
  , Observers()
  , manageObservers
  , addObserver
  , removeObserver
  , notifyObservers
  , CallbackObserver
  , spawnCallbackObserver
  ) where

import GHC.Stack
import Data.Dynamic
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Eff
import Control.Eff.Concurrent.MessagePassing
import Control.Eff.Concurrent.GenServer
import Control.Eff.Concurrent.Dispatcher
import Control.Eff.Log
import Control.Eff.State.Lazy
import Control.Lens
import Control.Monad

-- * Observation API

-- | An 'Api' index that support observation of the
-- another 'Api' that is 'Observable'.
class (Typeable p, Observable o) => Observer p o where
  -- | Wrap the 'Observation' and the 'ProcessId' (i.e. the 'Server')
  -- that caused the observation into a 'Api' value that the
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
notifyObserver :: ( Member Process r
                 , Member MessagePassing r
                 , Observable o
                 , Observer p o
                 , HasCallStack
                 )
               => Server p -> Server o -> Observation o -> Eff r ()
notifyObserver observer observed observation =
  cast_ observer (observationMessage observed observation)

-- | Send the 'registerObserverMessage'
registerObserver :: ( Member Process r
                 , Member MessagePassing r
                 , Observable o
                 , Observer p o
                 , HasCallStack
                 )
               => Server p -> Server o -> Eff r ()
registerObserver observer observed =
  cast_ observed (registerObserverMessage (SomeObserver observer))

-- | Send the 'forgetObserverMessage'
forgetObserver :: ( Member Process r
                , Member MessagePassing r
                , Observable o
                , Observer p o)
              => Server p -> Server o -> Eff r ()
forgetObserver observer observed =
  cast_ observed (forgetObserverMessage (SomeObserver observer))

-- ** Generalized observation

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
notifySomeObserver :: ( Member Process r
                     , Member MessagePassing r
                     , Observable o
                     , HasCallStack
                     )
                   => Server o
                   -> Observation o
                   -> SomeObserver o
                   -> Eff r ()
notifySomeObserver observed observation (SomeObserver observer) =
  notifyObserver observer observed observation

-- ** Manage 'Observers's

-- | Internal state for 'manageobservers'
data Observers o =
  Observers { _observers :: Set (SomeObserver o) }

observers :: Iso' (Observers o) (Set (SomeObserver o))
observers = iso _observers Observers

-- | Keep track of registered 'Observer's Observers can be added and removed,
-- and an 'Observation' can be sent to all registerd observers at once.
manageObservers :: Eff (State (Observers o) ': r) a -> Eff r a
manageObservers = flip evalState (Observers Set.empty)

-- | Add an 'Observer' to the 'Observers' managed by 'manageObservers'.
addObserver
  :: ( Member MessagePassing r
    , Member (State (Observers o)) r
    , Observable o)
  => SomeObserver o -> Eff r ()
addObserver = modify . over observers . Set.insert

-- | Delete an 'Observer' from the 'Observers' managed by 'manageObservers'.
removeObserver
  ::  ( Member MessagePassing r
    , Member (State (Observers o)) r
    , Observable o)
  => SomeObserver o -> Eff r ()
removeObserver = modify . over observers . Set.delete

-- | Send an 'Observation' to all 'SomeObserver's in the 'Observers' state.
notifyObservers
  :: forall o r . ( Observable o
            , Member MessagePassing r
            , Member Process r
            , Member (State (Observers o)) r)
  => Observation o -> Eff r ()
notifyObservers observation = do
  me <- asServer @o <$> self
  os <- view observers <$> get
  mapM_ (notifySomeObserver me observation) os

-- * Callback 'Observer'

-- | An 'Observer' that dispatches the observations to an effectful callback.
data CallbackObserver o
  deriving Typeable

data instance Api (CallbackObserver o) r where
  CbObserved :: (Typeable o, Typeable (Observation o)) =>
             Server o -> Observation o -> Api (CallbackObserver o) 'Asynchronous
  deriving Typeable

deriving instance Show (Observation o) => Show (Api (CallbackObserver o) r)

instance (Observable o) => Observer (CallbackObserver o) o where
  observationMessage = CbObserved

-- | Start a new process for an 'Observer' that dispatches
-- all observations to an effectful callback.
spawnCallbackObserver
  :: forall o r . (HasDispatcherIO r, Typeable o, Show (Observation o), Observable o)
  => (Server o -> Observation o -> Eff ProcIO Bool)
  -> Eff r (Server (CallbackObserver o))
spawnCallbackObserver onObserve =
  asServer @(CallbackObserver o)
  <$>
  (spawn $ do
      trapExit True
      me <- asServer @(CallbackObserver o) <$> self
      let loopUntil =
            serve_ (ApiHandler @(CallbackObserver o)
                     (handleCast loopUntil)
                     unhandledCallError
                     (logMsg . ((show me ++ " observer terminating ") ++)))
      loopUntil
  )
 where
   handleCast :: Eff ProcIO () -> Api (CallbackObserver o) 'Asynchronous -> Eff ProcIO ()
   handleCast k (CbObserved fromSvr v) = onObserve fromSvr v >>= flip when k
