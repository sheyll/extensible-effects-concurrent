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
module Control.Eff.Concurrent.Examples2 where

import Data.Dynamic
import Control.Eff
import Control.Eff.Concurrent.Dispatcher
import Control.Eff.Concurrent.Api
import Control.Eff.Concurrent.Api.Server
import Control.Eff.Concurrent.Api.Client
import Control.Eff.Concurrent.MessagePassing
import Control.Eff.Concurrent.Observer
import Control.Eff.Log
import Control.Eff.State.Lazy
import Control.Monad

data Counter deriving Typeable

data instance Api Counter x where
  Inc :: Api Counter 'Asynchronous
  Cnt :: Api Counter ('Synchronous Integer)
  ObserveCounter :: SomeObserver Counter -> Api Counter 'Asynchronous
  UnobserveCounter :: SomeObserver Counter -> Api Counter 'Asynchronous

deriving instance Show (Api Counter x)

instance Observable Counter where
  data Observation Counter where
    CountChanged :: Integer -> Observation Counter
    deriving (Show, Typeable)
  registerObserverMessage os = ObserveCounter os
  forgetObserverMessage os = UnobserveCounter os

logCounterObservations :: Eff ProcIO (Server (CallbackObserver Counter))
logCounterObservations =
  spawnCallbackObserver usingIoDispatcher
  (\fromSvr msg ->
     do me <- self usingIoDispatcher
        logMsg (show me ++ " observed on: " ++ show fromSvr ++ ": " ++ show msg)
        return True)

type CounterEff = State (Observers Counter) ': State Integer ': ProcIO

data ServerState st a where
  ServerState :: State st a -> ServerState st a

counterServerLoop :: Eff ProcIO ()
counterServerLoop = do
  evalState (manageObservers
             $ forever
             $ serve usingIoDispatcher
             $ ApiHandler @Counter handleCast handleCall (error . show)) 0
 where
   handleCast :: Api Counter 'Asynchronous -> Eff CounterEff ()
   handleCast (ObserveCounter o) = do
     addObserver o
   handleCast (UnobserveCounter o) = do
     removeObserver o
   handleCast Inc = do
     logMsg "Inc"
     modify (+ (1 :: Integer))
     currentCount <- get
     notifyObservers usingIoDispatcher (CountChanged currentCount)
   handleCall :: Api Counter ('Synchronous x) -> (x -> Eff CounterEff Bool) -> Eff CounterEff ()
   handleCall Cnt reply = do
     c <- get
     logMsg ("Cnt is " ++ show c)
     _ <- reply c
     return ()

-- ** Counter client

counterExample :: Eff ProcIO ()
counterExample = do
  let cnt sv = do r <- call px sv Cnt
                  logMsg (show sv ++ " " ++ show r)
      px = usingIoDispatcher
  server1 <- asServer @Counter <$> spawn counterServerLoop
  server2 <- asServer @Counter <$> spawn counterServerLoop
  cast px server1 Inc
  cnt server1
  cnt server2
  co1 <- logCounterObservations
  co2 <- logCounterObservations
  registerObserver px co1 server1
  registerObserver px co2 server2
  cast px server1 Inc
  cnt server1
  cast px server2 Inc
  cnt server2
  registerObserver px co2 server1
  registerObserver px co1 server2
  cast px server1 Inc
  cnt server1
  cast px server2 Inc
  cnt server2
  forgetObserver px co2 server1
  cast px server1 Inc
  cnt server1
  cast px server2 Inc
  cnt server2
