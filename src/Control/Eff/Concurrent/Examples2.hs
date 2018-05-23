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

import GHC.Stack
import Control.Eff
import Control.Eff.Lift
import Control.Monad
import Data.Dynamic
import Control.Eff.Concurrent.MessagePassing
import Control.Eff.Concurrent.GenServer
import Control.Eff.Concurrent.Dispatcher
import Control.Eff.Log
import Control.Eff.State.Lazy
import Control.Eff.Interactive

data Counter deriving Typeable

data instance Api Counter x where
  Inc :: Api Counter 'Asynchronous
  Cnt :: Api Counter ('Synchronous Integer)

deriving instance Show (Api Counter x)


runInSchedulerWithIO :: Eff ProcIO a -> IO a
runInSchedulerWithIO c =
  runLoggingT
    (logChannelBracket
      (Just "hello")
      (Just "KTHXBY")
      (runMainProcess c))
    (print :: String -> IO ())

counterExample :: Eff ProcIO (Server Counter)
counterExample = do
  server <- asServer @Counter <$> spawn counterServerLoop
  c0 <- call server Cnt
  logMsg (show c0)
  cast_ server Inc
  c1 <- call server Cnt
  logMsg (show c1)
  return server

type CounterEff = State Integer ': ProcIO

data ServerState st a where
  ServerState :: State st a -> ServerState st a

counterServerLoop :: Eff ProcIO ()
counterServerLoop = do
  void (trapExit True)
  evalState (forever $ serve_ $ ApiHandler @Counter handleCast handleCall error) 0
 where
   handleCast :: Api Counter 'Asynchronous -> Eff CounterEff ()
   handleCast Inc = do
     logMsg "Inc"
     modify (+ (1 :: Integer))
   handleCall :: Api Counter ('Synchronous x) -> (x -> Eff CounterEff Bool) -> Eff CounterEff ()
   handleCall Cnt reply = do
     c <- get
     logMsg ("Cnt is " ++ show c)
     _ <- reply c
     return ()
