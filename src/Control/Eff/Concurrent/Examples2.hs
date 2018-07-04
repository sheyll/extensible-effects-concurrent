-- | Another complete example for the library
module Control.Eff.Concurrent.Examples2 where

import Data.Dynamic
import Control.Eff
import qualified Control.Eff.Concurrent.Process.ForkIOScheduler as ForkIO
import qualified Control.Eff.Concurrent.Process.SingleThreadedScheduler as Pure
import Control.Eff.Concurrent
import Control.Eff.State.Strict
import Control.Monad

main :: IO ()
main =
  do ForkIO.defaultMain (counterExample SchedulerProxy)
     Pure.defaultMain (counterExample SchedulerProxy)

-- * First API

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

logCounterObservations
  :: (SetMember Process (Process q) r, Member (Logs String) q)
    => SchedulerProxy q -> Eff r (Server (CallbackObserver Counter))
logCounterObservations px =
  spawnCallbackObserver px
  (\fromSvr msg ->
     do me <- self px
        logMsg (show me
                 ++ " observed on: "
                 ++ show fromSvr
                 ++ ": "
                 ++ show msg)
        return True)

counterHandler :: forall r q .
                 ( Member (State (Observers Counter)) r
                 , Member (State Integer) r
                 , Member (Logs String) r
                 , SetMember Process (Process q) r)
               => SchedulerProxy q
               -> ApiHandler Counter r
counterHandler px =
  ApiHandler @Counter handleCast handleCall (defaultTermination px)
 where
   handleCast :: Api Counter 'Asynchronous -> Eff r ()
   handleCast (ObserveCounter o) = do
     addObserver o
   handleCast (UnobserveCounter o) = do
     removeObserver o
   handleCast Inc = do
     logMsg "Inc"
     modify (+ (1 :: Integer))
     currentCount <- get
     notifyObservers px (CountChanged currentCount)
   handleCall :: Api Counter ('Synchronous x) -> (x -> Eff r ()) -> Eff r ()
   handleCall Cnt reply = do
     c <- get
     logMsg ("Cnt is " ++ show c)
     reply c

-- * Second API

data PocketCalc deriving Typeable

data instance Api PocketCalc x where
  Add :: Integer -> Api PocketCalc ('Synchronous Integer)
  AAdd :: Integer -> Api PocketCalc 'Asynchronous

deriving instance Show (Api PocketCalc x)

pocketCalcHandler :: forall r q .
                 ( Member (State Integer) r
                 , Member (Logs String) r
                 , SetMember Process (Process q) r)
               => SchedulerProxy q
               -> ApiHandler PocketCalc r
pocketCalcHandler px =
  ApiHandler @PocketCalc handleCast handleCall (defaultTermination px)
 where
   handleCast :: Api PocketCalc 'Asynchronous -> Eff r ()
   handleCast (AAdd x) = do
     logMsg ("AsyncAdd " ++ show x)
     modify (+ x)
     c <- get @Integer
     logMsg ("Accumulator is " ++ show c)
   handleCall :: Api PocketCalc ('Synchronous x) -> (x -> Eff r ()) -> Eff r ()
   handleCall (Add x) reply = do
     logMsg ("Add " ++ show x)
     modify (+ x)
     c <- get
     logMsg ("Accumulator is " ++ show c)
     reply c

serverLoop :: forall r q .
                 ( Member (Logs String) r
                 , SetMember Process (Process q) r)
               => SchedulerProxy q
               -> Eff r ()
serverLoop px = do
  evalState @Integer
    0
    (manageObservers @Counter
     (serveBoth px
       (counterHandler px)
       (pocketCalcHandler px)))


-- ** Counter client
counterExample :: ( Member (Logs String) r
                 , Member (Logs String) q
                 , SetMember Process (Process q) r)
               => SchedulerProxy q
               -> Eff r ()
counterExample px = do
  let cnt sv = do r <- call px sv Cnt
                  logMsg (show sv ++ " " ++ show r)
  pid1 <- spawn (serverLoop px)
  pid2 <- spawn (serverLoop px)
  let cntServer1 = asServer @Counter pid1
      cntServer2 = asServer @Counter pid2
      calcServer1 = asServer @PocketCalc pid1
      calcServer2 = asServer @PocketCalc pid2
  cast px cntServer1 Inc
  cnt cntServer1
  cnt cntServer2
  co1 <- logCounterObservations px
  co2 <- logCounterObservations px
  registerObserver px co1 cntServer1
  registerObserver px co2 cntServer2
  cast px calcServer1 (AAdd 12313)
  cast px cntServer1 Inc
  cnt cntServer1
  cast px cntServer2 Inc
  cnt cntServer2
  registerObserver px co2 cntServer1
  registerObserver px co1 cntServer2
  cast px cntServer1 Inc
  void (call px calcServer2 (Add 878))
  cnt cntServer1
  cast px cntServer2 Inc
  cnt cntServer2
  forgetObserver px co2 cntServer1
  cast px cntServer1 Inc
  cnt cntServer1
  cast px cntServer2 Inc
  cnt cntServer2
  void $ sendShutdown px pid2
