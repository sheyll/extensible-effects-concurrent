-- | Another complete example for the library
module Main where

import           Data.Dynamic
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Concurrent
import           Control.Eff.State.Strict
import           Control.Monad
import           Control.Concurrent

main :: IO ()
main = do
  defaultMain (void (altCounterExample SchedulerProxy))

  defaultMain (void (counterExample SchedulerProxy))
  print (schedulePure (counterExample SchedulerProxy))

-- * First API

data Counter deriving Typeable

data instance Api Counter x where
  Inc :: Api Counter 'Asynchronous
  Cnt :: Api Counter ('Synchronous Integer)
  ObserveCounter :: SomeObserver Counter -> Api Counter 'Asynchronous
  UnobserveCounter :: SomeObserver Counter -> Api Counter 'Asynchronous
  deriving Typeable

altCounterExample
  :: (Member (Logs LogMessage) q, Lifted IO q)
  => SchedulerProxy q
  -> Eff (InterruptableProcess q) ()
altCounterExample px = do
  (c, cp) <- altCounter
  lift (threadDelay 500000)
  o <- logCounterObservations px
  lift (threadDelay 500000)
  registerObserver px o c
  lift (threadDelay 500000)
  cast px c Inc
  lift (threadDelay 500000)
  sendMessage px cp "test 123"
  cast px c Inc
  lift (threadDelay 500000)
  cast px c Inc
  sendMessage px cp (12312312 :: Int)
  lift (threadDelay 500000)
  cast px c Inc
  lift (threadDelay 500000)
  cast px c Inc
  lift (threadDelay 500000)
  cast px c Inc
  lift (threadDelay 500000)
  r <- call px c Cnt
  lift (threadDelay 500000)
  lift (putStrLn ("r: " ++ show r))
  lift (threadDelay 500000)
  lift (threadDelay 500000)

data SupiDupi deriving Typeable

data instance Api SupiDupi r where
  Whoopediedoo :: Bool -> Api SupiDupi ('Synchronous (Maybe ()))
  deriving Typeable

altCounter
  :: (Member (Logs LogMessage) q)
  => Eff (InterruptableProcess q) (Server Counter, ProcessId)
altCounter = spawnApiServerEffectful
  (manageObservers @Counter . evalState (0 :: Integer) . evalState
    (Nothing :: Maybe (RequestOrigin (Api SupiDupi ( 'Synchronous (Maybe ())))))
  )
  (  handleCalls
      SP
      (\case
        Cnt ->
          ($ do
            val <- get
            return (Just val, AwaitNext)
          )
      )
  <> handleCasts
       (\case
         Inc -> do
           val <- get @Integer
           let val' = val + 1
           notifyObservers SP (CountChanged val')
           put val'
           when (val' > 5) $ do
             get >>= traverse_ (flip sendReply (Just ()))
             put
               (Nothing :: Maybe
                   (RequestOrigin (Api SupiDupi ( 'Synchronous (Maybe ()))))
               )

           return AwaitNext
         ObserveCounter s -> do
           addObserver s
           return AwaitNext
         UnobserveCounter s -> do
           removeObserver s
           return AwaitNext
       )
  ^: handleCallsDeferred
       SP
       origin
       (\case
         Whoopediedoo c -> if c
           then put (Just origin)
           else
             put
               (Nothing :: Maybe
                   (RequestOrigin (Api SupiDupi ( 'Synchronous (Maybe ()))))
               )
       )
  ^: logUnhandledMessages
  )
  stopServerOnInterrupt

deriving instance Show (Api Counter x)

instance Observable Counter where
  data Observation Counter where
    CountChanged :: Integer -> Observation Counter
    deriving (Show, Typeable)
  registerObserverMessage = ObserveCounter
  forgetObserverMessage = UnobserveCounter

logCounterObservations
  :: ( SetMember Process (Process q) r
     , Member (Logs LogMessage) q
     , Member (Logs LogMessage) r
     , Member Interrupts r
     )
  => SchedulerProxy q
  -> Eff r (Server (CallbackObserver Counter))
logCounterObservations px = spawnCallbackObserver
  px
  (\fromSvr msg -> do
    me <- self px
    logInfo (show me ++ " observed on: " ++ show fromSvr ++ ": " ++ show msg)
    return HandleNextRequest
  )


counterHandler
  :: forall r q
   . ( Member (State (Observers Counter)) r
     , Member (State Integer) r
     , SetMember Process (Process q) r
     , Member (Logs LogMessage) q
     , Member (Logs LogMessage) r
     , Member Interrupts r
     )
  => SchedulerProxy q
  -> ApiHandler Counter r
counterHandler px = castAndCallHandler handleCastCounter handleCallCounter
 where
  handleCastCounter :: Api Counter 'Asynchronous -> Eff r ApiServerCmd
  handleCastCounter (ObserveCounter o) =
    addObserver o >> return HandleNextRequest
  handleCastCounter (UnobserveCounter o) =
    removeObserver o >> return HandleNextRequest
  handleCastCounter Inc = do
    logInfo "Inc"
    modify (+ (1 :: Integer))
    currentCount <- get
    notifyObservers px (CountChanged currentCount)
    return HandleNextRequest
  handleCallCounter
    :: Api Counter ( 'Synchronous x) -> (x -> Eff r ()) -> Eff r ApiServerCmd
  handleCallCounter Cnt reply = do
    c <- get
    logInfo ("Cnt is " ++ show c)
    reply c
    return HandleNextRequest

-- * Second API

data PocketCalc deriving Typeable

data instance Api PocketCalc x where
  Add :: Integer -> Api PocketCalc ('Synchronous Integer)
  AAdd :: Integer -> Api PocketCalc 'Asynchronous

deriving instance Show (Api PocketCalc x)

pocketCalcHandler
  :: forall r q
   . ( Member (State Integer) r
     , SetMember Process (Process q) r
     , Member (Logs LogMessage) r
     )
  => SchedulerProxy q
  -> ApiHandler PocketCalc r
pocketCalcHandler _ = castAndCallHandler handleCastCalc handleCallCalc
 where
  handleCastCalc :: Api PocketCalc 'Asynchronous -> Eff r ApiServerCmd
  handleCastCalc (AAdd x) = do
    logInfo ("AsyncAdd " ++ show x)
    modify (+ x)
    c <- get @Integer
    logInfo ("Accumulator is " ++ show c)
    return HandleNextRequest
  handleCallCalc
    :: Api PocketCalc ( 'Synchronous x) -> (x -> Eff r ()) -> Eff r ApiServerCmd
  handleCallCalc (Add x) reply = do
    logInfo ("Add " ++ show x)
    modify (+ x)
    c <- get
    logInfo ("Accumulator is " ++ show c)
    reply c
    return HandleNextRequest

serverLoop
  :: forall r q
   . ( Member (Logs LogMessage) q
     , Member (Logs LogMessage) r
     , Member Interrupts r
     , SetMember Process (Process q) r
     )
  => SchedulerProxy q
  -> Eff r ()
serverLoop px = evalState @Integer
  0
  (manageObservers @Counter (serve px (counterHandler px, pocketCalcHandler px))
  )

-- ** Counter client
counterExample
  :: ( SetMember Process (Process q) r
     , Member (Logs LogMessage) q
     , Member (Logs LogMessage) r
     , Member Interrupts r
     , q <:: r
     )
  => SchedulerProxy q
  -> Eff r Integer
counterExample px = execState (0 :: Integer) $ do
  let cnt sv = do
        r <- call px sv Cnt
        logInfo (show sv ++ " " ++ show r)
        modify (+ r)
  pid1 <- spawn (serverLoop px)
  pid2 <- spawn (serverLoop px)
  let cntServer1  = asServer @Counter pid1
      cntServer2  = asServer @Counter pid2
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
  cast px cntServer1  Inc
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
  void $ sendShutdown px pid2 (NotRecovered (ProcessError "test test test"))
