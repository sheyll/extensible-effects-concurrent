-- | Another complete example for the library
module Main where

import           Data.Dynamic
import           Control.Eff
import           Control.Eff.Concurrent
import           Control.Eff.State.Strict
import           Control.Monad
import           Data.Foldable
import           Control.Concurrent
import           Control.DeepSeq
import           Data.Type.Pretty

main :: IO ()
main = defaultMain (void counterExample)

-- * First API

data Counter deriving Typeable

type instance ToPretty Counter = PutStr "counter"

data instance Pdu Counter x where
  Inc :: Pdu Counter 'Asynchronous
  Cnt :: Pdu Counter ('Synchronous Integer)
  deriving Typeable

instance NFData (Pdu Counter x) where
  rnf Inc = ()
  rnf Cnt = ()

counterExample
  :: (Member Logs q, Lifted IO q)
  => Eff (InterruptableProcess q) ()
counterExample = do
  (c, (co, (_sdp, cp))) <- spawnCounter
  lift (threadDelay 500000)
  o <- logCounterObservations
  lift (threadDelay 500000)
  registerObserver o co
  lift (threadDelay 500000)
  cast c Inc
  lift (threadDelay 500000)
  sendMessage cp ("test 123" :: String)
  cast c Inc
  lift (threadDelay 500000)
  cast c Inc
  sendMessage cp (12312312 :: Int)
  lift (threadDelay 500000)
  cast c Inc
  lift (threadDelay 500000)
  cast c Inc
  lift (threadDelay 500000)
  cast c Inc
  lift (threadDelay 500000)
  r <- call c Cnt
  lift (threadDelay 500000)
  lift (putStrLn ("r: " ++ show r))
  lift (threadDelay 500000)
  lift (threadDelay 500000)

data SupiDupi deriving Typeable

type instance ToPretty SupiDupi = PutStr "supi dupi"

data instance Pdu SupiDupi r where
  Whoopediedoo :: Bool -> Pdu SupiDupi ('Synchronous (Maybe ()))
  deriving Typeable

instance NFData (Pdu SupiDupi r) where
  rnf (Whoopediedoo b) = rnf b

newtype CounterChanged = CounterChanged Integer
  deriving (Show, Typeable, NFData)

type instance ToPretty CounterChanged = PutStr "counter changed"

spawnCounter
  :: (Member Logs q)
  => Eff
       (InterruptableProcess q)
       ( Endpoint Counter
       , ( Endpoint (ObserverRegistry CounterChanged)
         , (Endpoint SupiDupi, ProcessId)
         )
       )
spawnCounter = spawnProtocolServerEffectful
  (manageObservers @CounterChanged . evalState (0 :: Integer) . evalState
    (Nothing :: Maybe (RequestOrigin (Pdu SupiDupi ( 'Synchronous (Maybe ())))))
  )
  (  handleCalls
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
           observed (CounterChanged val')
           put val'
           when (val' > 5) $ do
             get
               @( Maybe
                   (RequestOrigin (Pdu SupiDupi ( 'Synchronous (Maybe ()))))
               )
               >>= traverse_ (flip sendReply (Just ()))
             put
               (Nothing :: Maybe
                   (RequestOrigin (Pdu SupiDupi ( 'Synchronous (Maybe ()))))
               )

           return AwaitNext
       )
  ^: handleObserverRegistration
  ^: handleCallsDeferred

       (\origin ->
         (\case
           Whoopediedoo c -> do
             if c
               then put (Just origin)
               else
                 put
                   (Nothing :: Maybe
                       ( RequestOrigin
                           (Pdu SupiDupi ( 'Synchronous (Maybe ())))
                       )
                   )
             pure AwaitNext
         )
       )
  ^: logUnhandledMessages
  )
  stopServerOnInterrupt

deriving instance Show (Pdu Counter x)

logCounterObservations
  :: (Member Logs q)
  => Eff (InterruptableProcess q) (Observer CounterChanged)
logCounterObservations = do
  svr <- spawnProtocolServer
    (handleObservations
      (\msg -> do
        logInfo' ("observed: " ++ show msg)
        return AwaitNext
      )
    )
    stopServerOnInterrupt
  pure (toObserver svr)
