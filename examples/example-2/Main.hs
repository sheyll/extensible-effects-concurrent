{-# LANGUAGE UndecidableInstances #-}
-- | Another complete example for the library
module Main where

import           Data.Dynamic
import           Control.Eff
import           Control.Eff.Concurrent
import           Control.Eff.Concurrent.Protocol.StatefulServer
import           Control.Monad
import           Data.Foldable
import           Control.Lens
import           Control.Concurrent
import           Control.DeepSeq
import qualified Data.Text as T
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
  :: (Typeable q, LogsTo IO q, Lifted IO q)
  => Eff (InterruptableProcess q) ()
counterExample = do
  c <- spawnCounter
  let cp = _fromEndpoint c
  lift (threadDelay 500000)
  o <- logCounterObservations
  lift (threadDelay 500000)
  registerObserver o c
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

instance Show (Pdu SupiDupi r) where
  show (Whoopediedoo True) = "woopediedooo"
  show (Whoopediedoo False) = "no woopy doopy"

instance NFData (Pdu SupiDupi r) where
  rnf (Whoopediedoo b) = rnf b

newtype CounterChanged = CounterChanged Integer
  deriving (Show, Typeable, NFData)

type instance ToPretty CounterChanged = PutStr "counter changed"

type SupiCounter = (Counter, ObserverRegistry CounterChanged, SupiDupi)

type instance ToPretty (Counter, ObserverRegistry CounterChanged, SupiDupi) = PutStr "supi-counter"

instance (LogIo q) => Server SupiCounter q where

  type instance Model SupiCounter =
    ( Integer
    , Observers CounterChanged
    , Maybe ( Serializer (Reply SupiCounter (Maybe ()))
            , RequestOrigin SupiCounter (Maybe ())
            )
    )

  data instance StartArgument SupiCounter q = MkEmptySupiCounter

  setup _ = return ((0, emptyObservers, Nothing), ())

  update _ = \case
    OnCall ser orig callReq ->
      case callReq of
        ToPdu1 Cnt ->
          sendReply ser orig =<< useModel @SupiCounter _1
        ToPdu2 _ -> error "unreachable"
        ToPdu3 (Whoopediedoo c) ->
          modifyModel @SupiCounter (_3 .~ if c then Just (ser, orig) else Nothing)

    OnCast castReq ->
      case castReq of
        ToPdu1 Inc -> do
          val' <- view _1 <$> modifyAndGetModel @SupiCounter (_1 %~ (+ 1))
          zoomModel @SupiCounter _2 (observed (CounterChanged val'))
          when (val' > 5) $
            getAndModifyModel @SupiCounter (_3 .~ Nothing)
            >>= traverse_ (\(ser, orig) -> sendReply ser orig (Just ())) . view _3
        ToPdu2 x ->
          zoomModel @SupiCounter _2 (handleObserverRegistration x)
        ToPdu3 _ -> error "unreachable"

    other -> logWarning (T.pack (show other))

spawnCounter :: (LogsTo IO q, Lifted IO q) => Eff (InterruptableProcess q) ( Endpoint SupiCounter )
spawnCounter = start MkEmptySupiCounter


deriving instance Show (Pdu Counter x)

logCounterObservations
  :: (LogsTo IO q, Lifted IO q, Typeable q)
  => Eff (InterruptableProcess q) (Observer CounterChanged)
logCounterObservations = do
  svr <- start OCCStart
  pure (toObserver svr)

instance Member Logs q => Server (Observer CounterChanged) q where
  data StartArgument (Observer CounterChanged) q = OCCStart
  type Model (Observer CounterChanged) = Observers CounterChanged
  type Settings (Observer CounterChanged) = ()
  type Protocol (Observer CounterChanged) = Observer CounterChanged
  setup _ = pure (emptyObservers, ())
  update _ =
    \case
      OnCast r -> handleObservations (\msg -> logInfo' ("observed: " ++ show msg)) r
      wtf -> logNotice (T.pack (show wtf))