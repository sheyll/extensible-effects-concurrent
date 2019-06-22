{-# LANGUAGE UndecidableInstances #-}
-- | Another complete example for the library
module Main where

import           Data.Dynamic
import           Control.Eff
import           Control.Eff.Concurrent.SingleThreaded
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

instance Typeable x => HasPdu Counter x where
  data instance Pdu Counter x where
    Inc :: Pdu Counter 'Asynchronous
    Cnt :: Pdu Counter ('Synchronous Integer)
    deriving Typeable

instance NFData (Pdu Counter x) where
  rnf Inc = ()
  rnf Cnt = ()

counterExample
  :: Eff Effects ()
counterExample = do
  c <- spawnCounter
  let cp = _fromEndpoint c
  lift (threadDelay 500000)
  o <- logCounterObservations
  lift (threadDelay 500000)
  registerObserver @CounterChanged c o
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

instance Typeable r => HasPdu SupiDupi r where
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

instance (LogIo q) => Server SupiCounter (Processes q) where

  type instance Model SupiCounter =
    ( Integer
    , ObserverRegistry CounterChanged
    , Maybe (ReplyTarget SupiCounter (Maybe ()))
    )

  data instance StartArgument SupiCounter (Processes q) = MkEmptySupiCounter

  setup _ _ = return ((0, emptyObserverRegistry, Nothing), ())

  update _ _ = \case
    OnCall rt callReq ->
      case callReq of
        ToPdu1 Cnt ->
          sendReply rt =<< useModel @SupiCounter _1
        ToPdu2 _ -> error "unreachable"
        ToPdu3 (Whoopediedoo c) ->
          modifyModel @SupiCounter (_3 .~ if c then Just rt else Nothing)

    OnCast castReq ->
      case castReq of
        ToPdu1 Inc -> do
          val' <- view _1 <$> modifyAndGetModel @SupiCounter (_1 %~ (+ 1))
          zoomModel @SupiCounter _2 (observerRegistryNotify (CounterChanged val'))
          when (val' > 5) $
            getAndModifyModel @SupiCounter (_3 .~ Nothing)
            >>= traverse_ (\rt' -> sendReply rt' (Just ())) . view _3
        ToPdu2 x ->
          zoomModel @SupiCounter _2 (observerRegistryHandlePdu x)
        ToPdu3 _ -> error "unreachable"

    OnDown pd -> do
      wasRemoved <- zoomModel @SupiCounter _2 (observerRegistryRemoveProcess @CounterChanged (downProcess pd))
      if wasRemoved
        then logDebug ("removed: "    <> T.pack (show pd))
        else logError ("unexpected: " <> T.pack (show pd))

    other -> logWarning (T.pack (show other))

spawnCounter :: (LogIo q) => Eff (Processes q) ( Endpoint SupiCounter )
spawnCounter = start MkEmptySupiCounter


deriving instance Show (Pdu Counter x)

logCounterObservations
  :: (LogIo q, Typeable q)
  => Eff (Processes q) (Endpoint (Observer CounterChanged))
logCounterObservations = start OCCStart

instance Member Logs q => Server (Observer CounterChanged) (Processes q) where
  data StartArgument (Observer CounterChanged) (Processes q) = OCCStart
  update _ _ e =
    case e of
      OnCast (Observed msg) -> logInfo' ("observerRegistryNotify: " ++ show msg)
      _ -> logNotice (T.pack (show e))
