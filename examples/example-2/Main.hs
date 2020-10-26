{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Another complete example for the library
module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent.Protocol.StatefulServer
import Control.Eff.Concurrent.SingleThreaded
import Control.Lens
import Control.Monad
import Data.Default
import Data.Dynamic
import Data.Foldable

main :: IO ()
main = defaultMain (void counterExample)

-- * First API

data Counter deriving (Typeable)

instance HasPdu Counter where
  data Pdu Counter x where
    Inc :: Pdu Counter 'Asynchronous
    Cnt :: Pdu Counter ('Synchronous Integer)
    deriving (Typeable)

instance ToTypeLogMsg Counter where
  toTypeLogMsg _ = "Counter"

instance ToLogMsg (Pdu Counter x) where
  toLogMsg Inc = packLogMsg "increment"
  toLogMsg Cnt = packLogMsg "get-count"

instance NFData (Pdu Counter x) where
  rnf Inc = ()
  rnf Cnt = ()

counterExample ::
  Eff Effects ()
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

data SupiDupi deriving (Typeable)

instance HasPdu SupiDupi where
  data Pdu SupiDupi r where
    Whoopediedoo :: Bool -> Pdu SupiDupi ('Synchronous (Maybe ()))
    deriving (Typeable)

instance ToTypeLogMsg SupiDupi where
  toTypeLogMsg _ = "SupiDupi"

instance ToProtocolName SupiDupi where
  toProtocolName = "SupiDupi"

instance ToLogMsg (Pdu SupiDupi r) where
  toLogMsg (Whoopediedoo f) =
    packLogMsg "whoopediedoo: " <> toLogMsg f

instance Show (Pdu SupiDupi r) where
  show (Whoopediedoo True) = "woopediedooo"
  show (Whoopediedoo False) = "no woopy doopy"

instance NFData (Pdu SupiDupi r) where
  rnf (Whoopediedoo b) = rnf b

newtype CounterChanged = CounterChanged Integer
  deriving (Show, Typeable, NFData, ToLogMsg)

instance ToProtocolName CounterChanged where
  toProtocolName = "CounterChanged"

type SupiCounter = (Counter, ObserverRegistry CounterChanged, SupiDupi)

instance (IoLogging q) => Server SupiCounter (Processes q) where
  newtype Model SupiCounter
    = SupiCounterModel
        ( Integer,
          ObserverRegistry CounterChanged,
          Maybe (ReplyTarget SupiCounter (Maybe ()))
        )

  data StartArgument SupiCounter = MkEmptySupiCounter

  setup _ _ = return (SupiCounterModel (0, emptyObserverRegistry, Nothing), ())

  update _ _ = \case
    OnCall rt callReq ->
      case callReq of
        ToPdu1 Cnt ->
          sendReply rt =<< useModel supiCounter
        ToPdu2 _ -> error "unreachable"
        ToPdu3 (Whoopediedoo c) ->
          modifyModel @SupiCounter (supiTarget .~ if c then Just rt else Nothing)
    OnCast castReq ->
      case castReq of
        ToPdu1 Inc -> do
          val' <- view supiCounter <$> modifyAndGetModel (supiCounter %~ (+ 1))
          zoomModel supiRegistry (observerRegistryNotify (CounterChanged val'))
          when (val' > 5) $
            getAndModifyModel (supiTarget .~ Nothing)
              >>= traverse_ (\rt' -> sendReply rt' (Just ())) . view supiTarget
        ToPdu2 x ->
          zoomModel supiRegistry (observerRegistryHandlePdu x)
        ToPdu3 _ -> error "unreachable"
    OnDown pd -> do
      wasRemoved <- zoomModel supiRegistry (observerRegistryRemoveProcess @CounterChanged (downProcess pd))
      if wasRemoved
        then logDebug (LABEL "removed" pd)
        else logError (LABEL "unexpected" pd)
    other -> logWarning other

instance ToLogMsg (StartArgument SupiCounter) where
  toLogMsg _ = packLogMsg "start arg: supi counter"

supiCounter :: Lens' (Model SupiCounter) Integer
supiCounter =
  lens
    (\(SupiCounterModel (x, _, _)) -> x)
    (\(SupiCounterModel (_, y, z)) x -> SupiCounterModel (x, y, z))

supiRegistry :: Lens' (Model SupiCounter) (ObserverRegistry CounterChanged)
supiRegistry =
  lens
    (\(SupiCounterModel (_, y, _)) -> y)
    (\(SupiCounterModel (x, _, z)) y -> SupiCounterModel (x, y, z))

supiTarget :: Lens' (Model SupiCounter) (Maybe (ReplyTarget SupiCounter (Maybe ())))
supiTarget =
  lens
    (\(SupiCounterModel (_, _, z)) -> z)
    (\(SupiCounterModel (x, y, _)) z -> SupiCounterModel (x, y, z))

spawnCounter :: (IoLogging q) => Eff (Processes q) (Endpoint SupiCounter)
spawnCounter = startLink MkEmptySupiCounter

deriving instance Show (Pdu Counter x)

logCounterObservations ::
  IoLogging q => Eff (Processes q) (Endpoint (Observer CounterChanged))
logCounterObservations = startLink OCCStart

instance Member Logs q => Server (Observer CounterChanged) (Processes q) where
  data StartArgument (Observer CounterChanged) = OCCStart
  newtype Model (Observer CounterChanged) = CounterChangedModel () deriving (Default)
  update _ _ e =
    case e of
      OnCast (Observed msg) -> logInfo (LABEL "observerRegistryNotify" msg)
      _ -> logNotice e

instance ToLogMsg (StartArgument (Observer CounterChanged)) where
  toLogMsg _ = packLogMsg "start-arg for the CounterChanged observer"
