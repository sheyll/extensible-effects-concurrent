{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Another  example for the library that uses embedded protocols with multiple server back
-- ends and a polymorphic client, as well as the 'Broker' module to start multiple
-- back-ends.
--
-- @since 0.29.0
module Main where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent
import Control.Eff.Concurrent.Protocol.Broker as Broker
import Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import Control.Eff.State.Lazy as State
import Control.Lens
import Control.Monad
import Data.Default
import Data.Dynamic
import Data.Foldable
import Data.Functor.Contravariant (contramap)
import GHC.Stack (HasCallStack)

main :: IO ()
main =
  defaultMain (void embeddedExample)

embeddedExample :: HasCallStack => Eff Effects ()
embeddedExample = do
  b1 <- Stateful.startLink InitBackend1
  b2Broker <- startBackend2Broker
  b2 <- Broker.spawnOrLookup @Backend2 b2Broker 1
  _ <- Broker.spawnOrLookup b2Broker 2
  b2_2 <- Broker.spawnOrLookup b2Broker 2
  b2_3 <- Broker.spawnOrLookup b2Broker 3
  app <- Stateful.startLink InitApp
  cast app DoThis
  cast app DoThis
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b1)))
  cast app DoThis
  cast app DoThis
  spawn_ "sub-process" $ do
    logNotice (MSG "spawned sub process")
    b1_2 <- Stateful.startLink InitBackend1
    call app (SetBackend (Just (SomeBackend b1_2)))
    cast app DoThis
    cast app DoThis
    exitWithError "test-error"
  call app (SetBackend Nothing)
  cast app DoThis
  cast app DoThis
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b1)))
  cast app DoThis
  cast app DoThis
  call app (SetBackend Nothing)
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b2)))
  cast app DoThis
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b2_2)))
  cast app DoThis
  _ <- Broker.stopChild b2Broker 2
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b2_3)))
  cast app DoThis
  Broker.stopBroker b2Broker
  cast app DoThis

------------------------------ Server Instances

-- Application layer

-- Application layer

data App deriving (Typeable)

instance ToTypeLogMsg App

instance HasPdu App where
  type EmbeddedPduList App = '[Observer BackendEvent]
  data Pdu App r where
    SetBackend :: Maybe SomeBackend -> Pdu App ('Synchronous ())
    DoThis :: Pdu App 'Asynchronous
    AppBackendEvent :: Pdu (Observer BackendEvent) r -> Pdu App r
    deriving (Typeable)

instance NFData (Pdu App r) where
  rnf (SetBackend !_x) = ()
  rnf DoThis = ()
  rnf (AppBackendEvent e) = rnf e

instance ToLogMsg (Pdu App r) where
  toLogMsg (SetBackend _x) = "setting backend"
  toLogMsg DoThis = "doing this"
  toLogMsg (AppBackendEvent e) = "got backend event: " <> toLogMsg e

instance HasPduPrism App (Observer BackendEvent) where
  embedPdu = AppBackendEvent
  fromPdu (AppBackendEvent e) = Just e
  fromPdu _ = Nothing

instance Stateful.Server App Effects where
  newtype Model App = MkApp (Maybe SomeBackend) deriving (Default)
  data StartArgument App = InitApp
  update me _x e =
    case e of
      OnCall rt (SetBackend b) -> do
        logInfo (packLogMsg "setting backend")
        MkApp oldB <- getAndPutModel @App (MkApp b)
        traverse_ (`backendForgetObserver` me) oldB
        traverse_ (`backendRegisterObserver` me) b
        sendReply rt ()
      OnCast (AppBackendEvent be) ->
        logInfo (packLogMsg "got backend event: ") be
      OnCast DoThis ->
        do
          MkApp m <- getModel @App
          case m of
            Nothing -> logInfo (packLogMsg "doing this without backend")
            Just b -> handleInterrupts logWarning $ do
              doSomeBackendWork b
              bi <- getSomeBackendInfo b
              logInfo (packLogMsg "doing this. Backend: ") bi
      _ -> logWarning (packLogMsg "unexpected: ") e

instance ToLogMsg (StartArgument App) where
  toLogMsg _ = packLogMsg "start app"

------------------------------ Protocol Data Types

-- Backend
data Backend deriving (Typeable)

instance ToTypeLogMsg Backend

instance HasPdu Backend where
  data Pdu Backend r where
    BackendWork :: Pdu Backend 'Asynchronous
    GetBackendInfo :: Pdu Backend ('Synchronous String)
    deriving (Typeable)

instance NFData (Pdu Backend r) where
  rnf BackendWork = ()
  rnf GetBackendInfo = ()

instance ToLogMsg (Pdu Backend r)

instance Show (Pdu Backend r) where
  show BackendWork = "BackendWork"
  show GetBackendInfo = "GetBackendInfo"

newtype BackendEvent where
  BackendEvent :: String -> BackendEvent
  deriving (NFData, Show, Typeable, ToLogMsg)

instance ToTypeLogMsg BackendEvent

type IsBackend b =
  ( HasPdu b,
    Embeds b Backend,
    ToTypeLogMsg b,
    IsObservable b BackendEvent,
    Tangible (Pdu b ('Synchronous String)),
    ToLogMsg (Pdu b ('Synchronous String)),
    Tangible (Pdu b 'Asynchronous),
    ToLogMsg (Pdu b 'Asynchronous)
  )

data SomeBackend
  = forall b. IsBackend b => SomeBackend (Endpoint b)

withSomeBackend ::
  SomeBackend ->
  (forall b. IsBackend b => Endpoint b -> x) ->
  x
withSomeBackend (SomeBackend x) f = f x

backendRegisterObserver ::
  ( HasProcesses e q,
    CanObserve m BackendEvent,
    Embeds m (Observer BackendEvent),
    Tangible (Pdu m 'Asynchronous),
    ToLogMsg (Pdu m 'Asynchronous),
    ToTypeLogMsg m
  ) =>
  SomeBackend ->
  Endpoint m ->
  Eff e ()
backendRegisterObserver (SomeBackend x) o = registerObserver @BackendEvent x o

backendForgetObserver ::
  ( HasProcesses e q,
    CanObserve m BackendEvent,
    Embeds m (Observer BackendEvent),
    Tangible (Pdu m 'Asynchronous),
    ToLogMsg (Pdu m 'Asynchronous),
    ToTypeLogMsg m
  ) =>
  SomeBackend ->
  Endpoint m ->
  Eff e ()
backendForgetObserver (SomeBackend x) o = forgetObserver @BackendEvent x o

getSomeBackendInfo :: HasProcesses e q => SomeBackend -> Eff e String
getSomeBackendInfo (SomeBackend x) = call x GetBackendInfo

doSomeBackendWork :: HasProcesses e q => SomeBackend -> Eff e ()
doSomeBackendWork (SomeBackend x) = cast x BackendWork

-------------------------

-- Backend 1

data Backend1 deriving (Typeable)

instance ToTypeLogMsg Backend1

instance Stateful.Server Backend1 Effects where
  type Protocol Backend1 = (Backend, ObserverRegistry BackendEvent)
  newtype Model Backend1 = MkBackend1 (Int, ObserverRegistry BackendEvent)
  data StartArgument Backend1 = InitBackend1
  setup _ _ = pure (MkBackend1 (0, emptyObserverRegistry), ())
  update me _ e = do
    model <- getModel @Backend1
    case e of
      OnCall rt (ToPduLeft GetBackendInfo) ->
        sendReply
          (toEmbeddedReplyTarget @(Stateful.Protocol Backend1) @Backend rt)
          ("Backend1 " <> show me <> " " <> show (model ^. modelBackend1 . _1))
      OnCast (ToPduLeft BackendWork) -> do
        logInfo (MSG "working...")
        modifyModel @Backend1 (over (modelBackend1 . _1) (+ 1))
      OnCast (ToPduRight x) -> do
        logInfo (MSG "event registration stuff ...")
        zoomModel @Backend1 (modelBackend1 . _2) (observerRegistryHandlePdu x)
      OnDown pd -> do
        logWarning pd
        wasObserver <- zoomModel @Backend1 (modelBackend1 . _2) (observerRegistryRemoveProcess @BackendEvent (downProcess pd))
        when wasObserver $
          logNotice (MSG "observer removed")
      _ -> logWarning (MSG "unexpected: ") e

modelBackend1 :: Iso' (Model Backend1) (Int, ObserverRegistry BackendEvent)
modelBackend1 = iso (\(MkBackend1 x) -> x) MkBackend1

instance ToLogMsg (StartArgument Backend1) where
  toLogMsg InitBackend1 = packLogMsg "InitBackend1"

-- Backend 2 is behind a broker

data Backend2 deriving (Typeable)

instance ToTypeLogMsg Backend2

instance HasPdu Backend2 where
  type EmbeddedPduList Backend2 = '[Backend, ObserverRegistry BackendEvent]
  data Pdu Backend2 r where
    B2ObserverRegistry :: Pdu (ObserverRegistry BackendEvent) r -> Pdu Backend2 r
    B2BackendWork :: Pdu Backend r -> Pdu Backend2 r
    deriving (Typeable)

instance NFData (Pdu Backend2 r) where
  rnf (B2BackendWork w) = rnf w
  rnf (B2ObserverRegistry x) = rnf x

instance ToLogMsg (Pdu Backend2 r) where
  toLogMsg (B2BackendWork w) = toLogMsg w
  toLogMsg (B2ObserverRegistry x) = toLogMsg x

instance HasPduPrism Backend2 Backend where
  embedPdu = B2BackendWork
  fromPdu (B2BackendWork x) = Just x
  fromPdu _ = Nothing

instance HasPduPrism Backend2 (ObserverRegistry BackendEvent) where
  embedPdu = B2ObserverRegistry
  fromPdu (B2ObserverRegistry x) = Just x
  fromPdu _ = Nothing

instance Effectful.Server Backend2 Effects where
  type ServerEffects Backend2 Effects = State Int ': ObserverRegistryState BackendEvent ': Effects
  data Init Backend2 = InitBackend2 Int
  runEffects _me _ e = evalObserverRegistryState (evalState 0 e)
  onEvent me _ e = do
    myIndex <- get @Int
    case e of
      OnCall rt (B2BackendWork GetBackendInfo) ->
        sendReply rt ("Backend2 " <> show me <> " " <> show myIndex)
      OnCast (B2BackendWork BackendWork) -> do
        logInfo (MSG "working...")
        put @Int (myIndex + 1)
        when
          (myIndex `mod` 2 == 0)
          (observerRegistryNotify (BackendEvent "even!"))
      OnCast (B2ObserverRegistry x) -> do
        logInfo (MSG "event registration stuff ...")
        observerRegistryHandlePdu @BackendEvent x
      OnInterrupt NormalExitRequested
        | even myIndex -> do
          logNotice (MSG "Kindly exitting -_-")
          exitNormally
        | otherwise ->
          logNotice (MSG "Ignoring exit request! :P")
      OnDown pd -> do
        logWarning pd
        wasObserver <- observerRegistryRemoveProcess @BackendEvent (downProcess pd)
        when wasObserver $
          logNotice (packLogMsg "observer removed")
      _ -> logWarning (packLogMsg "unexpected: ") e

type instance Broker.ChildId Backend2 = Int

instance ToLogMsg (Init Backend2) where
  toLogMsg (InitBackend2 x) = packLogMsg "backend2_" <> toLogMsg x

startBackend2Broker :: Eff Effects (Endpoint (Broker.Broker Backend2))
startBackend2Broker = Broker.startLink (Broker.MkBrokerConfig (TimeoutMicros 1_000_000) InitBackend2)
