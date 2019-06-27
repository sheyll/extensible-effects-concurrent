{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NumericUnderscores #-}
-- | Another  example for the library that uses embedded protocols with multiple server back
-- ends and a polymorphic client, as well as the 'Supervisor.Sup' module to start multiple
-- back-ends.
--
-- @since 0.29.0
module Main where

import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.State.Lazy as State
import           Control.Eff.Concurrent
import           Control.Eff.Concurrent.Process.Timer
import           Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import           Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import           Control.Eff.Concurrent.Protocol.Supervisor as Supervisor
import           Control.Lens
import           Control.Monad
import           Data.Dynamic
import           Data.Foldable
import           Data.Functor.Contravariant (contramap)
import           Data.String
import qualified Data.Text as T
import GHC.Stack (HasCallStack)

main :: IO ()
main =
  defaultMain (void embeddedExample)

embeddedExample :: HasCallStack => Eff Effects ()
embeddedExample = do
  b1 <- Stateful.start InitBackend1
  b2Sup <- startBackend2Sup
  b2 <- Supervisor.spawnOrLookup @Backend2 b2Sup 1
  _ <- Supervisor.spawnOrLookup b2Sup 2
  b2_2 <- Supervisor.spawnOrLookup b2Sup 2
  b2_3 <- Supervisor.spawnOrLookup b2Sup 3
  app <- Stateful.start InitApp
  cast app DoThis
  cast app DoThis
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b1)))
  cast app DoThis
  cast app DoThis
  spawn_ "sub-process" $ do
    logNotice "spawned sub process"
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
  _ <- Supervisor.stopChild b2Sup 2
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b2_3)))
  cast app DoThis
  Supervisor.stopSupervisor b2Sup
  cast app DoThis


------------------------------ Server Instances

-- Application layer

instance Stateful.Server App Effects where
  type instance Model App = Maybe SomeBackend
  data instance StartArgument App Effects = InitApp
  update me _x e =
    case e of
      OnCall rt (SetBackend b) -> do
        logInfo "setting backend"
        oldB <- getAndPutModel @App b
        traverse_ (`backendForgetObserver` me) oldB
        traverse_ (`backendRegisterObserver` me) b
        sendReply rt ()
      OnCast (AppBackendEvent be) ->
        logInfo ("got backend event: " <> T.pack (show be))
      OnCast DoThis ->
        do m <- getModel @App
           case m of
            Nothing -> logInfo "doing this without backend"
            Just b -> handleInterrupts (logWarning . T.pack . show) $ do
                doSomeBackendWork b
                bi <- getSomeBackendInfo b
                logInfo ("doing this. Backend: " <> T.pack bi)
      _ -> logWarning ("unexpected: "<>T.pack(show e))


------------------------------ Protocol Data Types

-- Application layer

data App deriving Typeable

instance HasPdu App where
  type EmbeddedPduList App = '[Observer BackendEvent]
  data Pdu App r where
    SetBackend :: Maybe SomeBackend -> Pdu App ('Synchronous ())
    DoThis :: Pdu App 'Asynchronous
    AppBackendEvent :: Pdu (Observer BackendEvent) r -> Pdu App r
    deriving Typeable

instance NFData (Pdu App r) where
  rnf (SetBackend !_x) = ()
  rnf DoThis = ()
  rnf (AppBackendEvent e) = rnf e

instance Show (Pdu App r) where
  show (SetBackend _x) = "setting backend"
  show DoThis = "doing this"
  show (AppBackendEvent e) = "got backend event: " ++ show e

instance HasPduPrism App (Observer BackendEvent) where
  embedPdu = AppBackendEvent
  fromPdu (AppBackendEvent e) = Just e
  fromPdu _ = Nothing

-- Backend
data Backend deriving Typeable

instance HasPdu Backend where
  data Pdu Backend r where
    BackendWork :: Pdu Backend 'Asynchronous
    GetBackendInfo :: Pdu Backend ('Synchronous String)
    deriving Typeable

instance NFData (Pdu Backend r) where
  rnf BackendWork = ()
  rnf GetBackendInfo = ()

instance Show (Pdu Backend r) where
  show BackendWork = "BackendWork"
  show GetBackendInfo = "GetBackendInfo"

newtype BackendEvent where
    BackendEvent :: String -> BackendEvent
    deriving (NFData, Show, Typeable)

type IsBackend b =
  ( HasPdu b
  , Embeds b Backend
  , IsObservable b BackendEvent
  , Tangible (Pdu b ('Synchronous String))
  , Tangible (Pdu b 'Asynchronous)
  )

data SomeBackend =
  forall b . IsBackend b => SomeBackend (Endpoint b)

withSomeBackend ::
   SomeBackend
  -> (forall b . IsBackend b => Endpoint b -> x )
  -> x
withSomeBackend (SomeBackend x) f = f x

backendRegisterObserver
  :: ( HasProcesses e q
     , CanObserve m BackendEvent
     , Embeds m (Observer BackendEvent)
     , Tangible (Pdu m 'Asynchronous))
  => SomeBackend
  -> Endpoint m
  -> Eff e ()
backendRegisterObserver (SomeBackend x) o = registerObserver @BackendEvent x o

backendForgetObserver
  :: ( HasProcesses e q
     , CanObserve m BackendEvent
     , Embeds m (Observer BackendEvent)
     , Tangible (Pdu m 'Asynchronous)
     )
  => SomeBackend
  -> Endpoint m
  -> Eff e ()
backendForgetObserver (SomeBackend x) o = forgetObserver @BackendEvent x o

getSomeBackendInfo :: HasProcesses e q => SomeBackend -> Eff e String
getSomeBackendInfo (SomeBackend x) = call x GetBackendInfo

doSomeBackendWork ::  HasProcesses e q => SomeBackend -> Eff e ()
doSomeBackendWork (SomeBackend x) = cast x BackendWork

-------------------------

-- Backend 1

data Backend1 deriving Typeable

instance Stateful.Server Backend1 Effects where
  type instance Protocol Backend1 = (Backend, ObserverRegistry BackendEvent)
  type instance Model Backend1 = (Int, ObserverRegistry BackendEvent)
  data instance StartArgument Backend1 Effects = InitBackend1
  setup _ _ = pure ( (0, emptyObserverRegistry), () )
  update me _ e = do
    model <- getModel @Backend1
    case e of
      OnCall rt (ToPduLeft GetBackendInfo) ->
        sendReply
          (toEmbeddedReplyTarget @(Stateful.Protocol Backend1) @Backend rt)
          ("Backend1 " <> show me <> " " <> show (model ^. _1))
      OnCast (ToPduLeft BackendWork) -> do
        logInfo "working..."
        modifyModel @Backend1 (over _1 (+ 1))
      OnCast (ToPduRight x) -> do
        logInfo "event registration stuff ..."
        zoomModel @Backend1 _2 (observerRegistryHandlePdu x)
      OnDown pd -> do
        logWarning (T.pack (show pd))
        wasObserver <- zoomModel @Backend1 _2 (observerRegistryRemoveProcess @BackendEvent (downProcess pd))
        when wasObserver $
          logNotice "observer removed"
      _ -> logWarning ("unexpected: " <> T.pack (show e))

-- Backend 2 is behind a supervisor

data Backend2 deriving Typeable

instance HasPdu Backend2 where
  type instance EmbeddedPduList Backend2 = '[Backend, ObserverRegistry BackendEvent]
  data instance Pdu Backend2 r where
    B2ObserverRegistry :: Pdu (ObserverRegistry BackendEvent) r -> Pdu Backend2 r
    B2BackendWork :: Pdu Backend r -> Pdu Backend2 r
    deriving Typeable

instance NFData (Pdu Backend2 r) where
  rnf (B2BackendWork w) = rnf w
  rnf (B2ObserverRegistry x) = rnf x

instance Show (Pdu Backend2 r) where
  show (B2BackendWork w) = show w
  show (B2ObserverRegistry x) = show x

instance HasPduPrism Backend2 Backend where
  embedPdu = B2BackendWork
  fromPdu (B2BackendWork x) = Just x
  fromPdu _ = Nothing

instance HasPduPrism Backend2 (ObserverRegistry BackendEvent) where
  embedPdu = B2ObserverRegistry
  fromPdu (B2ObserverRegistry x) = Just x
  fromPdu _ = Nothing

instance Effectful.Server Backend2 Effects where
  type instance ServerEffects Backend2 Effects = State Int ': ObserverRegistryState BackendEvent ': Effects
  data instance Init Backend2 Effects = InitBackend2 Int
  serverTitle (InitBackend2 x) = fromString ("backend-2: " ++ show x)
  runEffects _me _ e =  evalObserverRegistryState (evalState 0 e)
  onEvent me _ e = do
    myIndex <- get @Int
    case e of
      OnCall rt (B2BackendWork GetBackendInfo) ->
        sendReply rt ("Backend2 " <> show me <> " " <> show myIndex)
      OnCast (B2BackendWork BackendWork) -> do
        logInfo "working..."
        put @Int (myIndex + 1)
        when (myIndex `mod` 2 == 0)
          (observerRegistryNotify (BackendEvent "even!"))
      OnCast (B2ObserverRegistry x) -> do
        logInfo "event registration stuff ..."
        observerRegistryHandlePdu @BackendEvent x
      OnInterrupt NormalExitRequested
        | even myIndex -> do
          logNotice "Kindly exitting -_-"
          exitNormally
        | otherwise ->
          logNotice "Ignoring exit request! :P"
      OnDown pd -> do
        logWarning (T.pack (show pd))
        wasObserver <- observerRegistryRemoveProcess @BackendEvent (downProcess pd)
        when wasObserver $
          logNotice "observer removed"
      _ -> logWarning ("unexpected: " <> T.pack (show e))

type instance Supervisor.ChildId Backend2 = Int

startBackend2Sup :: Eff Effects (Endpoint (Supervisor.Sup Backend2))
startBackend2Sup = Supervisor.startSupervisor (Supervisor.MkSupConfig (TimeoutMicros 1_000_000) InitBackend2)

-- EXPERIMENTING
data EP a where
  EP :: forall a (r :: Synchronicity) . (NFData (Pdu a r), Typeable r) => Receiver (Pdu a r) -> EP a

sendEPCast
  :: forall a e q
  . (HasProcesses e q)
  => EP a
  -> (forall x . Pdu a x)
  -> Eff e ()
sendEPCast (EP r) p = sendToReceiver r p

embeddedReceiver
  :: forall  a b
  . (Embeds a b, (forall (r :: Synchronicity) . Typeable r => NFData (Pdu b r) ))
  => EP a
  -> EP b
embeddedReceiver (EP r) = EP (contramap embedPdu r)


