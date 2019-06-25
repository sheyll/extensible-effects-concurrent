{-# LANGUAGE UndecidableInstances #-}
-- | Another  example for the library that uses embedded protocols with multiple server back
-- ends and a polymorphic client.
module Main where

import           Data.Dynamic
import           Control.Eff
import           Control.Eff.Concurrent
import           Control.Eff.Concurrent.Protocol
import           Control.Eff.Concurrent.Protocol.Client
import           Control.Eff.Concurrent.Protocol.StatefulServer as Server
import           Control.Monad
import           Data.Foldable
import           Control.Lens
import           Control.DeepSeq
import qualified Data.Text as T

main :: IO ()
main =
  defaultMain (void embeddedExample)

embeddedExample :: Eff Effects ()
embeddedExample = do
  b1 <- Server.start InitBackend1
  b2 <- Server.start InitBackend2
  app <- Server.start InitApp
  cast app DoThis
  cast app DoThis
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b1)))
  cast app DoThis
  cast app DoThis
  cast app DoThis
  cast app DoThis
  call app (SetBackend Nothing)
  cast app DoThis
  cast app DoThis
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b1)))
  cast app DoThis
  cast app DoThis
  cast app DoThis
  call app (SetBackend (Just (SomeBackend b2)))
  cast app DoThis
  cast app DoThis
  cast app DoThis
  call app (SetBackend Nothing)

------------------------------ Server Instances

-- Application layer

instance Server.Server App Effects where
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
      OnCast DoThis ->
        do m <- getModel @App
           case m of
            Nothing -> logInfo "doing this without backend"
            Just b -> do
                doSomeBackendWork b
                bi <- getSomeBackendInfo b
                logInfo ("doing this. Backend: " <> T.pack bi)
      _ -> logWarning ("unexpected: "<>T.pack(show e))


------------------------------ Protocol Data Types

-- Application layer

data App deriving Typeable

instance HasPdu App where
  type EmbeddedProtocols App = '[Observer BackendEvent]
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

instance EmbedProtocol App (Observer BackendEvent) 'Asynchronous where
  embedPdu = AppBackendEvent
  fromPdu (AppBackendEvent e) = Just e
  fromPdu _ = Nothing

instance EmbedProtocol2 App (Observer BackendEvent) where
  embedPdu2 = AppBackendEvent
  fromPdu2 (AppBackendEvent e) = Just e
  fromPdu2 _ = Nothing

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
--  , EmbedProtocol b Backend 'Asynchronous
--  , EmbedProtocol b Backend ('Synchronous String)
  , EmbedProtocol2 b Backend
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
     , EmbedProtocol2 m (Observer BackendEvent)
     , Tangible (Pdu m 'Asynchronous))
  => SomeBackend
  -> Endpoint m
  -> Eff e ()
backendRegisterObserver (SomeBackend x) o = registerObserver @BackendEvent x o

backendForgetObserver
  :: ( HasProcesses e q
     , CanObserve m BackendEvent
     , EmbedProtocol2 m (Observer BackendEvent)
     , Tangible (Pdu m 'Asynchronous)
     )
  => SomeBackend
  -> Endpoint m
  -> Eff e ()
backendForgetObserver (SomeBackend x) o = forgetObserver @BackendEvent x o

getSomeBackendInfo :: HasProcesses e q => SomeBackend -> Eff e String
getSomeBackendInfo (SomeBackend x) = call2 x GetBackendInfo

doSomeBackendWork ::  HasProcesses e q => SomeBackend -> Eff e ()
doSomeBackendWork (SomeBackend x) = cast2 x BackendWork

-------------------------

-- Backend 1

data Backend1 deriving Typeable

instance Server.Server Backend1 Effects where
  type instance Protocol Backend1 = (Backend, ObserverRegistry BackendEvent)
  type instance Model Backend1 = (Int, ObserverRegistry BackendEvent)
  data instance StartArgument Backend1 Effects = InitBackend1
  setup _ _ = pure ( (0, emptyObserverRegistry), () )
  update me _ e = do
    model <- getModel @Backend1
    case e of
      OnCall rt (ToPduLeft GetBackendInfo) ->
        sendReply
          (toEmbeddedReplyTarget @(Server.Protocol Backend1) @Backend rt)
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

-- Backend 2

data Backend2 deriving Typeable

instance HasPdu Backend2 where
  type instance EmbeddedProtocols Backend2 = '[Backend, ObserverRegistry BackendEvent]
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

instance EmbedProtocol2 Backend2 Backend where
  embedPdu2 = B2BackendWork
  fromPdu2 (B2BackendWork x) = Just x
  fromPdu2 _ = Nothing

instance EmbedProtocol2 Backend2 (ObserverRegistry BackendEvent) where
  embedPdu2 = B2ObserverRegistry
  fromPdu2 (B2ObserverRegistry x) = Just x
  fromPdu2 _ = Nothing

instance Server.Server Backend2 Effects where
  type instance Model Backend2 = (Int, ObserverRegistry BackendEvent)
  data instance StartArgument Backend2 Effects = InitBackend2
  setup _ _ = pure ( (0, emptyObserverRegistry), () )
  update me _ e = do
    model <- getModel @Backend2
    case e of
      OnCall rt (B2BackendWork GetBackendInfo) ->
        sendReply rt ("Backend2 " <> show me <> " " <> show (model ^. _1))
      OnCast (B2BackendWork BackendWork) -> do
        logInfo "working..."
        oldM <- getAndModifyModel @Backend2 (over _1 (+ 1))
        when (fst oldM `mod` 2 == 0)
          (zoomModel @Backend2 _2 (observerRegistryNotify (BackendEvent "even!")))
      OnCast (B2ObserverRegistry x) -> do
        logInfo "event registration stuff ..."
        zoomModel @Backend2 _2 (observerRegistryHandlePdu x)
      OnDown pd -> do
        logWarning (T.pack (show pd))
        wasObserver <- zoomModel @Backend2 _2 (observerRegistryRemoveProcess @BackendEvent (downProcess pd))
        when wasObserver $
          logNotice "observer removed"
      _ -> logWarning ("unexpected: " <> T.pack (show e))
