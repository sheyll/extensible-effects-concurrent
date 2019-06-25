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

instance Typeable r => HasPdu App r where
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

instance Typeable r => HasPdu Backend r where
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
  ( HasPdu b ('Synchronous String)
  , HasPdu b 'Asynchronous
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
    logInfo ("got event: " <> T.pack (show e))
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
