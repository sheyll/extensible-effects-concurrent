{-# LANGUAGE UndecidableInstances #-}
module ObserverTests
  ( test_observer
  ) where

import Common
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as S
import qualified Control.Eff.Concurrent.Protocol.Observer.Queue as OQ
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as M
import Control.Lens
import Data.Typeable (Typeable)


test_observer :: HasCallStack => TestTree
test_observer = testGroup "observer" (basicTests ++ [observerQueueTests])


basicTests :: HasCallStack => [TestTree]
basicTests =
  [runTestCase "when no observer is present, nothing crashes"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            call testObservable (SendTestEvent "1")
            cast testObservable StopTestObservable

            void $ awaitProcessDown (testObservable ^. fromEndpoint)

  , runTestCase "observers receive only messages sent after registration"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver <- M.startLink MkTestObserver
            registerObserver @String testObservable testObserver
            call testObservable (SendTestEvent "4")
            call testObservable (SendTestEvent "5")
            es <- call testObserver GetCapturedEvents
            lift (["4", "5"] @=? es)
            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "observers receive only messages sent before de-registration"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver <- M.startLink MkTestObserver
            registerObserver @String testObservable testObserver
            call testObservable (SendTestEvent "4")
            call testObservable (SendTestEvent "5")
            forgetObserver @String testObservable testObserver
            call testObservable (SendTestEvent "6")
            call testObservable (SendTestEvent "7")
            es <- call testObserver GetCapturedEvents
            lift (["4", "5"] @=? es)
            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "observers receive only messages sent between registration and deregistration"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver <- M.startLink MkTestObserver
            registerObserver @String testObservable testObserver
            call testObservable (SendTestEvent "4")
            call testObservable (SendTestEvent "5")
            forgetObserver @String testObservable testObserver
            call testObservable (SendTestEvent "6")
            registerObserver @String testObservable testObserver
            call testObservable (SendTestEvent "7")
            es <- call testObserver GetCapturedEvents
            lift (["4", "5", "7"] @=? es)
            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "all observers receive all messages sent between registration and deregistration"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver1 <- M.startLink MkTestObserver
            testObserver2 <- M.startLink MkTestObserver
            testObserver3 <- M.startLink MkTestObserver
            registerObserver @String testObservable testObserver1
            call testObservable (SendTestEvent "4")
            registerObserver @String testObservable testObserver2
            call testObservable (SendTestEvent "5")
            registerObserver @String testObservable testObserver3
            call testObservable (SendTestEvent "6")
            forgetObserver @String testObservable testObserver1
            call testObservable (SendTestEvent "7")
            forgetObserver @String testObservable testObserver2
            call testObservable (SendTestEvent "8")
            forgetObserver @String testObservable testObserver3
            call testObservable (SendTestEvent "9")
            es1 <- call testObserver1 GetCapturedEvents
            lift (["4", "5", "6"] @=? es1)
            es2 <- call testObserver2 GetCapturedEvents
            lift (["5", "6", "7"] @=? es2)
            es3 <- call testObserver3 GetCapturedEvents
            lift (["6", "7", "8"] @=? es3)
            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "when an observer exits, the messages are still deliviered to the others"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver1 <- M.startLink MkTestObserver
            testObserver2 <- M.startLink MkTestObserver
            registerObserver @String testObservable testObserver1
            registerObserver @String testObservable testObserver2
            call testObservable (SendTestEvent "4")
            call testObservable (SendTestEvent "5")
            sendShutdown (testObserver2^.fromEndpoint) ExitNormally
            void $ awaitProcessDown (testObserver2^.fromEndpoint)
            call testObservable (SendTestEvent "6")
            es1 <- call testObserver1 GetCapturedEvents
            lift (["4", "5", "6"] @=? es1)
            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "evil observer monitoring"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver1 <- M.startLink MkTestObserver
            testObserver2 <- M.startLink MkTestObserver
            registerObserver @String testObservable testObserver1
            sendShutdown (testObserver2^.fromEndpoint) ExitNormally
            void $ monitor (testObserver2^.fromEndpoint)
            void $ awaitProcessDownAny
            call testObservable (SendTestEvent "6")
            es1 <- call testObserver1 GetCapturedEvents
            lift (["6"] @=? es1)
            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "when an observer registers multiple times, it still gets the messages only once"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver1 <- M.startLink MkTestObserver
            registerObserver @String testObservable testObserver1
            registerObserver @String testObservable testObserver1
            call testObservable (SendTestEvent "4")
            call testObservable (SendTestEvent "5")
            call testObservable (SendTestEvent "6")
            es1 <- call testObserver1 GetCapturedEvents
            lift (["4", "5", "6"] @=? es1)
            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "when an observer is forgotton multiple times, nothing bad happens"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver1 <- M.startLink MkTestObserver
            registerObserver @String testObservable testObserver1
            call testObservable (SendTestEvent "4")
            call testObservable (SendTestEvent "5")
            forgetObserver @String testObservable testObserver1
            forgetObserver @String testObservable testObserver1
            forgetObserver @String testObservable testObserver1
            call testObservable (SendTestEvent "6")
            es1 <- call testObserver1 GetCapturedEvents
            lift (["4", "5"] @=? es1)
            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  ]

observerQueueTests :: TestTree
observerQueueTests =
  testGroup "observer-queue"
  [ runTestCase "tryRead"
      $ do
          testObservable <- S.startLink TestObservableServerInit
          let len :: Int
              len = 1
          OQ.observe  @String len testObservable $ do
            OQ.tryRead @String >>= lift . (Nothing @=?)
            call testObservable (SendTestEvent "1")
            OQ.tryRead @String >>= lift . (Just "1" @=?)

          cast testObservable StopTestObservable
          void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "observe then read"
      $ do
          testObservable <- S.startLink TestObservableServerInit
          let len :: Int
              len = 1
          OQ.observe  @String len testObservable $ do
            call testObservable (SendTestEvent "1")
            OQ.await @String >>= lift . ("1" @=?)
            call testObservable (SendTestEvent "2")
            OQ.await @String >>= lift . ("2" @=?)
            call testObservable (SendTestEvent "3")
            OQ.await @String >>= lift . ("3" @=?)

          cast testObservable StopTestObservable
          void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "FIFO"
      $ do
          testObservable <- S.startLink TestObservableServerInit
          let len :: Int
              len = 3
          OQ.observe  @String len testObservable $ do
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            OQ.await @String >>= lift . ("1" @=?)
            OQ.await @String >>= lift . ("2" @=?)
            OQ.await @String >>= lift . ("3" @=?)

          cast testObservable StopTestObservable
          void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "flush"
      $ do
          testObservable <- S.startLink TestObservableServerInit
          let len :: Int
              len = 3
          OQ.observe  @String len testObservable $ do
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            OQ.flush @String >>= lift . (["1", "2", "3"] @=?)
            OQ.tryRead @String >>= lift . (Nothing @=?)

          cast testObservable StopTestObservable
          void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "when the queue is full, new observations are dropped"
      $ do
            testObservable <- S.startLink TestObservableServerInit
            let len :: Int
                len = 2
            OQ.observe  @String len testObservable $ do
              call testObservable (SendTestEvent "1")
              call testObservable (SendTestEvent "2")
              call testObservable (SendTestEvent "3")
              call testObservable (SendTestEvent "4")
              OQ.await @String >>= lift . ("1" @=?)
              OQ.await @String >>= lift . ("2" @=?)
              OQ.tryRead @String >>= lift . (Nothing @=?)

            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "flush after queue full"
      $ do
          testObservable <- S.startLink TestObservableServerInit
          let len :: Int
              len = 3
          OQ.observe  @String len testObservable $ do
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            call testObservable (SendTestEvent "4")
            call testObservable (SendTestEvent "5")
            OQ.flush @String >>= lift . (["1", "2", "3"] @=?)
            OQ.flush @String >>= lift . ([] @=?)
            OQ.tryRead @String >>= lift . (Nothing @=?)
            call testObservable (SendTestEvent "6")
            OQ.await @String >>= lift . ("6" @=?)

          cast testObservable StopTestObservable
          void $ awaitProcessDown (testObservable ^. fromEndpoint)
  ]


data TestObservable deriving Typeable

instance HasPdu TestObservable where
     type instance EmbeddedPduList TestObservable = '[ObserverRegistry String]
     data Pdu TestObservable r where
      SendTestEvent :: String -> Pdu TestObservable ('Synchronous ())
      StopTestObservable :: Pdu TestObservable 'Asynchronous
      TestObsReg :: Pdu (ObserverRegistry String) r -> Pdu TestObservable r
      deriving (Typeable)

instance HasPduPrism TestObservable (ObserverRegistry String) where
  embedPdu = TestObsReg
  fromPdu (TestObsReg x) = Just x
  fromPdu _ = Nothing

instance Typeable r => NFData (Pdu TestObservable r) where
  rnf (SendTestEvent s) = rnf s
  rnf StopTestObservable = ()
  rnf (TestObsReg x) = rnf x

instance Typeable r => Show (Pdu TestObservable r) where
  show (SendTestEvent x) = "SendTestEvent " ++ show x
  show StopTestObservable = "StopTestObservable"
  show (TestObsReg x) = "TestObsReg " ++ show x

instance (IoLogging r, HasProcesses r q) => S.Server TestObservable r where
  data Init TestObservable = TestObservableServerInit
  type ServerEffects TestObservable r = ObserverRegistryState String ': r
  runEffects _ _ = evalObserverRegistryState
  onEvent _ _ =
    \case
      S.OnCall rt e ->
        case e of
          SendTestEvent x -> observerRegistryNotify x >> sendReply rt ()
          TestObsReg x -> logError ("unexpected: " <> pack (show x))
      S.OnCast (TestObsReg x) -> observerRegistryHandlePdu x
      S.OnCast StopTestObservable -> exitNormally
      S.OnDown pd -> do
        logDebug ("inspecting: " <> pack (show pd))
        wasHandled <- observerRegistryRemoveProcess @String (downProcess pd)
        unless wasHandled $
          logError ("the process down message was not handled: " <> pack (show pd))
      other ->
        logError ("unexpected: " <> pack (show other))


data TestObserver deriving Typeable

instance HasPdu TestObserver where
  type EmbeddedPduList TestObserver = '[Observer String]
  data Pdu TestObserver r where
    GetCapturedEvents :: Pdu TestObserver ('Synchronous [String])
    OnTestEvent :: Pdu (Observer String) r -> Pdu TestObserver r
    deriving Typeable

instance NFData (Pdu TestObserver r) where
  rnf GetCapturedEvents = ()
  rnf (OnTestEvent e) =  rnf e

instance Show (Pdu TestObserver r) where
  showsPrec _ GetCapturedEvents = showString "GetCapturedEvents"
  showsPrec d (OnTestEvent e) = showParen (d>=10) (showString "OnTestEvent " . shows e)

instance HasPduPrism TestObserver (Observer String) where
  embedPdu = OnTestEvent
  fromPdu (OnTestEvent e) = Just e
  fromPdu _ = Nothing


instance (IoLogging r, HasProcesses r q) => M.Server TestObserver r where
  data StartArgument TestObserver = MkTestObserver
  newtype instance Model TestObserver = TestObserverModel {fromTestObserverModel :: [String]} deriving Default
  update _ MkTestObserver e =
    case e of
      M.OnCall rt GetCapturedEvents ->
        M.getAndPutModel (TestObserverModel []) >>= sendReply rt . fromTestObserverModel
      M.OnCast (OnTestEvent (Observed x)) ->
        M.modifyModel (\ (TestObserverModel o) -> TestObserverModel (o ++ [x]))
      _ ->
        logError ("unexpected: " <> pack (show e))
