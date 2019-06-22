{-# LANGUAGE UndecidableInstances #-}
module ObserverTests
  ( test_observer
  ) where

import Common
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as S
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as M
import Control.Lens
import Control.Monad
import Data.Text as T
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.HUnit


test_observer :: HasCallStack => TestTree
test_observer = testGroup "observer" observerQueueTests


observerQueueTests :: HasCallStack => [TestTree]
observerQueueTests =
  [runTestCase "when no observer is present, nothing crashes"
      $ do
            testObservable <- S.start TestObservableServerInit
            call testObservable (SendTestEvent "1")
            cast testObservable StopTestObservable

            void $ awaitProcessDown (testObservable ^. fromEndpoint)

  , runTestCase "observers receive only messages sent after registration"
      $ do
            testObservable <- S.start TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver <- M.start MkTestObserver
            registerObserver @String testObservable testObserver
            call testObservable (SendTestEvent "4")
            call testObservable (SendTestEvent "5")
            es <- call testObserver GetCapturedEvents
            lift (["4", "5"] @=? es)
            cast testObservable StopTestObservable
            void $ awaitProcessDown (testObservable ^. fromEndpoint)
  , runTestCase "observers receive only messages sent before de-registration"
      $ do
            testObservable <- S.start TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver <- M.start MkTestObserver
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
            testObservable <- S.start TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver <- M.start MkTestObserver
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
            testObservable <- S.start TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver1 <- M.start MkTestObserver
            testObserver2 <- M.start MkTestObserver
            testObserver3 <- M.start MkTestObserver
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
            testObservable <- S.start TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver1 <- M.start MkTestObserver
            testObserver2 <- M.start MkTestObserver
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
  , runTestCase "when an observer registers multiple times, it still gets the messages only once"
      $ do
            testObservable <- S.start TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver1 <- M.start MkTestObserver
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
            testObservable <- S.start TestObservableServerInit
            call testObservable (SendTestEvent "1")
            call testObservable (SendTestEvent "2")
            call testObservable (SendTestEvent "3")
            testObserver1 <- M.start MkTestObserver
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


data TestObservable deriving Typeable

instance Typeable r => HasPdu TestObservable r  where
     data Pdu TestObservable r where
      SendTestEvent :: String -> Pdu TestObservable ('Synchronous ())
      StopTestObservable :: Pdu TestObservable 'Asynchronous
      TestObsReg :: Pdu (ObserverRegistry String) 'Asynchronous -> Pdu TestObservable 'Asynchronous
      deriving (Typeable)

instance EmbedProtocol TestObservable (ObserverRegistry String) 'Asynchronous where
  embeddedPdu = prism' TestObsReg $
    \case
      TestObsReg e -> Just e
      _ -> Nothing

instance Typeable r => NFData (Pdu TestObservable r) where
  rnf (SendTestEvent s) = rnf s
  rnf StopTestObservable = ()
  rnf (TestObsReg x) = rnf x

instance Typeable r => Show (Pdu TestObservable r) where
  show (SendTestEvent x) = "SendTestEvent " ++ show x
  show StopTestObservable = "StopTestObservable"
  show (TestObsReg x) = "TestObsReg " ++ show x

instance (LogIo r, HasProcesses r q) => S.Server TestObservable r where
  data Init TestObservable r = TestObservableServerInit
  type ServerEffects TestObservable r = ObserverRegistryState String ': r
  runEffects _ _ = evalObserverRegistryState
  onEvent _ _ =
    \case
      S.OnCall rt e ->
        case e of
          SendTestEvent x -> observerRegistryNotify x >> sendReply rt ()
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

instance Typeable r => HasPdu TestObserver r where
  data Pdu TestObserver r where
    GetCapturedEvents :: Pdu TestObserver ('Synchronous [String])
    OnTestEvent :: Pdu (Observer String) 'Asynchronous -> Pdu TestObserver 'Asynchronous
    deriving Typeable

instance NFData (Pdu TestObserver r) where
  rnf GetCapturedEvents = ()
  rnf (OnTestEvent e) =  rnf e

instance Show (Pdu TestObserver r) where
  showsPrec _ GetCapturedEvents = showString "GetCapturedEvents"
  showsPrec d (OnTestEvent e) = showParen (d>=10) (showString "OnTestEvent " . shows e)

instance EmbedProtocol TestObserver (Observer String) 'Asynchronous where
  embeddedPdu = prism' OnTestEvent $
    \case
      OnTestEvent e -> Just e


instance (LogIo r, HasProcesses r q) => M.Server TestObserver r where
  data StartArgument TestObserver r = MkTestObserver
  type Model TestObserver = [String]
  setup _ MkTestObserver = pure ([], ())
  update _ MkTestObserver e =
    case e of
      M.OnCall rt GetCapturedEvents ->
        M.getAndPutModel @TestObserver [] >>= sendReply rt
      M.OnCast (OnTestEvent (Observed x)) ->
        M.modifyModel @TestObserver (++ [x])
      _ ->
        logError ("unexpected: " <> pack (show e))
