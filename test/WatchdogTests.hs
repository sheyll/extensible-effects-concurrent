{-# LANGUAGE NumericUnderscores, UndecidableInstances #-}
module WatchdogTests(test_watchdogTests) where

import Common  hiding (runTestCase)
import qualified Common
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import qualified Control.Eff.Concurrent.Protocol.Broker as Broker
import qualified Control.Eff.Concurrent.Protocol.Observer.Queue as OQ
import qualified Control.Eff.Concurrent.Protocol.Watchdog as Watchdog
import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio
import Data.Fixed
import Data.Time.Clock


runTestCase :: TestName -> Eff Effects () -> TestTree
runTestCase = Common.runTestCase


test_watchdogTests :: HasCallStack => TestTree
test_watchdogTests =
  testGroup "watchdog"
    [ runTestCase "demonstrate Bookshelf" bookshelfDemo

    , testGroup "watchdog broker interaction"
      [

        runTestCase "test 1: when the broker exits, the watchdog does not care, it can be re-attached to a new broker" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attach wd broker
            logNotice "attached watchdog"
            assertShutdown (broker ^. fromEndpoint) ExitNormally
            logNotice "stopped broker"

          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started new broker"
            Watchdog.attach wd broker
            logNotice "attached broker"
            let expected = ExitUnhandledError "test-broker-kill"
            logNotice "crashing broker"
            assertShutdown (broker ^. fromEndpoint) expected
            logNotice "crashed broker"

          logNotice "shutting down watchdog"
          assertShutdown (wd^.fromEndpoint) ExitNormally
          logNotice "watchdog down"


      , runTestCase "test 2: when the broker exits, and the watchdog is linked to it, it will exit" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attachLinked wd broker
            logNotice "attached and linked broker"
            logNotice "crashing broker"
            let expected = ExitUnhandledError "test-broker-kill"
            sendShutdown (broker ^. fromEndpoint) expected
            logNotice "crashed broker"
            logNotice "waiting for watchdog to crash"
            awaitProcessDown (wd^.fromEndpoint) >>= lift . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (broker^.fromEndpoint)) . downReason
            logNotice "watchdog crashed"

      , runTestCase "test 3: when the same broker is attached twice, the second attachment is ignored" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attach wd broker
            logNotice "attached broker"
            Watchdog.attach wd broker
            logNotice "attached broker"
            logNotice "restart child test begin"
            restartChildTest broker
            logNotice "restart child test finished"

      , runTestCase "test 4: when the same broker is attached twice, the first linked then not linked, the watchdog is not linked anymore" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attachLinked wd broker
            logNotice "attached and linked broker"
            Watchdog.attach wd broker
            logNotice "attached broker"
            logNotice "run restart child test"
            restartChildTest broker
            logNotice "restart child test finished"
            logNotice "crashing broker"
            let expected = ExitUnhandledError "test-broker-kill"
            assertShutdown (broker ^. fromEndpoint) expected
            logNotice "crashed broker"
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started new broker"
            Watchdog.attach wd broker
            logNotice "attached new broker"
            logNotice "run restart child test again to show everything is ok"
            restartChildTest broker
            logNotice "restart child test finished"


      , runTestCase "test 5: when the same broker is attached twice, the first time not linked but then linked, the watchdog is linked" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attach wd broker
            logNotice "attached broker"
            Watchdog.attachLinked wd broker
            logNotice "attached and linked broker"
            logNotice "run restart child test"
            restartChildTest broker
            logNotice "restart child test finished"
            logNotice "crashing broker"
            let expected = ExitUnhandledError "test-broker-kill"
            assertShutdown (broker ^. fromEndpoint) expected
            logNotice "crashed broker"
            logNotice "waiting for watchdog to crash"
            awaitProcessDown (wd^.fromEndpoint) >>= lift . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (broker^.fromEndpoint)) . downReason
            logNotice "watchdog down"


      , runTestCase "test 6: when multiple brokers are attached to a watchdog, and a linked broker exits, the watchdog should exit" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker1 <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker1^.fromEndpoint)
            logNotice "started broker 1"
            Watchdog.attachLinked wd broker1
            logNotice "attached + linked broker 1"

            broker2 <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            logNotice "started broker 2"
            Watchdog.attach wd broker2
            logNotice "attached broker 2"

            broker3 <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            logNotice "started broker 3"
            Watchdog.attach wd broker3
            logNotice "attached broker 3"
            unlinkProcess (broker3 ^. fromEndpoint)

            let expected = ExitUnhandledError "test-broker-kill 1"
            logNotice "crashing broker 1"
            assertShutdown (broker1 ^. fromEndpoint) expected
            logNotice "crashed broker 1"

            isProcessAlive (broker2 ^. fromEndpoint) >>= lift . assertBool "broker2 should be running"
            isProcessAlive (broker3 ^. fromEndpoint) >>= lift . assertBool "broker3 should be running"
            logNotice "other brokers are alive"

            logNotice "waiting for watchdog to crash"
            awaitProcessDown (wd^.fromEndpoint) >>= lift . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (broker1^.fromEndpoint)) . downReason
            logNotice "watchdog crashed"

      ]
    , testGroup "restarting children"
      [ runTestCase "test 7: when a child exits it is restarted" $ do
          broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
          logNotice "started broker"
          wd <- Watchdog.startLink
          logNotice "started watchdog"
          Watchdog.attach wd broker
          logNotice "attached broker"
          logNotice "running child tests"
          restartChildTest broker
          logNotice "finished child tests"

      , runTestCase "test 8: when the broker emits the shutting down event the watchdog does not restart children" $ do
          broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
          unlinkProcess (broker ^. fromEndpoint)
          logNotice "started broker"
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          Watchdog.attach wd broker
          OQ.observe @(Broker.ChildEvent (Stateful.Stateful BookShelf)) (100 :: Int) broker $ do
            let c0 = BookShelfId 0
            do
              void $ Broker.spawnOrLookup broker c0
              OQ.await @(Broker.ChildEvent (Stateful.Stateful BookShelf)) >>= logNotice . pack . show
              logNotice "bookshelf started"

              Broker.stopBroker broker
              OQ.await @(Broker.ChildEvent (Stateful.Stateful BookShelf)) >>= logNotice . pack . show -- broker shutting down event
              void $ awaitProcessDown (broker ^. fromEndpoint)
              logNotice "broker stopped"

          awaitProcessDown (wd ^. fromEndpoint) >>= logNotice . pack . show
          logNotice "watchdog stopped"

      , testGroup "reusing ChildIds"
          []
--        [ runTestCase "when a child exits normally, and a new child with the same ChildId crashes, the watchdog behaves as if the first child never existed" $ do
--            error "TODO"
--        ]
      ]
    ]
bookshelfDemo :: HasCallStack => Eff Effects ()
bookshelfDemo = do
  logNotice "Bookshelf Demo Begin"
  broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
  shelf1 <- Broker.spawnOrLookup broker (BookShelfId 1)
  call shelf1 (AddBook "Solaris")
  call shelf1 GetBookList >>= logDebug . pack . show
  cast shelf1 (TakeBook "Solaris")
  call shelf1 GetBookList >>= logDebug . pack . show
  logNotice "Bookshelf Demo End"

restartChildTest :: HasCallStack => Endpoint (Broker.Broker (Stateful.Stateful BookShelf)) -> Eff Effects ()
restartChildTest broker =
   OQ.observe @(Broker.ChildEvent (Stateful.Stateful BookShelf)) (100 :: Int) broker $ do
     let c0 = BookShelfId 0
     do
       c00 <- Broker.spawnOrLookup broker c0
       call c00 (AddBook "Solaris")
       OQ.await @(Broker.ChildEvent (Stateful.Stateful BookShelf)) >>= logNotice . pack . show
       handleInterrupts (const (pure ())) (call c00 (AddBook "Solaris")) -- adding twice the same book causes a crash
       void $ awaitProcessDown (c00 ^. fromEndpoint)
       OQ.await @(Broker.ChildEvent (Stateful.Stateful BookShelf)) >>= logNotice . pack . show -- child ended
       logNotice "part 1 passed"
       OQ.await @(Broker.ChildEvent (Stateful.Stateful BookShelf)) >>= logNotice . pack . show -- child restarted
       logNotice "part 2 passed"
     do
       c01m <- Broker.lookupChild broker c0
       case c01m of
        Nothing ->
          lift (assertFailure "failed to lookup child, seems it wasn't restarted!")
        Just c01 -> do
          call c01 (AddBook "Solaris")
          logNotice "part 3 passed"


data BookShelf deriving Typeable

type Book = String

instance HasPdu BookShelf where
  data Pdu BookShelf r where
    GetBookList :: Pdu BookShelf ('Synchronous [Book])
    AddBook :: Book -> Pdu BookShelf ('Synchronous ())
    TakeBook :: Book -> Pdu BookShelf 'Asynchronous
      deriving Typeable

instance Stateful.Server BookShelf Effects where
  newtype StartArgument BookShelf Effects = BookShelfId Int
    deriving (Show, Eq, Ord, Num, NFData, Typeable)

  newtype instance Model BookShelf = BookShelfModel (Set String) deriving (Eq, Show, Default)

  update _ shelfId event = do
    logDebug (pack (show shelfId ++ " " ++ show event))
    case event of
      Stateful.OnCast (TakeBook book) -> do
        booksBefore <- Stateful.getModel @BookShelf
        booksAfter <- Stateful.modifyAndGetModel (over bookShelfModel (Set.delete book))
        when (booksBefore == booksAfter) (exitWithError "book not taken")

      Stateful.OnCall rt callEvent ->
        case callEvent of
          GetBookList -> do
            books <- Stateful.useModel bookShelfModel
            sendReply rt (toList books)

          AddBook book -> do
            booksBefore <- Stateful.getModel @BookShelf
            booksAfter <- Stateful.modifyAndGetModel (over bookShelfModel (Set.insert book))
            when (booksBefore == booksAfter) (exitWithError "book not added")
            sendReply rt ()

      other -> logWarning (pack (show other))

bookShelfModel :: Iso' (Stateful.Model BookShelf) (Set String)
bookShelfModel = iso (\(BookShelfModel s) -> s) BookShelfModel

type instance Broker.ChildId BookShelf = Stateful.StartArgument BookShelf Effects

instance NFData (Pdu BookShelf r) where
  rnf GetBookList = ()
  rnf (AddBook b) = rnf b
  rnf (TakeBook b) = rnf b

instance Show (Pdu BookShelf r) where
  show GetBookList = "get-book-list"
  show (AddBook b) = "add-book: " ++ b
  show (TakeBook b) = "take-book: " ++ b
