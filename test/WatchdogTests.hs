{-# LANGUAGE NumericUnderscores, UndecidableInstances #-}
module WatchdogTests(test_watchdogTests) where

import Common  hiding (runTestCase)
import qualified Common
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import Control.Eff.Concurrent.Protocol.StatefulServer (Stateful)
import qualified Control.Eff.Concurrent.Protocol.Broker as Broker
import Control.Eff.Concurrent.Protocol.Broker (Broker)
import qualified Control.Eff.Concurrent.Protocol.Observer.Queue as OQ
import qualified Control.Eff.Concurrent.Protocol.Watchdog as Watchdog
import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isNothing)


runTestCase :: TestName -> Eff Effects () -> TestTree
runTestCase = Common.runTestCase


test_watchdogTests :: HasCallStack => TestTree
test_watchdogTests =
  testGroup "watchdog"
    [ runTestCase "demonstrate Bookshelf" bookshelfDemo

    , testGroup "watchdog broker interaction"
      [

        runTestCase "test 1: when the broker exits, the watchdog does not care, it can be re-attached to a new broker" $ do
          wd <- Watchdog.startLink def
          unlinkProcess (wd ^. fromEndpoint)
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attachTemporary wd broker
            logNotice "attached watchdog"
            assertShutdown (broker ^. fromEndpoint) ExitNormally
            logNotice "stopped broker"

          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started new broker"
            Watchdog.attachTemporary wd broker
            logNotice "attached broker"
            let expected = ExitUnhandledError "test-broker-kill"
            logNotice "crashing broker"
            assertShutdown (broker ^. fromEndpoint) expected
            logNotice "crashed broker"

          logNotice "shutting down watchdog"
          assertShutdown (wd^.fromEndpoint) ExitNormally
          logNotice "watchdog down"


      , runTestCase "test 2: when the broker exits, and the watchdog is linked to it, it will exit" $ do
          wd <- Watchdog.startLink def
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attachPermanent wd broker
            logNotice "attached and linked broker"
            logNotice "crashing broker"
            let expected = ExitUnhandledError "test-broker-kill"
            sendShutdown (broker ^. fromEndpoint) expected
            logNotice "crashed broker"
            logNotice "waiting for watchdog to crash"
            awaitProcessDown (wd^.fromEndpoint)
              >>= lift
              . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (broker^.fromEndpoint))
              . downReason
            logNotice "watchdog crashed"

      , runTestCase "test 3: when the same broker is attached twice, the second attachment is ignored" $ do
          wd <- Watchdog.startLink def
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attachTemporary wd broker
            logNotice "attached broker"
            Watchdog.attachTemporary wd broker
            logNotice "attached broker"
            logNotice "restart child test begin"
            restartChildTest broker
            logNotice "restart child test finished"

      , runTestCase "test 4: when the same broker is attached twice, the\
                     \ first linked then not linked, the watchdog is not linked anymore" $ do
          wd <- Watchdog.startLink def
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attachPermanent wd broker
            logNotice "attached and linked broker"
            Watchdog.attachTemporary wd broker
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
            Watchdog.attachTemporary wd broker
            logNotice "attached new broker"
            logNotice "run restart child test again to show everything is ok"
            restartChildTest broker
            logNotice "restart child test finished"


      , runTestCase "test 5: when the same broker is attached twice,\
                    \ the first time not linked but then linked, the watchdog is linked \
                    \ to that broker" $ do
          wd <- Watchdog.startLink def
          unlinkProcess (wd ^. fromEndpoint)
          mwd <- monitor (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker ^. fromEndpoint)
            logNotice "started broker"
            Watchdog.attachTemporary wd broker
            logNotice "attached broker"
            Watchdog.attachPermanent wd broker
            logNotice "attached and linked broker"
            logNotice "run restart child test"
            restartChildTest broker
            logNotice "restart child test finished"
            logNotice "crashing broker"
            let expected = ExitUnhandledError "test-broker-kill"
            assertShutdown (broker ^. fromEndpoint) expected
            logNotice "crashed broker"
            logNotice "waiting for watchdog to crash"
            receiveSelectedMessage (selectProcessDown mwd)
              >>= lift
              . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (broker^.fromEndpoint))
              . downReason
            logNotice "watchdog down"


      , runTestCase "test 6: when multiple brokers are attached to a watchdog,\
                     \ and a linked broker exits, the watchdog should exit" $ do
          wd <- Watchdog.startLink def
          unlinkProcess (wd ^. fromEndpoint)
          wdMon <- monitor (wd ^. fromEndpoint)
          logNotice "started watchdog"
          do
            broker1 <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (broker1^.fromEndpoint)
            logNotice "started broker 1"
            Watchdog.attachPermanent wd broker1
            logNotice "attached + linked broker 1"

            broker2 <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            logNotice "started broker 2"
            Watchdog.attachTemporary wd broker2
            logNotice "attached broker 2"

            broker3 <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            logNotice "started broker 3"
            Watchdog.attachTemporary wd broker3
            logNotice "attached broker 3"
            unlinkProcess (broker3 ^. fromEndpoint)

            let expected = ExitUnhandledError "test-broker-kill 1"
            logNotice "crashing broker 1"
            assertShutdown (broker1 ^. fromEndpoint) expected
            logNotice "crashed broker 1"

            isProcessAlive (broker2 ^. fromEndpoint)
              >>= lift . assertBool "broker2 should be running"
            isProcessAlive (broker3 ^. fromEndpoint)
              >>= lift . assertBool "broker3 should be running"
            logNotice "other brokers are alive"

            logNotice "waiting for watchdog to crash"
            receiveSelectedMessage (selectProcessDown wdMon)
                >>= lift
                . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (broker1^.fromEndpoint))
                . downReason
            logNotice "watchdog crashed"

      ]
    , testGroup "restarting children"
      [ runTestCase "test 7: when a child exits it is restarted" $ do
          broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
          logNotice "started broker"
          wd <- Watchdog.startLink def
          logNotice "started watchdog"
          Watchdog.attachTemporary wd broker
          logNotice "attached broker"
          logNotice "running child tests"
          restartChildTest broker
          logNotice "finished child tests"

      , runTestCase "test 8: when the broker emits the shutting down\
                    \ event the watchdog does not restart children" $ do
          broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
          unlinkProcess (broker ^. fromEndpoint)
          logNotice "started broker"
          wd <- Watchdog.startLink def
          unlinkProcess (wd ^. fromEndpoint)
          logNotice "started watchdog"
          Watchdog.attachTemporary wd broker
          OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) broker $ do
            let c0 = BookShelfId 0
            do
              void $ Broker.spawnOrLookup broker c0
              OQ.await @(Broker.ChildEvent (Stateful BookShelf)) >>= logNotice . pack . show
              logNotice "bookshelf started"

              Broker.stopBroker broker
              OQ.await @(Broker.ChildEvent (Stateful BookShelf)) >>= logNotice . pack . show -- broker shutting down event
              void $ awaitProcessDown (broker ^. fromEndpoint)
              logNotice "broker stopped"

          awaitProcessDown (wd ^. fromEndpoint) >>= logNotice . pack . show
          logNotice "watchdog stopped"

      , runTestCase "test 9: a child is only restarted if it crashes" $ do
          broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
          logNotice "started broker"
          wd <- Watchdog.startLink def
          logNotice "started watchdog"
          Watchdog.attachTemporary wd broker
          OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) broker $ do
            let shelfId = BookShelfId 0
            do
              shelf <- Broker.spawnOrLookup broker shelfId
              OQ.await @(Broker.ChildEvent (Stateful BookShelf)) >>= logNotice . pack . show
              logNotice "bookshelf started"

              cast shelf StopShelf
              OQ.await @(Broker.ChildEvent (Stateful BookShelf)) >>= logNotice . pack . show -- broker shutting down event
              void $ awaitProcessDown (shelf ^. fromEndpoint)
              logNotice "shelf stopped"
              lift (threadDelay 100000)
              Broker.lookupChild broker shelfId >>= lift . assertBool "child must not be restarted" . isNothing

          assertShutdown (wd ^. fromEndpoint) ExitNormally
          logNotice "watchdog stopped"

      , testGroup "a child is only restarted only 3 times within a 1 second"
        $ let threeTimesASecond = 3 `Watchdog.crashesPerSeconds` 1
          in  [ runTestCase "test 10: if a child crashes 3 times in 300ms, waits 1.1 seconds\
                            \ and crashes again 3 times, and is restarted three times" $ do

                  broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                  logNotice "started broker"
                  wd <- Watchdog.startLink threeTimesASecond
                  logNotice "started watchdog"
                  Watchdog.attachTemporary wd broker

                  OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) broker $ do
                    let crash3Times = do
                          logNotice "crashing 3 times"
                          replicateM_ 3 $ do
                            spawnAndCrashBookShelf broker shelfId
                            logNotice "waiting for last restart"
                            awaitChildStartedEvent shelfId
                            lift (threadDelay 100_000)
                        shelfId = BookShelfId 0

                    crash3Times
                    logNotice "sleeping"
                    lift (threadDelay 1_100_000)
                    logNotice "crashing again"
                    crash3Times

                  assertShutdown (wd ^. fromEndpoint) ExitNormally
                  logNotice "watchdog stopped"

              , runTestCase "test 11: if a child crashes 4 times within 1s it is not restarted\
                            \ and the watchdog exits with an error and stop all supervisors" $ do
                  broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                  logNotice "started broker"
                  wd <- Watchdog.startLink threeTimesASecond
                  logNotice "started watchdog"
                  unlinkProcess (wd ^. fromEndpoint)
                  Watchdog.attachTemporary wd broker

                  OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) broker $ do
                    let shelfId = BookShelfId 0
                        crash3Times = do
                          logNotice "crashing 3 times"
                          replicateM_ 3 $ do
                            spawnAndCrashBookShelf broker shelfId
                            logNotice "waiting for restart"
                            awaitChildStartedEvent shelfId
                            lift (threadDelay 100_000)

                    crash3Times

                    logNotice "crashing 4. time"
                    spawnAndCrashBookShelf broker shelfId
                    lift (threadDelay 10_000)
                    Broker.lookupChild broker shelfId
                      >>= lift . assertBool "child must not be reststarted" . isNothing

                  assertShutdown (wd ^. fromEndpoint) ExitNormally
                  logNotice "watchdog stopped"

              , runTestCase "test 12: if a child of a linked broker crashes too often,\
                            \ the watchdog exits with an error and interrupts the broker" $ do

                  broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                  logNotice "started broker"
                  unlinkProcess (broker ^. fromEndpoint)
                  mBroker <- monitor (broker ^. fromEndpoint)
                  wd <- Watchdog.startLink threeTimesASecond
                  logNotice "started watchdog"
                  mwd <- monitor (wd ^. fromEndpoint)
                  unlinkProcess (wd ^. fromEndpoint)
                  Watchdog.attachPermanent wd broker

                  OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) broker $ do
                    let shelfId = BookShelfId 0
                        crash3Times = do
                          logNotice "crashing 3 times"
                          replicateM_ 3 $ do
                            spawnAndCrashBookShelf broker shelfId
                            logNotice "waiting for restart"
                            awaitChildStartedEvent shelfId
                            lift (threadDelay 100_000)

                    crash3Times

                    logNotice "crashing 4. time"
                    spawnAndCrashBookShelf broker shelfId

                  logNotice "await broker down"
                  receiveSelectedMessage (selectProcessDown mBroker)
                    >>= lift
                        . assertEqual
                              "wrong exit reason"
                              (ExitUnhandledError "restart frequency exceeded")
                        . downReason
                  logNotice "broker down"

                  logNotice "await watchdog down"
                  receiveSelectedMessage (selectProcessDown mwd)
                    >>= lift
                        . assertEqual
                              "wrong exit reason"
                              (ExitUnhandledError "restart frequency exceeded")
                        . downReason
                  logNotice "watchdog down"

              ]
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

restartChildTest :: HasCallStack => Endpoint (Broker (Stateful BookShelf)) -> Eff Effects ()
restartChildTest broker =
  OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) broker $ do
    let c0 = BookShelfId 123
    spawnAndCrashBookShelf broker c0
    c01m <- Broker.lookupChild broker c0
    case c01m of
      Nothing ->
        lift (assertFailure "failed to lookup child, seems it wasn't restarted!")
      Just c01 -> do
        call c01 (AddBook "Solaris")
        logNotice "part 3 passed"

spawnAndCrashBookShelf
  :: HasCallStack
  => Endpoint (Broker (Stateful BookShelf))
  -> Broker.ChildId (Stateful BookShelf)
  -> Eff (OQ.Reader (Broker.ChildEvent (Stateful BookShelf)) : Effects) ()
spawnAndCrashBookShelf broker c0 = do
   logNotice "spawn or lookup book shelf"
   res <- Broker.spawnChild broker c0
   c00 <- case res of
     Left (Broker.AlreadyStarted _ ep) -> pure ep
     Right ep -> awaitChildStartedEvent c0 >> pure ep

   logNotice ("got book shelf: " <> pack (show c00))
   logNotice "adding Solaris"
   call c00 (AddBook "Solaris")
   logNotice "added Solaris"
   logNotice "adding Solaris again to crash the book shelf"
   handleInterrupts (const (pure ())) (call c00 (AddBook "Solaris")) -- adding twice the same book causes a crash
   logNotice "added Solaris again waiting for crash"
   void $ awaitProcessDown (c00 ^. fromEndpoint)
   logNotice "shelf crashed"
   awaitChildDownEvent c0

awaitChildDownEvent c0 = do
   logNotice ("waiting for down event of " <> pack (show c0))
   evt <- OQ.await @(Broker.ChildEvent (Stateful BookShelf))
   case evt of
    (Broker.OnChildDown _ c' _ _) | c0 == c' ->
      logNotice ("child down: " <> pack (show c0))
    otherEvent ->
      lift (assertFailure ("wrong broker down event received: " <> show otherEvent))

awaitChildStartedEvent c0 = do
   logNotice ("waiting for start event of " <> pack (show c0))
   evt <- OQ.await @(Broker.ChildEvent (Stateful BookShelf))
   case evt of
    (Broker.OnChildSpawned _ c' _) | c0 == c' ->
      logNotice ("child started: " <> pack (show c0))
    otherEvent ->
      lift (assertFailure ("wrong broker start event received: " <> show otherEvent))


data BookShelf deriving Typeable

type Book = String

instance HasPdu BookShelf where
  data Pdu BookShelf r where
    GetBookList :: Pdu BookShelf ('Synchronous [Book])
    AddBook :: Book -> Pdu BookShelf ('Synchronous ())
    TakeBook :: Book -> Pdu BookShelf 'Asynchronous
    StopShelf :: Pdu BookShelf 'Asynchronous
      deriving Typeable

instance Stateful.Server BookShelf Effects where
  newtype StartArgument BookShelf Effects = BookShelfId Int
    deriving (Show, Eq, Ord, Num, NFData, Typeable)

  newtype instance Model BookShelf = BookShelfModel (Set String) deriving (Eq, Show, Default)

  update _ shelfId event = do
    logDebug (pack (show shelfId ++ " " ++ show event))
    case event of

      Stateful.OnCast StopShelf -> do
        logNotice "stop shelf"
        exitNormally

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
  rnf StopShelf = ()
  rnf (AddBook b) = rnf b
  rnf (TakeBook b) = rnf b

instance Show (Pdu BookShelf r) where
  show GetBookList = "get-book-list"
  show StopShelf = "stop-shelf"
  show (AddBook b) = "add-book: " ++ b
  show (TakeBook b) = "take-book: " ++ b
