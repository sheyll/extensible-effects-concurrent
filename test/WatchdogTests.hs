{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoOverloadedStrings #-}

module WatchdogTests
  ( test_watchdogTests,
  )
where

import Common hiding (runTestCase)
import qualified Common
import Control.Eff.Concurrent.Protocol.Broker (Broker)
import qualified Control.Eff.Concurrent.Protocol.Broker as Broker
import qualified Control.Eff.Concurrent.Protocol.Observer.Queue as OQ
import Control.Eff.Concurrent.Protocol.StatefulServer (Stateful)
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import qualified Control.Eff.Concurrent.Protocol.Watchdog as Watchdog
import Control.Lens
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String

runTestCase :: TestName -> Eff Effects () -> TestTree
runTestCase = Common.runTestCase

test_watchdogTests :: HasCallStack => TestTree
test_watchdogTests =
  testGroup
    "watchdog"
    [ runTestCase "demonstrate Bookshelf" bookshelfDemo,
      testGroup
        "watchdog broker interaction"
        [ runTestCase "test 1: when the broker exits, the watchdog does not care, it can be re-attached to a new broker" $ do
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
              let expected = ExitUnhandledError (fromString "test-broker-kill")
              logNotice "crashing broker"
              assertShutdown (broker ^. fromEndpoint) expected
              logNotice "crashed broker"
            logNotice "shutting down watchdog"
            assertShutdown (wd ^. fromEndpoint) ExitNormally
            logNotice "watchdog down",
          runTestCase "test 2: when the broker exits, and the watchdog is linked to it, it will exit" $ do
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
              let expected = ExitUnhandledError (fromString "test-broker-kill")
              sendShutdown (broker ^. fromEndpoint) expected
              logNotice "crashed broker"
              logNotice "waiting for watchdog to crash"
              awaitProcessDown (wd ^. fromEndpoint)
                >>= lift
                  . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (broker ^. fromEndpoint))
                  . downReason
              logNotice "watchdog crashed",
          runTestCase "test 3: when the same broker is attached twice, the second attachment is ignored" $ do
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
              logNotice "restart child test finished",
          runTestCase
            "test 4: when the same broker is attached twice, the\
            \ first linked then not linked, the watchdog is not linked anymore"
            $ do
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
                let expected = ExitUnhandledError (fromString "test-broker-kill")
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
                logNotice "restart child test finished",
          runTestCase
            "test 5: when the same broker is attached twice,\
            \ the first time not linked but then linked, the watchdog is linked \
            \ to that broker"
            $ do
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
                let expected = ExitUnhandledError (fromString "test-broker-kill")
                assertShutdown (broker ^. fromEndpoint) expected
                logNotice "crashed broker"
                logNotice "waiting for watchdog to crash"
                receiveSelectedMessage (selectProcessDown mwd)
                  >>= lift
                    . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (broker ^. fromEndpoint))
                    . downReason
                logNotice "watchdog down",
          runTestCase
            "test 6: when multiple brokers are attached to a watchdog,\
            \ and a linked broker exits, the watchdog should exit"
            $ do
              wd <- Watchdog.startLink def
              unlinkProcess (wd ^. fromEndpoint)
              wdMon <- monitor (wd ^. fromEndpoint)
              logNotice "started watchdog"
              do
                broker1 <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                unlinkProcess (broker1 ^. fromEndpoint)
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
                let expected = ExitUnhandledError (fromString "test-broker-kill 1")
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
                    . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (broker1 ^. fromEndpoint))
                    . downReason
                logNotice "watchdog crashed"
        ],
      testGroup
        "restarting children"
        [ runTestCase "test 7: when a child exits it is restarted" $ do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            logNotice "started broker"
            wd <- Watchdog.startLink def
            logNotice "started watchdog"
            Watchdog.attachTemporary wd broker
            logNotice "attached broker"
            logNotice "running child tests"
            restartChildTest broker
            logNotice "finished child tests",
          runTestCase
            "test 8: when the broker emits the shutting down\
            \ event the watchdog does not restart children"
            $ do
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
                  awaitChildStartedEvent c0
                  logNotice "bookshelf started"
                  Broker.stopBroker broker
                  awaitBrokershuttingDownEvent broker
                  lift (threadDelay 10_000)
                  logNotice "checking state"
                  Watchdog.getCrashReports wd >>= lift . assertBool "no crash reports expected" . null
              assertShutdown (wd ^. fromEndpoint) ExitNormally
              logNotice "watchdog stopped",
          runTestCase "test 9: a child is only restarted if it crashes" $ do
            broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            logNotice "started broker"
            wd <- Watchdog.startLink def
            logNotice "started watchdog"
            Watchdog.attachTemporary wd broker
            OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) broker $ do
              let shelfId = BookShelfId 0
              do
                shelf <- Broker.spawnOrLookup broker shelfId
                awaitChildStartedEvent shelfId
                logNotice "bookshelf started"
                cast shelf StopShelf
                awaitChildDownEvent shelfId
                void $ awaitProcessDown (shelf ^. fromEndpoint)
                logNotice "shelf stopped"
                lift (threadDelay 10_000)
                Broker.lookupChild broker shelfId >>= lift . assertBool "child must not be restarted" . isNothing
            assertShutdown (wd ^. fromEndpoint) ExitNormally
            logNotice "watchdog stopped",
          testGroup "child restart" $
            let threeTimesASecond = 3 `Watchdog.crashesPerSeconds` 1
             in [ runTestCase
                    "test 10: if a child crashes 3 times in 300ms, waits 1.1 seconds\
                    \ and crashes again 3 times, and is restarted three times"
                    $ do
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
                      logNotice "watchdog stopped",
                  runTestCase
                    "test 11: if a child crashes 4 times within 1s it is not restarted\
                    \ and the watchdog exits with an error"
                    $ do
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
                                cr <- Watchdog.getCrashReports @(Stateful BookShelf) wd
                                traverse_ (logNotice . LABEL "crash-reports") cr
                                lift (threadDelay 100_000)
                        crash3Times
                        logNotice "crashing 4. time"
                        spawnAndCrashBookShelf broker shelfId
                        lift (threadDelay 10_000)
                        Broker.lookupChild broker shelfId
                          >>= lift . assertBool "child must not be reststarted" . isNothing
                      assertShutdown (wd ^. fromEndpoint) ExitNormally
                      logNotice "watchdog stopped",
                  runTestCase
                    "test 12: if a child of a linked broker crashes too often,\
                    \ the watchdog exits with an error and interrupts the broker"
                    $ do
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
                            (ExitUnhandledError (fromString "restart frequency exceeded"))
                          . downReason
                      logNotice "broker down"
                      logNotice "await watchdog down"
                      receiveSelectedMessage (selectProcessDown mwd)
                        >>= lift
                          . assertEqual
                            "wrong exit reason"
                            (ExitUnhandledError (fromString "restart frequency exceeded"))
                          . downReason
                      logNotice "watchdog down",
                  runTestCase
                    "test 13: if the watchdog is killed, all watched children of all\
                    \ brokers are still alive"
                    $ do
                      wd <- Watchdog.startLink (1 `Watchdog.crashesPerSeconds` 1)
                      unlinkProcess (wd ^. fromEndpoint)
                      logNotice "started watchdog"
                      brokerT <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                      unlinkProcess (brokerT ^. fromEndpoint)
                      logNotice "started temporary broker"
                      brokerP <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                      unlinkProcess (brokerP ^. fromEndpoint)
                      logNotice "started permanent broker"
                      Watchdog.attachTemporary wd brokerT
                      logNotice "attached temporary broker"
                      Watchdog.attachPermanent wd brokerP
                      logNotice "attached permanent broker"
                      let b0 = BookShelfId 0
                          b1 = BookShelfId 1
                          b2 = BookShelfId 2
                          b3 = BookShelfId 3
                      (e0, e1) <-
                        OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) brokerT $
                          (,) <$> spawnBookShelf brokerT b0 <*> spawnBookShelf brokerT b1
                      m0 <- monitor (e0 ^. fromEndpoint)
                      m1 <- monitor (e1 ^. fromEndpoint)
                      logNotice (LABEL "started bookshelfs of temporary broker" (m0, m1))
                      (e2, e3) <-
                        OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) brokerP $
                          (,) <$> spawnBookShelf brokerP b2 <*> spawnBookShelf brokerP b3
                      m2 <- monitor (e2 ^. fromEndpoint)
                      m3 <- monitor (e3 ^. fromEndpoint)
                      logNotice (LABEL "started bookshelfs of permanent broker" (m2, m3))
                      logNotice "crashing the watchdog"
                      assertShutdown (wd ^. fromEndpoint) (ExitUnhandledError (fromString "watchdog test crash"))
                      logNotice "watchdog stopped"
                      call e0 (AddBook "Solaris")
                      logNotice "e0 still alive"
                      call e1 (AddBook "Solaris")
                      logNotice "e1 still alive"
                      call e2 (AddBook "Solaris")
                      logNotice "e2 still alive"
                      call e3 (AddBook "Solaris")
                      logNotice "e3 still alive"
                      assertShutdown (e0 ^. fromEndpoint) ExitNormally
                      assertShutdown (e1 ^. fromEndpoint) ExitNormally
                      assertShutdown (e2 ^. fromEndpoint) ExitNormally
                      assertShutdown (e3 ^. fromEndpoint) ExitNormally
                      assertShutdown (brokerT ^. fromEndpoint) ExitNormally
                      assertShutdown (brokerP ^. fromEndpoint) ExitNormally,
                  runTestCase
                    "test 14: if the watchdog receives OnChildDown\
                    \ for a known broker it behaves as if there\
                    \ was an OnChildSpawned for that child"
                    $ do
                      wd <- Watchdog.startLink (1 `Watchdog.crashesPerSeconds` 1)
                      logNotice "started watchdog"
                      brokerT <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                      logNotice "started temporary broker"
                      OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) brokerT $ do
                        let b0 = BookShelfId 0
                        e0 <- spawnBookShelf brokerT b0
                        logNotice (LABEL "started bookshelf of temporary broker" e0)
                        Watchdog.attachTemporary wd brokerT
                        logNotice "attached temporary broker"
                        logNotice "crash the bookshelf"
                        assertShutdown (e0 ^. fromEndpoint) (ExitUnhandledError (fromString "bookshelf test crash"))
                        awaitChildDownEvent b0
                        logNotice "bookshelf down"
                        awaitChildStartedEvent b0
                        logNotice "bookshelf restarted"
                        assertShutdown (wd ^. fromEndpoint) ExitNormally
                        assertShutdown (brokerT ^. fromEndpoint) ExitNormally,
                  runTestCase
                    "test 15: when a child exits normally,\
                    \ and a new child with the same ChildId\
                    \ crashes, the watchdog behaves as if\
                    \ the first child never existed"
                    $ do
                      wd <- Watchdog.startLink (1 `Watchdog.crashesPerSeconds` 10)
                      logNotice "started watchdog"
                      brokerT <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                      logNotice "started temporary broker"
                      OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) brokerT $ do
                        let b0 = BookShelfId 0
                        e0 <- spawnBookShelf brokerT b0
                        logNotice (LABEL "started bookshelf of temporary broker" e0)
                        Watchdog.attachTemporary wd brokerT
                        logNotice "attached temporary broker"
                        logNotice "crash the bookshelf"
                        assertShutdown (e0 ^. fromEndpoint) (ExitUnhandledError (fromString "bookshelf test crash"))
                        awaitChildDownEvent b0
                        logNotice "bookshelf down"
                        awaitChildStartedEvent b0
                        e1 <- fromJust <$> Broker.lookupChild brokerT b0
                        logNotice "bookshelf restarted"
                        logNotice "exit the bookshelf normally"
                        assertShutdown (e1 ^. fromEndpoint) ExitNormally
                        awaitChildDownEvent b0
                        logNotice "bookshelf stopped, starting a new one"
                        e2 <- spawnBookShelf brokerT b0
                        logNotice "bookshelf restarted"
                        assertShutdown (e2 ^. fromEndpoint) (ExitUnhandledError (fromString "bookshelf test crash"))
                        awaitChildDownEvent b0
                        logNotice "bookshelf down"
                        awaitChildStartedEvent b0
                        logNotice "bookshelf restarted"
                        assertShutdown (wd ^. fromEndpoint) ExitNormally
                        assertShutdown (brokerT ^. fromEndpoint) ExitNormally,
                  testGroup "test 16: the amount of state is kept to a minimum" $
                    let setup k = do
                          wd <- Watchdog.startLink (4 `Watchdog.crashesPerSeconds` 30)
                          logNotice "started watchdog"
                          brokerT <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                          logNotice "started temporary broker"
                          Watchdog.attachTemporary wd brokerT
                          logNotice "attached temporary broker"
                          brokerP <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
                          logNotice "started permanent broker"
                          Watchdog.attachTemporary wd brokerP
                          logNotice "attached permanent broker"
                          do
                            p <- self
                            t <- spawn (fromString "test-proc") $ do
                              void $ k wd brokerT brokerP
                              sendMessage p ()
                            res <- receiveSelectedWithMonitorAfter t selectMessage (TimeoutMicros 10_000_000)
                            logNotice (LABEL "test-proc finish message" res)
                            case res of
                              Left x ->
                                do
                                  logError (LABEL "test-proc crashed" x)
                                  lift (assertFailure ("test-proc crashed: " <> showAsLogMsg x))
                              Right () ->
                                logNotice "test-proc succeeded"
                        setupThenShutdown k =
                          setup $ \wd brokerT brokerP -> do
                            void $ k wd brokerT brokerP
                            assertShutdown (wd ^. fromEndpoint) ExitNormally
                            assertShutdown (brokerT ^. fromEndpoint) ExitNormally
                            assertShutdown (brokerP ^. fromEndpoint) ExitNormally
                     in [ runTestCase "test 17: child events of unknown broker don't add state" $
                            setupThenShutdown $
                              \wd _brokerT _brokerP -> do
                                someBroker <- asEndpoint @(Broker (Stateful BookShelf)) <$> spawn (fromString "someBroker") (pure ())
                                someChild <- asEndpoint @BookShelf <$> spawn (fromString "someChild") (pure ())
                                let someChildId = BookShelfId 2321
                                cast wd (Observed (Broker.OnChildDown someBroker someChildId someChild (ExitUnhandledError (packLogMsg "test error"))))
                                lift (threadDelay 1_000)
                                cr <- Watchdog.getCrashReports @(Stateful BookShelf) wd
                                traverse_ (logNotice . LABEL "crash-reports") cr
                                lift (assertBool "no crash reports expected" (Map.null cr)),
                          runTestCase "test 18: when a broker exits, the children of that broker are forgotten and ignored" $ do
                            setup $ \wd brokerT brokerP -> do
                              let someChildId1 = BookShelfId 2321
                                  someChildId2 = BookShelfId 2322
                                  someChildId3 = BookShelfId 2323
                              (someChild1, someChild2) <-
                                OQ.observe @(Broker.ChildEvent (Stateful BookShelf))
                                  (100 :: Int)
                                  brokerT
                                  ((,) <$> spawnBookShelf brokerT someChildId1 <*> spawnBookShelf brokerT someChildId2)
                              someChild3 <-
                                OQ.observe @(Broker.ChildEvent (Stateful BookShelf))
                                  (100 :: Int)
                                  brokerP
                                  (spawnBookShelf brokerP someChildId3)
                              do
                                cr <- Watchdog.getCrashReports @(Stateful BookShelf) wd
                                traverse_ (logNotice . LABEL "crash-report") cr
                                lift (assertBool "no crash reports expected" (Map.null cr))
                              OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) brokerT $ do
                                crashBookShelf someChild1 someChildId1
                                awaitChildStartedEvent someChildId1
                                crashBookShelf someChild2 someChildId2
                                awaitChildStartedEvent someChildId2
                              OQ.observe @(Broker.ChildEvent (Stateful BookShelf)) (100 :: Int) brokerP $ do
                                crashBookShelf someChild3 someChildId3
                                awaitChildStartedEvent someChildId3
                              do
                                cr <- Watchdog.getCrashReports @(Stateful BookShelf) wd
                                traverse_ (logNotice . LABEL "crash-report") cr
                                lift (assertEqual "wrong number of crash reports" 3 (Map.size cr))
                              assertShutdown (brokerT ^. fromEndpoint) ExitNormally
                              do
                                cr <- Watchdog.getCrashReports @(Stateful BookShelf) wd
                                traverse_ (logNotice . LABEL "crash-report") cr
                                lift (assertEqual "wrong number of crash reports" 1 (Map.size cr))
                              assertShutdown (wd ^. fromEndpoint) ExitNormally
                              assertShutdown (brokerP ^. fromEndpoint) ExitNormally
                        ]
                ]
        ]
    ]

bookshelfDemo :: HasCallStack => Eff Effects ()
bookshelfDemo = do
  logNotice "Bookshelf Demo Begin"
  broker <- Broker.startLink (Broker.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
  shelf1 <- Broker.spawnOrLookup broker (BookShelfId 1)
  call shelf1 (AddBook "Solaris")
  call shelf1 GetBookList >>= mapM_ logDebug
  cast shelf1 (TakeBook "Solaris")
  call shelf1 GetBookList >>= mapM_ logDebug
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

spawnBookShelf ::
  HasCallStack =>
  Endpoint (Broker (Stateful BookShelf)) ->
  Broker.ChildId (Stateful BookShelf) ->
  Eff (OQ.Reader (Broker.ChildEvent (Stateful BookShelf)) : Effects) (Endpoint BookShelf)
spawnBookShelf broker c0 = do
  logNotice "spawn or lookup book shelf"
  res <- Broker.spawnChild broker c0
  c00 <- case res of
    Left (Broker.AlreadyStarted _ ep) -> pure ep
    Right ep -> awaitChildStartedEvent c0 >> pure ep
  logNotice (LABEL "got book shelf" c00)
  pure c00

spawnAndCrashBookShelf ::
  HasCallStack =>
  Endpoint (Broker (Stateful BookShelf)) ->
  Broker.ChildId (Stateful BookShelf) ->
  Eff (OQ.Reader (Broker.ChildEvent (Stateful BookShelf)) : Effects) ()
spawnAndCrashBookShelf broker c0 = do
  c00 <- spawnBookShelf broker c0
  crashBookShelf c00 c0

crashBookShelf ::
  HasCallStack =>
  Endpoint BookShelf ->
  Broker.ChildId (Stateful BookShelf) ->
  Eff (OQ.Reader (Broker.ChildEvent (Stateful BookShelf)) : Effects) ()
crashBookShelf c00 c0 = do
  logNotice "adding Solaris"
  call c00 (AddBook "Solaris")
  logNotice "added Solaris"
  logNotice "adding Solaris again to crash the book shelf"
  handleInterrupts (const (pure ())) (call c00 (AddBook "Solaris")) -- adding twice the same book causes a crash
  logNotice "added Solaris again waiting for crash"
  void $ awaitProcessDown (c00 ^. fromEndpoint)
  logNotice "shelf crashed"
  awaitChildDownEvent c0

awaitBrokershuttingDownEvent broker = do
  logNotice (LABEL "waiting for broker shutting down event of" broker)
  evt <- OQ.await @(Broker.ChildEvent (Stateful BookShelf))
  case evt of
    x@(Broker.OnBrokerShuttingDown broker')
      | broker == broker' ->
        logNotice x
    otherEvent ->
      lift (assertFailure ("wrong event received: " <> showAsLogMsg otherEvent))

awaitChildDownEvent c0 = do
  logNotice (LABEL "waiting for down event of" c0)
  evt <- OQ.await @(Broker.ChildEvent (Stateful BookShelf))
  case evt of
    x@(Broker.OnChildDown _ c' _ _)
      | c0 == c' ->
        logNotice x
    otherEvent ->
      lift (assertFailure ("wrong broker down event received: " <> showAsLogMsg otherEvent))

awaitChildStartedEvent c0 = do
  logNotice (LABEL "waiting for start event of" c0)
  evt <- OQ.await @(Broker.ChildEvent (Stateful BookShelf))
  case evt of
    x@(Broker.OnChildSpawned _ c' _)
      | c0 == c' ->
        logNotice x
    otherEvent ->
      lift (assertFailure ("wrong broker start event received: " <> showAsLogMsg otherEvent))

data BookShelf deriving (Typeable)

type Book = String

instance HasPdu BookShelf where
  data Pdu BookShelf r where
    GetBookList :: Pdu BookShelf ('Synchronous [Book])
    AddBook :: Book -> Pdu BookShelf ('Synchronous ())
    TakeBook :: Book -> Pdu BookShelf 'Asynchronous
    StopShelf :: Pdu BookShelf 'Asynchronous
    deriving (Typeable)

instance Stateful.Server BookShelf Effects where
  newtype StartArgument BookShelf = BookShelfId Int
    deriving (Show, Eq, Ord, Num, NFData, Typeable)

  newtype Model BookShelf = BookShelfModel (Set String) deriving (Eq, Show, Default)

  update _ shelfId event = do
    logDebug shelfId event
    case event of
      Stateful.OnCast StopShelf -> do
        logNotice (MSG "stop shelf")
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
      other -> logWarning other

bookShelfModel :: Iso' (Stateful.Model BookShelf) (Set String)
bookShelfModel = iso (\(BookShelfModel s) -> s) BookShelfModel

instance ToLogMsg (Stateful.StartArgument BookShelf) where
  toLogMsg (BookShelfId bId) = packLogMsg "BookShelf" <> toLogMsg bId

type instance Broker.ChildId BookShelf = Stateful.StartArgument BookShelf

instance NFData (Pdu BookShelf r) where
  rnf GetBookList = ()
  rnf StopShelf = ()
  rnf (AddBook b) = rnf b
  rnf (TakeBook b) = rnf b

instance ToLogMsg (Pdu BookShelf r) where
  toLogMsg GetBookList = packLogMsg "get-book-list"
  toLogMsg StopShelf = packLogMsg "stop-shelf"
  toLogMsg (AddBook b) = packLogMsg "add-book: " <> toLogMsg b
  toLogMsg (TakeBook b) = packLogMsg "take-book: " <> toLogMsg b

instance ToTypeLogMsg BookShelf where
  toTypeLogMsg _ = fromString "BookShelf"
