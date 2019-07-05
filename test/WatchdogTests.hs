{-# LANGUAGE NumericUnderscores, UndecidableInstances #-}
module WatchdogTests(test_watchdogTests) where

import Common
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import qualified Control.Eff.Concurrent.Protocol.Supervisor as Supervisor
import qualified Control.Eff.Concurrent.Protocol.Observer.Queue as OQ
import qualified Control.Eff.Concurrent.Protocol.Watchdog as Watchdog
import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio
import Data.Fixed
import Data.Time.Clock

test_watchdogTests :: HasCallStack => TestTree
test_watchdogTests =
  testGroup "watchdog"
    [ runTestCase "demonstrate Bookshelf" bookshelfDemo

    , testGroup "watchdog supervisor interaction"
      [

        runTestCase "when the supervisor exits, the watchdog does not care, it can be re-attached to a new supervisor" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          do
            sup <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            Watchdog.attach wd sup
            unlinkProcess (sup ^. fromEndpoint)
            let expected = ExitUnhandledError "test-supervisor-kill"
            Supervisor.stopSupervisor sup
            awaitProcessDown (sup ^. fromEndpoint) >>= lift . assertEqual "bad exit reason" expected . downReason

          do
            sup <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            Watchdog.attach wd sup
            unlinkProcess (sup ^. fromEndpoint)
            let expected = ExitUnhandledError "test-supervisor-kill"
            sendShutdown (sup ^. fromEndpoint) expected
            awaitProcessDown (sup ^. fromEndpoint) >>= lift . assertEqual "bad exit reason" expected . downReason

          sendShutdown (wd^.fromEndpoint) ExitNormally
          awaitProcessDown (wd^.fromEndpoint) >>= lift . assertEqual "bad exit reason" ExitNormally . downReason


      , runTestCase "when the same supervisor is attached twice, the second attachment is ignored" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          do
            sup <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            unlinkProcess (sup ^. fromEndpoint)
            Watchdog.attach wd sup
            Watchdog.attach wd sup
            restartChildTest sup
            let expected = ExitUnhandledError "test-supervisor-kill"
            sendShutdown (sup ^. fromEndpoint) expected
          awaitProcessDown (wd^.fromEndpoint) >>= lift . assertEqual "bad exit reason" ExitNormally . downReason


      , runTestCase "when the same supervisor is attached twice, the first linked then not linked, the watchdog is not linked anymore" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          do
            sup <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            Watchdog.attachLinked wd sup
            unlinkProcess (sup ^. fromEndpoint)
            let expected = ExitUnhandledError "test-supervisor-kill"
            sendShutdown (sup ^. fromEndpoint) expected
          awaitProcessDown (wd^.fromEndpoint) >>= lift . assertEqual "bad exit reason" ExitNormally . downReason


      , runTestCase "when the same supervisor is attached twice, the first time not linked but then linked, the watchdog is linked" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          do
            sup <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            Watchdog.attachLinked wd sup
            unlinkProcess (sup ^. fromEndpoint)
            let expected = ExitUnhandledError "test-supervisor-kill"
            sendShutdown (sup ^. fromEndpoint) expected
          awaitProcessDown (wd^.fromEndpoint) >>= lift . assertEqual "bad exit reason" ExitNormally . downReason


      , runTestCase "when the supervisor exits, an the watchdog is linked to it, it will exit" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          do
            sup <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            Watchdog.attachLinked wd sup
            unlinkProcess (sup ^. fromEndpoint)
            let expected = ExitUnhandledError "test-supervisor-kill"
            sendShutdown (sup ^. fromEndpoint) expected
          awaitProcessDown (wd^.fromEndpoint) >>= lift . assertEqual "bad exit reason" ExitNormally . downReason


      , runTestCase "when multiple supervisors are attached to a watchdog, and a linked supervisor exits, the watchdog should exit" $ do
          wd <- Watchdog.startLink
          unlinkProcess (wd ^. fromEndpoint)
          do
            sup1 <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            Watchdog.attachLinked wd sup1

            sup2 <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            Watchdog.attach wd sup2

            sup3 <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
            Watchdog.attach wd sup3
            unlinkProcess (sup3 ^. fromEndpoint)
            let expected = ExitUnhandledError "test-supervisor-kill 3"

            sendShutdown (sup1 ^. fromEndpoint) expected

            awaitProcessDown (wd^.fromEndpoint) >>= lift . assertEqual "bad exit reason" (ExitOtherProcessNotRunning (sup1^.fromEndpoint)) . downReason

            isProcessAlive (sup2 ^. fromEndpoint) >>= lift . assertBool "sup2 should be running"
            isProcessAlive (sup3 ^. fromEndpoint) >>= lift . assertBool "sup3 should be running"

      ]
    , testGroup "restarting children"
      [ runTestCase "when a child exits it is restarted" $ do
           sup <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
           wd <- Watchdog.startLink
           Watchdog.attach wd sup
           restartChildTest sup

      , runTestCase "when the supervisor emits the shutting down event the watchdog does not restart children" $ do
           sup <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
           unlinkProcess (sup ^. fromEndpoint)
           wd <- Watchdog.startLink
           unlinkProcess (wd ^. fromEndpoint)
           Watchdog.attach wd sup
           OQ.observe @(Supervisor.ChildEvent (Stateful.Stateful BookShelf)) (100 :: Int) sup $ do
             let c0 = BookShelfId 0
             do
               void $ Supervisor.spawnOrLookup sup c0
               OQ.await @(Supervisor.ChildEvent (Stateful.Stateful BookShelf)) >>= logInfo . pack . show
               logInfo "bookshelf started"

               Supervisor.stopSupervisor sup
               OQ.await @(Supervisor.ChildEvent (Stateful.Stateful BookShelf)) >>= logInfo . pack . show -- super visor shutting down event
               void $ awaitProcessDown (sup ^. fromEndpoint)
               logInfo "supervisor stopped"

           awaitProcessDown (wd ^. fromEndpoint) >>= logInfo . pack . show
           logInfo "watchdog stopped"

      , testGroup "reusing ChildIds"
          []
--        [ runTestCase "when a child exits normally, and a new child with the same ChildId crashes, the watchdog behaves as if the first child never existed" $ do
--            error "TODO"
--        ]
      ]
    ]
bookshelfDemo :: HasCallStack => Eff Effects ()
bookshelfDemo = do
  logInfo "Bookshelf Demo Begin"
  sup <- Supervisor.startLink (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
  shelf1 <- Supervisor.spawnOrLookup sup (BookShelfId 1)
  call shelf1 (AddBook "Solaris")
  call shelf1 GetBookList >>= logDebug . pack . show
  cast shelf1 (TakeBook "Solaris")
  call shelf1 GetBookList >>= logDebug . pack . show
  logInfo "Bookshelf Demo End"

restartChildTest :: HasCallStack => Endpoint (Supervisor.Sup (Stateful.Stateful BookShelf)) -> Eff Effects ()
restartChildTest sup =
   OQ.observe @(Supervisor.ChildEvent (Stateful.Stateful BookShelf)) (100 :: Int) sup $ do
     let c0 = BookShelfId 0
     do
       c00 <- Supervisor.spawnOrLookup sup c0
       call c00 (AddBook "Solaris")
       OQ.await @(Supervisor.ChildEvent (Stateful.Stateful BookShelf)) >>= logInfo . pack . show
       call c00 (AddBook "Solaris") -- adding twice the same book causes a crash
       void $ awaitProcessDown (c00 ^. fromEndpoint)
       OQ.await @(Supervisor.ChildEvent (Stateful.Stateful BookShelf)) >>= logInfo . pack . show -- child ended
       logInfo "part 1 passed"
       OQ.await @(Supervisor.ChildEvent (Stateful.Stateful BookShelf)) >>= logInfo . pack . show -- child restarted
       logInfo "part 2 passed"
     do
       c01m <- Supervisor.lookupChild sup c0
       case c01m of
        Nothing ->
          lift (assertFailure "failed to lookup child, seems it wasn't restarted!")
        Just c01 -> do
          call c01 (AddBook "Solaris")
          logInfo "part 3 passed"


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

type instance Supervisor.ChildId BookShelf = Stateful.StartArgument BookShelf Effects

instance NFData (Pdu BookShelf r) where
  rnf GetBookList = ()
  rnf (AddBook b) = rnf b
  rnf (TakeBook b) = rnf b

instance Show (Pdu BookShelf r) where
  show GetBookList = "get-book-list"
  show (AddBook b) = "add-book: " ++ b
  show (TakeBook b) = "take-book: " ++ b
