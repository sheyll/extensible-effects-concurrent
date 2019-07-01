{-# LANGUAGE NumericUnderscores, UndecidableInstances #-}
module WatchdogTests(test_watchdogTests) where

import Common
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import qualified Control.Eff.Concurrent.Protocol.Supervisor as Supervisor
import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio
import Data.Fixed
import Data.Time.Clock

test_watchdogTests :: TestTree
test_watchdogTests =
  testGroup "watchdog"
    [ runTestCase "demonstrate Bookshelf" bookshelfDemo
    , runTestCase "demonstrate Bookshelf WITH a simple watchdog" bookshelfWatchdogDemo
    ]

bookshelfDemo :: Eff Effects ()
bookshelfDemo = do
  logInfo "Bookshelf Demo Begin"
  sup <- Supervisor.startSupervisor (Supervisor.statefulChild @BookShelf (TimeoutMicros 1_000_000) id)
  shelf1 <- Supervisor.spawnOrLookup sup (BookShelfId 1)
  call shelf1 (AddBook "Solaris")
  call shelf1 GetBookList >>= logDebug . pack . show
  cast shelf1 (TakeBook "Solaris")
  call shelf1 GetBookList >>= logDebug . pack . show
  logInfo "Bookshelf Demo End"

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





bookshelfWatchdogDemo :: Eff Effects ()
bookshelfWatchdogDemo = do
  logInfo "Bookshelf Watchdog Demo Begin"
--  watchDogSup <- Supervisor.startSupervisor (Supervisor.MkSupConfig @(Watchdog BookShelf) (TimeoutMicros 1_000_000) StartWatchDog)
--
--  shelf1Watchdog <- Supervisor.spawnOrLookup sup (BookShelfId 1)
--  shelf1 <- call shelf1Watchdog GetChild
--  call shelf1 (AddBook "Ubik")
--  call shelf1 GetBookList >>= logDebug . pack . show
--  cast shelf1 (TakeBook "Ubik")
--  call shelf1 GetBookList >>= logDebug . pack . show
  logInfo "Bookshelf Watchdog Demo End"

