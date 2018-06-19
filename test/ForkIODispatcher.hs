module ForkIODispatcher where

import Control.Eff.Concurrent.MessagePassing
import Control.Eff.Concurrent.Dispatcher as Dispatcher
import Control.Monad (void)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Dynamic

timeoutSeconds :: Integer -> Timeout
timeoutSeconds seconds = Timeout (seconds * 1000000) (show seconds ++ "s")

test_mainProcessSpawnsAChildAndReturns :: TestTree
test_mainProcessSpawnsAChildAndReturns =
  localOption
  (timeoutSeconds 2)
  (testCase "spawn a child and return"
   (Dispatcher.defaultMain
     (void (spawn (void (receiveMessage usingIoDispatcher))))))


test_mainProcessSpawnsAChildAndExitsNormally :: TestTree
test_mainProcessSpawnsAChildAndExitsNormally =
  localOption
  (timeoutSeconds 2)
  (testCase "spawn a child and exit normally"
   (Dispatcher.defaultMain
     (do void (spawn (void (receiveMessage usingIoDispatcher)))
         void (exitNormally usingIoDispatcher)
         fail "This should not happen!!"
     )))


test_mainProcessSpawnsAChildBothReturn :: TestTree
test_mainProcessSpawnsAChildBothReturn =
  localOption
  (timeoutSeconds 2)
  (testCase "spawn a child and let it return and return"
   (Dispatcher.defaultMain
     (do child <- spawn (void (receiveMessageAs @String usingIoDispatcher))
         True <- sendMessage usingIoDispatcher child (toDyn "test")
         return ()
     )))

test_mainProcessSpawnsAChildBothExitNormally :: TestTree
test_mainProcessSpawnsAChildBothExitNormally =
  localOption
  (timeoutSeconds 2)
  (testCase "spawn a child and let it return and return"
   (Dispatcher.defaultMain
     (do child <- spawn
                 (do void (receiveMessageAs @String usingIoDispatcher)
                     exitNormally usingIoDispatcher
                     error "This should not happen (child)!!"
                 )
         True <- sendMessage usingIoDispatcher child (toDyn "test")
         void (exitNormally usingIoDispatcher)
         error "This should not happen!!"
     )))
