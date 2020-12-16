module Driver where

import Test.Tasty
import Test.Tasty.HUnit
import qualified BrokerTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ BrokerTests.test_Broker ]
