module Debug
where

import           ProcessBehaviourTestCases
import           Test.Tasty
import           Test.Tasty.HUnit

debugMain :: IO ()
debugMain = defaultMain test_forkIo
