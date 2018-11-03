module Debug
where

import           ProcessBehaviourTestCases
import           Test.Tasty

debugMain :: IO ()
debugMain = defaultMain test_forkIo
