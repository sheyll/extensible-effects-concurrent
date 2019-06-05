module Interactive where

import           Control.Concurrent
import           Control.Eff
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Process.Interactive
import qualified Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                               as SingleThreaded
import qualified Control.Eff.Concurrent.Process.ForkIOScheduler
                                               as ForkIOScheduler
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common

test_interactive :: TestTree
test_interactive = setTravisTestOptions $ testGroup
  "Interactive"
  [ testGroup "SingleThreadedScheduler" $ allTests SingleThreaded.defaultMain
  , testGroup "ForkIOScheduler" $ allTests ForkIOScheduler.defaultMain
  ]

allTests
  :: SetMember Lift (Lift IO) r
  => (Eff (Processes r) () -> IO ())
  -> [TestTree]
allTests scheduler = [happyCaseTest scheduler]

happyCaseTest
  :: SetMember Lift (Lift IO) r
  => (Eff (Processes r) () -> IO ())
  -> TestTree
happyCaseTest scheduler =
  testCase "start, wait and stop interactive scheduler" $ do
    s <- forkInteractiveScheduler scheduler
    threadDelay 100000
    killInteractiveScheduler s
    return ()
