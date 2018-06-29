module Interactive
where

import           Data.List                      ( sort )
import           Data.Dynamic
import           Data.Foldable                  ( traverse_ )
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Process.Interactive
import qualified Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                               as SingleThreaded
import qualified Control.Eff.Concurrent.Process.ForkIOScheduler
                                               as ForkIOScheduler
import           Control.Eff
import           Control.Eff.Log
import           Control.Eff.Lift
import           Control.Monad                  ( void
                                                , replicateM
                                                , forever
                                                , when
                                                )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common

test_singleThreaded :: TestTree
test_singleThreaded = setTravisTestOptions $ testGroup
  "Interactive"
  [ testGroup
    "SingleThreadedScheduler"
    [testCase "start interactive scheduler, do nothing, then exit" theTestST]
  , testGroup
    "ForkIOScheduler"
    [testCase "start interactive scheduler, do nothing, then exit" theTestF]
  ]



theTestST :: IO ()
theTestST = do
  s <- forkInteractiveScheduler (SingleThreaded.defaultMain)
  threadDelay 10000000
  killInteractiveScheduler s
  return ()


theTestF :: IO ()
theTestF = do
  s <- forkInteractiveScheduler (ForkIOScheduler.defaultMain)
  threadDelay 10000000
  killInteractiveScheduler s
  return ()
