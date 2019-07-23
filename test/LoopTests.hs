module LoopTests
    ( test_loopTests
    , test_loopWithLeaksTests
    )
where

import           Control.Eff.State.Strict
import           Common
import           Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                               as Scheduler
import           Data.Text.IO as T

test_loopTests :: TestTree
test_loopTests =
    let soMany = 1000000
    in
        setTravisTestOptions $ testGroup
            "Loops without space leaks"
            [ testCase
                    "scheduleMonadIOEff with many yields from replicateCheapM_"
                $ do  lw <- consoleLogWriter
                      res <-
                          Scheduler.scheduleIOWithLogging  lw
                              $ replicateCheapM_ soMany yieldProcess
                      res @=? Right ()
            , testCase
                    "replicateCheapM_ of strict Int increments via the state effect"
                $ do
                      let
                          res = run
                              (execState
                                  (0 :: Int)
                                  ( replicateCheapM_ soMany
                                  $ modify (force . (+ 1))
                                  )
                              )
                      res @=? soMany
            , testCase
                    "'foreverCheap' inside a child process and 'replicateCheapM_' in the main process"
                $ do
                      res <- Scheduler.scheduleIOWithLogging  (MkLogWriter (T.putStrLn . (">>> " <>) . renderLogMessageConsoleLog))
                              $ do
                                    me <- self
                                    spawn_ "test" (foreverCheap $ sendMessage me ())
                                    replicateCheapM_
                                        soMany
                                        (void (receiveMessage @()))

                      res @=? Right ()
            ]


test_loopWithLeaksTests :: TestTree
test_loopWithLeaksTests =
    let soMany = 1000000
    in
        setTravisTestOptions $ testGroup
            "Loops WITH space leaks"
            [ testCase "scheduleMonadIOEff with many yields from replicateM_"
                $ do
                      res <-
                          Scheduler.scheduleIOWithLogging  (MkLogWriter (T.putStrLn . (">>> " <>) . renderLogMessageConsoleLog))
                              $ replicateM_ soMany yieldProcess
                      res @=? Right ()
            , testCase
                    "replicateM_ of strict Int increments via the state effect"
                $ do
                      let
                          res =
                              run
                                  (execState
                                      (0 :: Int)
                                      ( replicateM_ soMany
                                      $ modify (force . (+ 1))
                                      )
                                  )
                      res @=? soMany
            , testCase
                    "'forever' inside a child process and 'replicateM_' in the main process"
                $ do
                      res <-
                          Scheduler.scheduleIOWithLogging  (MkLogWriter (T.putStrLn . (">>> " <>) . renderLogMessageConsoleLog))
                              $ do
                                    me <- self
                                    spawn_ "test" (forever $ sendMessage me ())
                                    replicateM_ soMany
                                                (void (receiveMessage @()))

                      res @=? Right ()
            ]
