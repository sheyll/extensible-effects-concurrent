module LoopTests
    ( test_loopTests
    , test_loopWithLeaksTests
    )
where

import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.Concurrent
import           Control.Eff.State.Strict
import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import           Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                               as Scheduler

test_loopTests :: TestTree
test_loopTests
    = let soMany = 1000000
      in
          setTravisTestOptions $ testGroup
              "Loops without space leaks"
              [ testCase
                      "scheduleMonadIOEff with many yields from replicateCheapM_"
                  $ do
                        res <-
                            Scheduler.scheduleIOWithLogging
                                (multiMessageLogWriter
                                    ($! (putStrLn . (">>> " ++)))
                                )
                            $ replicateCheapM_ soMany
                            $ yieldProcess SP
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
                        res <-
                            Scheduler.scheduleIOWithLogging
                                    (multiMessageLogWriter
                                        ($! (putStrLn . (">>> " ++)))
                                    )

                                $ do
                                      me <- self SP
                                      spawn_
                                          (foreverCheap $ sendMessage SP me ()
                                          )
                                      replicateCheapM_
                                          soMany
                                          (void (receiveMessage @() SP))

                        res @=? Right ()
              ]


test_loopWithLeaksTests :: TestTree
test_loopWithLeaksTests
    = let soMany = 1000000
      in
          setTravisTestOptions $ testGroup
              "Loops WITH space leaks"
              [ testCase "scheduleMonadIOEff with many yields from replicateM_"
                  $ do
                        res <-
                            Scheduler.scheduleIOWithLogging
                                (multiMessageLogWriter
                                    ($! (putStrLn . (">>> " ++)))
                                )
                            $ replicateM_ soMany
                            $ yieldProcess SP
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
                            Scheduler.scheduleIOWithLogging
                                    (multiMessageLogWriter
                                        ($! (putStrLn . (">>> " ++)))
                                    )
                                $ do
                                      me <- self SP
                                      spawn_ (forever $ sendMessage SP me ())
                                      replicateM_
                                          soMany
                                          (void (receiveMessage @() SP))

                        res @=? Right ()
              ]
