module ProcessBehaviourTestCases where

import Data.List (sort)
import Data.Dynamic
import Data.Foldable (traverse_)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Eff.Concurrent.Process
import qualified Control.Eff.Concurrent.Process.ForkIOScheduler as ForkIO
import qualified Control.Eff.Concurrent.Process.SingleThreadedScheduler as SingleThreaded
import Control.Eff
import Control.Eff.Log
import Control.Eff.Lift
import Control.Monad (void, replicateM, forever, when)
import Test.Tasty
import Test.Tasty.HUnit
import Common

test_forkIo :: TestTree
test_forkIo =
  setTravisTestOptions
  (withTestLogC ForkIO.schedule
    (\factory ->
       testGroup "ForkIOScheduler"
       [allTests factory]))

test_singleThreaded :: TestTree
test_singleThreaded =
  setTravisTestOptions
  (withTestLogC (const . SingleThreaded.defaultMain)
    (\factory ->
       testGroup "SingleThreadedScheduler"
       [allTests factory]))

allTests :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r)
           => IO (Eff (Process r ': r) () -> IO ())
           -> TestTree
allTests schedulerFactory =
    localOption (timeoutSeconds 300)
    (testGroup "Process"
      [ errorTests schedulerFactory
      , sendShutdownTests schedulerFactory
      , concurrencyTests schedulerFactory
      , exitTests schedulerFactory
      ])

errorTests :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r)
             => IO (Eff (Process r ': r) () -> IO ()) -> TestTree
errorTests schedulerFactory =
  let px :: SchedulerProxy r
      px = SchedulerProxy
  in
    testGroup "causing and handling errors"
    [ testGroup "raiseError"
       [
         testCase "unhandled raiseError"
         $ applySchedulerFactory schedulerFactory
         $ do void $ raiseError px "test error"
              error "This should not happen"
       , testCase "catch raiseError"
         $ scheduleAndAssert schedulerFactory
         $ \assertEff ->
             catchRaisedError
                px
                (assertEff "error must be caught" . (== "test error 2"))
                (void (raiseError px "test error 2"))
       , testCase "catch raiseError from a long sub block"
         $ scheduleAndAssert schedulerFactory
         $ \assertEff ->
             catchRaisedError
                px
                (assertEff "error must be caught" . (== "test error 3"))
                (do void (replicateM 100000 (void (self px)))
                    void (raiseError px "test error 3"))
       ]
    , testGroup "exitWithError"
      [ testCase "unhandled exitWithError"
         $ applySchedulerFactory schedulerFactory
         $ do void $ exitWithError px "test error"
              error "This should not happen"
      , testCase "cannot catch exitWithError"
         $ applySchedulerFactory schedulerFactory
         $ do void $ ignoreProcessError px $ exitWithError px "test error 4"
              error "This should not happen"
      , testCase "multi process exitWithError"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff ->
           do me <- self px
              let n = 15
              traverse_
                (\(i :: Int) ->
                    spawn $
                      if i `rem` 5 == 0 then do
                        void $ sendMessage px me (toDyn i)
                        void (exitWithError px (show i ++ " died"))
                        assertEff "this should not be reached" False
                      else
                        forever
                          (void (sendMessage px 888 (toDyn "test message to 888"))))
                [0, 5 .. n]
              oks <- replicateM
                     (length [0,5 .. n])
                     (do j <- receiveMessageAs px
                         return j)
              assertEff "" (sort oks == [0,5 .. n])

      ]
    ]

concurrencyTests :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r)
                  => IO (Eff (Process r ': r) () -> IO ()) -> TestTree
concurrencyTests schedulerFactory =
  let px :: SchedulerProxy r
      px = SchedulerProxy
      n = 100
  in
    testGroup "concurrency tests"
    [ testCase "when main process exits the scheduler kills/cleans and returns"
      $ applySchedulerFactory schedulerFactory
      $ do me <- self px
           traverse_
             (const
               (spawn
                 (do m <- receiveMessage px
                     void (sendMessage px me m))))
             [1..n]
           lift (threadDelay 1000)
    ,
      testCase "new processes are executed before the parent process"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff ->
          do -- start massive amount of children that exit as soon as they are
             -- executed, this will only work smoothly when scheduler schedules
             -- the new child before the parent
             traverse_ (const (spawn (exitNormally px))) [1..n]
             assertEff "" True
    ,
      testCase "two concurrent processes"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff ->
          do me <- self px
             child1 <-
               spawn (do m <- receiveMessage px
                         void (sendMessage px me m))
             child2 <-
               spawn (forever (void (sendMessage px 888 (toDyn ""))))
             True <- sendMessage px child1 (toDyn "test")
             i <- receiveMessageAs px
             True <- sendShutdown px child2
             assertEff "" (i == "test")
    ,
      testCase "most processes send forever"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff ->
          do me <- self px
             traverse_
               (\(i :: Int) ->
                   spawn
                   $ do when (i `rem` 5 == 0) $
                          void $ sendMessage px me (toDyn i)
                        forever $
                          void (sendMessage px 888 (toDyn "test message to 888"))
               ) [0 .. n]
             oks <- replicateM
                     (length [0,5 .. n])
                     (do j <- receiveMessageAs px
                         return j)
             assertEff "" (sort oks == [0,5 .. n])
    , testCase "most processes self forever"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff ->
          do me <- self px
             traverse_
               (\(i :: Int) ->
                   spawn
                   $ do when (i `rem` 5 == 0) $
                          void $ sendMessage px me (toDyn i)
                        forever $ void (self px)
               ) [0 .. n]
             oks <- replicateM
                     (length [0,5 .. n])
                     (do j <- receiveMessageAs px
                         return j)
             assertEff "" (sort oks == [0,5 .. n])
    , testCase "most processes sendShutdown forever"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff ->
          do me <- self px
             traverse_
               (\(i :: Int) ->
                   spawn
                   $ do when (i `rem` 5 == 0) $
                          void $ sendMessage px me (toDyn i)
                        forever $ void (sendShutdown px 999)
               ) [0 .. n]
             oks <- replicateM
                     (length [0,5 .. n])
                     (do j <- receiveMessageAs px
                         return j)
             assertEff "" (sort oks == [0,5 .. n])
    , testCase "most processes spawn forever"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff ->
          do me <- self px
             traverse_
               (\(i :: Int) ->
                   spawn
                   $ do when (i `rem` 5 == 0) $
                          void $ sendMessage px me (toDyn i)
                        parent <- self px
                        forever $
                          void (spawn (void (sendMessage px parent (toDyn "test msg from child"))))
               ) [0 .. n]
             oks <- replicateM
                     (length [0,5 .. n])
                     (do j <- receiveMessageAs px
                         return j)
             assertEff "" (sort oks == [0,5 .. n])
    , testCase "most processes receive forever"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff ->
          do me <- self px
             traverse_
               (\(i :: Int) ->
                   spawn
                   $ do when (i `rem` 5 == 0) $
                          void $ sendMessage px me (toDyn i)
                        forever $
                          void (receiveMessage px)
               ) [0 .. n]
             oks <- replicateM
                     (length [0,5 .. n])
                     (do j <- receiveMessageAs px
                         return j)
             assertEff "" (sort oks == [0,5 .. n])
       ]

exitTests :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r)
          => IO (Eff (Process r ': r) () -> IO ()) -> TestTree
exitTests schedulerFactory =
  let px :: SchedulerProxy r
      px = SchedulerProxy
  in
    testGroup "process exit tests" $
    [ testGroup "async exceptions"
       [ testCase ("a process dies immediately if a "
                   ++ show e ++ " is thrown, while " ++ busyWith )
         $ do tidVar <- newEmptyTMVarIO
              schedulerDoneVar <- newEmptyTMVarIO
              void $ forkIO $
                do void $ try @SomeException $ void $ applySchedulerFactory schedulerFactory
                     $ do tid <- lift $ myThreadId
                          lift $ atomically $ putTMVar tidVar tid
                          forever busyEffect
                   atomically $ putTMVar schedulerDoneVar ()
              tid <- atomically $ takeTMVar tidVar
              threadDelay 1000
              throwTo tid e
              void $ atomically $ takeTMVar schedulerDoneVar
       | e <- [ ThreadKilled, UserInterrupt, HeapOverflow, StackOverflow ]
       , (busyWith, busyEffect) <-
         [ ("receiving", void (send (ReceiveMessage @r)))
         , ("sending", void (send (SendMessage @r 44444 (toDyn "test message"))))
         , ("sending shutdown", void (send (SendShutdown @r 44444)))
         , ("selfpid-ing", void (send (SelfPid @r)))
         , ("spawn-ing", void (send (Spawn @r (void (send (ReceiveMessage @r))))))
         , ("sleeping", lift (threadDelay 100000))
         ]
       ]
    , testGroup "main thread exit not blocked by"
       [ testCase ("a child process, busy with "++ busyWith)
        $ applySchedulerFactory schedulerFactory
        $ do void $ spawn $ forever busyEffect
             lift (threadDelay 10000)
       |  (busyWith, busyEffect) <-
        [ ("receiving", void (send (ReceiveMessage @r)))
        , ("sending", void (send (SendMessage @r 44444 (toDyn "test message"))))
        , ("sending shutdown", void (send (SendShutdown @r 44444)))
        , ("selfpid-ing", void (send (SelfPid @r)))
        , ("spawn-ing", void (send (Spawn @r (void (send (ReceiveMessage @r))))))
        ]

       ]
    , testGroup "one process exits, the other continues unimpaired"
       [ testCase ("process 2 exits with: "++ howToExit
                    ++ " - while process 1 is busy with: " ++ busyWith)
        $ scheduleAndAssert schedulerFactory
        $ \assertEff ->
          do p1 <- spawn $ forever busyEffect
             lift (threadDelay 1000)
             tlog "Yeah"
             void $ spawn $ do lift (threadDelay 1000)
                               doExit
             lift (threadDelay 100000)
             wasStillRunningP1 <- sendShutdown px p1
             assertEff "the other process was still running" wasStillRunningP1

       | (busyWith, busyEffect) <-
         [ ("receiving", void (send (ReceiveMessage @r)))
         , ("sending", void (send (SendMessage @r 44444 (toDyn "test message"))))
         , ("sending shutdown", void (send (SendShutdown @r 44444)))
         , ("selfpid-ing", void (send (SelfPid @r)))
         , ("spawn-ing", void (send (Spawn @r (void (send (ReceiveMessage @r))))))
         ]
       , (howToExit, doExit) <-
         [ ("normally", void (exitNormally px))
         , ("simply returning", return ())
         , ("raiseError", void (raiseError px "test error raised"))
         , ("exitWithError", void (exitWithError px "test error exit"))
         , ("sendShutdown to self", do me <- self px
                                       void (sendShutdown px me))
         ]
       ]
     ]


sendShutdownTests :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r)
                  => IO (Eff (Process r ': r) () -> IO ()) -> TestTree
sendShutdownTests schedulerFactory =
  let px :: SchedulerProxy r
      px = SchedulerProxy
  in
    testGroup "sendShutdown"
    [ testCase "... self"
      $ applySchedulerFactory schedulerFactory
      $ do me <- self px
           void $ sendShutdown px me
           raiseError px "sendShutdown must not return"
    , testCase "... self low-level"
      $ scheduleAndAssert schedulerFactory
      $ \assertEff ->
          do  me <- self px
              r <- send (SendShutdown @r me)
              assertEff
                "ShutdownRequested must be returned"
                (case r of
                    ShutdownRequested -> True
                    _ -> False)

    , testGroup "... other process"
      [ testCase "while it is sending"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff ->
          do  me <- self px
              other <-
                spawn (do untilShutdown (SendMessage @r 666 (toDyn "test"))
                          void (sendMessage px me (toDyn "OK")))
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
      , testCase "while it is receiving"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff ->
          do  me <- self px
              other <-
                spawn (do untilShutdown (ReceiveMessage @r)
                          void (sendMessage px me (toDyn "OK")))
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
      , testCase "while it is self'ing"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff ->
          do  me <- self px
              other <-
                spawn (do untilShutdown (SelfPid @r)
                          void (sendMessage px me (toDyn "OK")))
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
      , testCase "while it is spawning"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff ->
          do  me <- self px
              other <-
                spawn (do untilShutdown (Spawn @r (return ()))
                          void (sendMessage px me (toDyn "OK")))
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
      , testCase "while it is sending shutdown messages"
        $ scheduleAndAssert schedulerFactory
        $ \assertEff ->
          do  me <- self px
              other <-
                spawn (do untilShutdown (SendShutdown @r 777)
                          void (sendMessage px me (toDyn "OK")))
              void (sendShutdown px other)
              a <- receiveMessageAs px
              assertEff "" (a == "OK")
      ]
    ]
