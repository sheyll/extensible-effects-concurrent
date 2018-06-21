module ProcessBehaviourTestCases where

import Data.Dynamic
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
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

timeoutSeconds :: Integer -> Timeout
timeoutSeconds seconds = Timeout (seconds * 1000000) (show seconds ++ "s")

test_forkIo :: TestTree
test_forkIo =
  withResource
  (forkLogChannel (print . (">>>>>>>>>>>>>>>>>>>>>>>>>>>>> " ++))
                    (Just "~~~~~~~ForkIo Logs Begin~~~~~~~"))
  (joinLogChannel (Just "^^^^^^^ForkIo Logs End^^^^^^^"))
  (\ logCFactory ->
      testGroup "ForkIOScheduler"
      [allTests
       (return
         (\e ->
            do logC <- logCFactory
               ForkIO.schedule e logC))])

test_singleThreaded :: TestTree
test_singleThreaded =
  testGroup "SingleThreadedScheduler"
  [allTests (return SingleThreaded.defaultMain)]

allTests :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r)
           => IO (Eff (Process r ': r) () -> IO ())
           -> TestTree
allTests schedulerFactory =
    localOption (timeoutSeconds 1)
    (testGroup "Process"
      [ errorTests schedulerFactory
      , sendShutdownTests schedulerFactory
      , concurrencyTests schedulerFactory
      ])

errorTests :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r) => IO (Eff (Process r ': r) () -> IO ()) -> TestTree
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
                (assertEff "error must be caught" . (== "test error 2"))
                (do void (replicateM 100000 (void (self px)))
                    void (raiseError px "test error 2"))
       ]
    , testGroup "exitWithError"
      [ testCase "unhandled exitWithError"
         $ applySchedulerFactory schedulerFactory
         $ do void $ exitWithError px "test error"
              error "This should not happen"
      , testCase "cannot catch exitWithError"
         $ applySchedulerFactory schedulerFactory
         $ do void $ ignoreProcessError px $ exitWithError px "test error"
              error "This should not happen"
      , localOption (timeoutSeconds 10)
        $ testCase "multi process exitWithError"
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
                          (void (sendMessage px 888 (toDyn "test message to 888")))
                          -- (void (receiveMessage px))
                        )
                [0, 5 .. n]
              oks <- traverse
                     (\(i :: Int) ->
                        do j <- receiveMessageAs px
                           return (i == j))
                     [0, 5 .. n]
              assertEff "" (all id oks)

      ]
    ]

concurrencyTests :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r)
                  => IO (Eff (Process r ': r) () -> IO ()) -> TestTree
concurrencyTests schedulerFactory =
  let px :: SchedulerProxy r
      px = SchedulerProxy
      n = 100
  in
    localOption (timeoutSeconds 15)
    $ testGroup "concurrency tests"
    [ testCase "when main process exits the scheduler kills/cleans and returns"
      $ do schedule <- schedulerFactory
           schedule
             $ do me <- self px
                  traverse_
                    (const
                     (spawn
                      (do m <- receiveMessage px
                          void (sendMessage px me m))))
                    [1..n]
                  logMsg (show me ++ " returning")
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
             oks <- traverse
                     (\(i :: Int) ->
                        do j <- receiveMessageAs px
                           return (i == j))
                     [0, 5 .. n]
             assertEff "" (all id oks)
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
             oks <- traverse
                     (\(i :: Int) ->
                        do j <- receiveMessageAs px
                           return (i == j))
                     [0, 5 .. n]
             assertEff "" (all id oks)
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
             oks <- traverse
                     (\(i :: Int) ->
                        do j <- receiveMessageAs px
                           return (i == j))
                     [0, 5 .. n]
             assertEff "" (all id oks)
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
             oks <- traverse
                     (\(i :: Int) ->
                        do j <- receiveMessageAs px
                           return (i == j))
                     [0, 5 .. n]
             assertEff "" (all id oks)
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
             oks <- traverse
                     (\(i :: Int) ->
                        do j <- receiveMessageAs px
                           return (i == j))
                     [0, 5 .. n]
             assertEff "" (all id oks)
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
           logMsg "sendShutdown must not return"
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

untilShutdown
  :: Member t r => t (ResumeProcess v) -> Eff r ()
untilShutdown pa = do
  r <- send pa
  case r of
    ShutdownRequested -> return ()
    _ -> untilShutdown pa

scheduleAndAssert :: forall r .
                    (SetMember Lift (Lift IO) r, Member (Logs String) r)
                  => IO (Eff (Process r ': r) () -> IO ())
                  -> ((String -> Bool -> Eff (Process r ': r) ()) -> Eff (Process r ': r) ())
                  -> IO ()
scheduleAndAssert schedulerFactory testCaseAction =
  do resultVar <- newEmptyTMVarIO
     void (applySchedulerFactory schedulerFactory
            (testCaseAction (((lift . atomically . putTMVar resultVar) .) . (,))))
     (title, result) <- atomically (readTMVar resultVar)
     assertBool title result

scheduleAndAssertA :: forall r .
                    (SetMember Lift (Lift IO) r, Member (Logs String) r)
                  => IO (Eff (Process r ': r) () -> IO ())
                  -> ((String -> Bool -> Eff (Process r ': r) ()) -> Eff (Process r ': r) ())
                  -> IO ()
scheduleAndAssertA schedulerFactory testCaseAction =
  do resultVar <- newEmptyTMVarIO
     void (forkIO (applySchedulerFactory schedulerFactory
                        (testCaseAction (((lift . atomically . putTMVar resultVar) .) . (,)))))
     (title, result) <- atomically (readTMVar resultVar)
     assertBool title result

applySchedulerFactory :: forall r . (Member (Logs String) r, SetMember Lift (Lift IO) r) => IO (Eff (Process r ': r) () -> IO ()) -> Eff (Process r ': r) () -> IO ()
applySchedulerFactory factory procAction =
  do scheduler <- factory
     scheduler procAction
