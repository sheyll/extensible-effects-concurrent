module ServerForkIO where

import           Control.Concurrent
import           Control.Eff.Extend
import           Control.Eff.State.Strict
import           Control.Eff.Concurrent
import           Control.Eff.Concurrent.Process.ForkIOScheduler
                                               as Scheduler
import           Control.Monad                  ( )
import           Test.Tasty                    hiding (Timeout)
import           Test.Tasty.HUnit
import           Common
import           Data.Typeable                 hiding ( cast )
import           Control.DeepSeq

data TestServer deriving Typeable

data instance Api TestServer x where
  TestGetStringLength :: String -> Api TestServer ('Synchronous Int)
  TestSetNextDelay :: Timeout -> Api TestServer 'Asynchronous
  deriving Typeable

instance NFData (Api TestServer x) where
  rnf (TestGetStringLength x) = rnf x
  rnf (TestSetNextDelay x) = rnf x

test_ServerCallTimeout :: TestTree
test_ServerCallTimeout =
  setTravisTestOptions
    $ testCase "Server call timeout"
    $ do
         runLift . withTraceLogging "test" local0 allLogMessages . Scheduler.schedule
               $ do  let backendDelay  = 100000
                         clientTimeout = 1000
                     s <- spawnBackend
                     linkProcess (_fromServer s)
                     logInfo "====================== All Servers Started ========================"
                     cast s (TestSetNextDelay backendDelay)
                     let testData = "this is a nice string" :: String
                         expected = 2 * length testData
                         testAction = callWithTimeout
                                       s
                                       (TestGetStringLength testData)

                     actual <- handleInterrupts
                                  (\i -> do
                                      logNotice' ("caught interrupt: " ++ show i)
                                      logNotice "adapting server delay"
                                      cast s (TestSetNextDelay clientTimeout)
                                      res <- testAction backendDelay
                                      logNotice "Yeah!! Got a result..."
                                      return (res * 2)
                                  ) 
                                  (testAction clientTimeout)
                     lift (expected @=? actual)
         threadDelay 1000


spawnRelay :: Timeout -> Server TestServer -> Eff InterruptableProcEff (Server TestServer)
spawnRelay callTimeout target = spawnApiServer hm hi
  where
    hi = InterruptCallback $ \i -> do
            logAlert' (show i)
            pure AwaitNext
    hm :: MessageCallback TestServer InterruptableProcEff
    hm = handleCastsAndCalls onCast onCall
      where
        onCast :: Api TestServer 'Asynchronous -> Eff InterruptableProcEff CallbackResult
        onCast (TestSetNextDelay x) = do
          logDebug "relaying delay"
          cast target (TestSetNextDelay x)
          pure AwaitNext

        onCall :: (NFData l, Typeable l)
               => Api TestServer ('Synchronous l)
               -> (Eff InterruptableProcEff (Maybe l, CallbackResult) -> k)
               -> k
        onCall (TestGetStringLength s) runHandler = runHandler $ do
          logDebug "relaying get string length"
          l <- callWithTimeout target (TestGetStringLength s) callTimeout
          logDebug' ("got result: " ++ show l)
          return (Just l, AwaitNext)


spawnBackend :: Eff InterruptableProcEff (Server TestServer)
spawnBackend = spawnApiServerStateful (return (1000000 :: Timeout)) hm hi
  where
    hi = InterruptCallback $ \i -> do
            logAlert' (show i)
            pure AwaitNext
    hm :: MessageCallback TestServer (State Timeout ': InterruptableProcEff)
    hm = handleCastsAndCalls onCast onCall
      where
        onCast :: Api TestServer 'Asynchronous -> Eff (State Timeout ': InterruptableProcEff) CallbackResult
        onCast (TestSetNextDelay x) = do
          logDebug' ("setting delay: " ++ show x)
          put x
          pure AwaitNext

        onCall :: (NFData l, Typeable l)
               => Api TestServer ('Synchronous l)
               -> (Eff (State Timeout ': InterruptableProcEff) (Maybe l, CallbackResult) -> k)
               -> k
        onCall (TestGetStringLength s) runHandler = runHandler $ do
          logDebug' ("calculating string length: " ++ show s)
          t <- get
          logDebug' ("sleeping for: " ++ show t)
          lift (threadDelay (fromTimeoutMicros t))
          logDebug "sending result"
          return (Just (length s), AwaitNext)