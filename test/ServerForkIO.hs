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
import           Data.Type.Pretty
import           Control.DeepSeq

data TestServer deriving Typeable
type instance ToPretty TestServer = PutStr "test"

data instance Pdu TestServer x where
  TestGetStringLength :: String -> Pdu TestServer ('Synchronous Int)
  TestSetNextDelay :: Timeout -> Pdu TestServer 'Asynchronous
  deriving Typeable

instance NFData (Pdu TestServer x) where
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
                     linkProcess (_fromEndpoint s)
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


spawnRelay :: Timeout -> Endpoint TestServer -> Eff InterruptableProcEff (Endpoint TestServer)
spawnRelay callTimeout target = spawnProtocolServer hm hi
  where
    hi = InterruptCallback $ \i -> do
            logAlert' (show i)
            pure AwaitNext
    hm :: MessageCallback TestServer InterruptableProcEff
    hm = handleCastsAndCalls onCast onCall
      where
        onCast :: Pdu TestServer 'Asynchronous -> Eff InterruptableProcEff (ServerLoopCommand 'Recoverable)
        onCast (TestSetNextDelay x) = do
          logDebug "relaying delay"
          cast target (TestSetNextDelay x)
          pure AwaitNext

        onCall :: (NFData l, Typeable l)
               => Pdu TestServer ('Synchronous l)
               -> (Eff InterruptableProcEff (Maybe l, ServerLoopCommand 'Recoverable) -> k)
               -> k
        onCall (TestGetStringLength s) runHandler = runHandler $ do
          logDebug "relaying get string length"
          l <- callWithTimeout target (TestGetStringLength s) callTimeout
          logDebug' ("got result: " ++ show l)
          return (Just l, AwaitNext)


spawnBackend :: Eff InterruptableProcEff (Endpoint TestServer)
spawnBackend = spawnProtocolServerStateful (return (1000000 :: Timeout)) hm hi
  where
    hi = InterruptCallback $ \i -> do
            logAlert' (show i)
            pure AwaitNext
    hm :: MessageCallback TestServer (State Timeout ': InterruptableProcEff)
    hm = handleCastsAndCalls onCast onCall
      where
        onCast :: Pdu TestServer 'Asynchronous -> Eff (State Timeout ': InterruptableProcEff) (ServerLoopCommand 'Recoverable)
        onCast (TestSetNextDelay x) = do
          logDebug' ("setting delay: " ++ show x)
          put x
          pure AwaitNext

        onCall :: (NFData l, Typeable l)
               => Pdu TestServer ('Synchronous l)
               -> (Eff (State Timeout ': InterruptableProcEff) (Maybe l, ServerLoopCommand 'Recoverable) -> k)
               -> k
        onCall (TestGetStringLength s) runHandler = runHandler $ do
          logDebug' ("calculating string length: " ++ show s)
          t <- get
          logDebug' ("sleeping for: " ++ show t)
          lift (threadDelay (fromTimeoutMicros t))
          logDebug "sending result"
          return (Just (length s), AwaitNext)
