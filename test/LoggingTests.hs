module LoggingTests where

import           Control.Eff
import qualified Control.Eff.LogWriter.UDP as UDP
import qualified Control.Eff.LogWriter.Async as Async
import           Control.Eff.Concurrent
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import           Control.Lens       ((.~))
import           Control.Monad.Trans.Control (liftBaseOp)


test_Logging :: TestTree
test_Logging = setTravisTestOptions $ testGroup "Logging"
  [ basics
  , strictness
  , testGroup "IO"
    [ liftedIoLogging
    , udpLogging
    , udpNestedLogging
    , asyncLogging
    , asyncNestedLogging
    ]
  ]


basics :: HasCallStack => TestTree
basics =
  testCase "basic logging works" $
      pureLogs demo @?= (lmSrcLoc .~ Nothing) <$> [infoMessage "jo", debugMessage "oh"]
 where

    demo :: ('[Logs] <:: e) => Eff e ()
    demo = do
      logInfo "jo"
      logDebug "oh"

    pureLogs :: Eff '[Logs, LogWriterReader CaptureLogWriter, CaptureLogWriter] a -> [LogMessage]
    pureLogs =
        snd
      . run
      . runCaptureLogWriter
      . withLogging captureLogWriter
      . censorLogs @CaptureLogWriter (lmSrcLoc .~ Nothing)

strictness :: HasCallStack => TestTree
strictness =
  testCase "messages failing the predicate are not deeply evaluated"
    $ runLift
    $ withLogging consoleLogWriter
    $ excludeLogMessages (lmSeverityIs errorSeverity)
    $ do logDebug "test"
         logError' ("test" <> error "TEST FAILED: this log statement should not have been evaluated deeply")


liftedIoLogging :: HasCallStack => TestTree
liftedIoLogging =
  testCase "LogWriter can lifted through MonadBaseControl"
    $ do outVar <- newEmptyMVar
         runLift
          $ withLogging consoleLogWriter
          $ (\ e -> liftBaseOp
                      (testWriter outVar)
                      (\doWrite ->
                            addLogWriter (mkLogWriterIO doWrite) e))
          $ logDebug "test"
         actual <- takeMVar outVar
         assertEqual "wrong log message" "test" actual
   where
     testWriter :: MVar String -> ((LogMessage -> IO ()) -> IO ()) -> IO ()
     testWriter outVar withWriter =
       withWriter (putMVar outVar . show)

udpLogging :: HasCallStack => TestTree
udpLogging =
  testCase "UDP logging"
    $ do
         runLift
          -- $ withLogging consoleLogWriter
          $ UDP.withUDPLogging renderRFC5424NoLocation "localhost" "9999" "test-app" local0 allLogMessages
          $ do
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

udpNestedLogging :: HasCallStack => TestTree
udpNestedLogging =
  testCase "UDP Nested Logging"
    $ do
         runLift
          $ withLogging consoleLogWriter
          $ UDP.withUDPLogWriter renderRFC5424 "localhost" "9999"
          $ do
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

asyncLogging :: HasCallStack => TestTree
asyncLogging =
  testCase "Async Logging"
    $ do
         runLift
          $ Async.withAsyncLogging consoleLogWriter (1000::Int) "app-name" local0 allLogMessages
          $ do
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

asyncNestedLogging :: HasCallStack => TestTree
asyncNestedLogging =
  testCase "Async Nested Logging"
    $ do
         runLift
          $ withLogging consoleLogWriter
          $ Async.withAsyncLogWriter (1000::Int)
          $ do
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
