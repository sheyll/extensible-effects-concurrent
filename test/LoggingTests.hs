module LoggingTests where

import Common
import qualified Control.Eff.LogWriter.Async as Async
import qualified Control.Eff.LogWriter.UDP as UDP
import Control.Lens
import Control.Monad.Trans.Control (liftBaseOp)
import qualified Data.Text as T

test_Logging :: TestTree
test_Logging =
  setTravisTestOptions $
    testGroup
      "logging"
      [ cencoredLogging,
        strictness,
        testGroup
          "IO"
          [ liftedIoLogging,
            udpLogging,
            udpNestedLogging,
            asyncLogging,
            asyncNestedLogging
          ]
      ]

cencoredLogging :: HasCallStack => TestTree
cencoredLogging =
  testCase "log cencorship works" $ do
    res <- fmap (view logEventMessage) <$> censoredLoggingTestImpl demo
    (renderLogMsgToString <$> res)
      @?= renderLogMsgToString . view logEventMessage
      <$> [ infoMessage $ MSG "1",
            debugMessage $ MSG "2",
            infoMessage $ MSG "x 1",
            debugMessage $ MSG "x 2",
            infoMessage $ MSG "x y 1",
            debugMessage $ MSG "x y 2",
            infoMessage $ MSG "x 1",
            debugMessage $ MSG "x 2",
            infoMessage $ MSG "1",
            debugMessage $ MSG "2"
          ]
  where
    renderLogMsgToString :: LogMsg -> String
    renderLogMsgToString (MkLogMsg txt) = T.unpack txt
    demo :: ('[Logs] <:: e) => Eff e ()
    demo = do
      logDebug (MSG "2")
      logInfo (MSG "1")
    censoredLoggingTestImpl :: Eff '[Logs, LogWriterReader, Lift IO] () -> IO [LogEvent]
    censoredLoggingTestImpl e = do
      logs <- newMVar []
      runLift $
        withLogging (MkLogWriter (\lm -> modifyMVar_ logs (\lms -> return (lm : lms)))) $
          do
            e
            censorLogs (logEventMessage %~ ("x " <>)) $ do
              e
              censorLogs (logEventMessage %~ ("y " <>)) e
              e
            e
      takeMVar logs

strictness :: HasCallStack => TestTree
strictness =
  testCase "messages failing the predicate are not deeply evaluated" $
    runLift $
      withConsoleLogging "test-app" local0 allLogEvents $
        blacklistLogEvents (logEventSeverityIs errorSeverity) $
          do
            logDebug (MSG "test")
            logError (error "TEST FAILED: this log statement should not have been evaluated deeply" :: String)

liftedIoLogging :: HasCallStack => TestTree
liftedIoLogging =
  testCase "logging vs. MonadBaseControl" $
    do
      outVar <- newEmptyMVar
      runLift $
        withConsoleLogging "test-app" local0 allLogEvents $
          ( \e ->
              liftBaseOp
                (testWriter outVar)
                ( \doWrite ->
                    addLogWriter (MkLogWriter doWrite) e
                )
          )
            $ logDebug (MSG "test")
      actual <- takeMVar outVar
      assertEqual "wrong log message" "test" actual
  where
    testWriter :: MVar String -> ((LogEvent -> IO ()) -> IO ()) -> IO ()
    testWriter outVar withWriter =
      withWriter (putMVar outVar . show)

test1234 :: Member Logs e => Eff e ()
test1234 = do
  logNotice $ MSG "~~~~~~~~~~~~~~~~~~~~~~~~~~test 1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  logNotice $ MSG "~~~~~~~~~~~~~~~~~~~~~~~~~~test 2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  logNotice $ MSG "~~~~~~~~~~~~~~~~~~~~~~~~~~test 3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  logNotice $ MSG "~~~~~~~~~~~~~~~~~~~~~~~~~~test 4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  let yumi = Yumi 123
      bubu = Bubu
      this = True
      that = 0.4 :: Double
      t :: Text
      t = "test"
  logNotice t
  logNotice yumi
  logNotice bubu
  logNotice this
  logNotice that

data Bubu = Bubu

instance ToLogMsg Bubu where toLogMsg _ = "bubu"

newtype Yumi = Yumi Double deriving (ToLogMsg)

udpLogging :: TestTree
udpLogging =
  testCase "udp logging" $
    runLift $
      UDP.withUDPLogging
        renderRFC5424NoLocation
        "localhost"
        "9999"
        "test-app"
        local0
        allLogEvents
        test1234

udpNestedLogging :: TestTree
udpNestedLogging =
  testCase "udp nested filteredlogging" $
    runLift $
      withConsoleLogging "test-app" local0 allLogEvents $
        UDP.withUDPLogWriter
          renderRFC5424
          "localhost"
          "9999"
          test1234

asyncLogging :: TestTree
asyncLogging =
  testCase "async filteredlogging" $
    do
      lw <- consoleLogWriter
      runLift $
        Async.withAsyncLogging
          lw
          (1000 :: Int)
          "app-name"
          local0
          allLogEvents
          test1234

asyncNestedLogging :: TestTree
asyncNestedLogging =
  testCase "async nested filteredlogging" $
    do
      lw <- consoleLogWriter
      runLift $
        withLogging lw $
          Async.withAsyncLogWriter
            (1000 :: Int)
            test1234
