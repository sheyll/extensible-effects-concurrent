module LoggingTests where

import           Control.Eff
import qualified Control.Eff.LogWriter.UDP as UDP
import qualified Control.Eff.LogWriter.Async as Async
import           Control.Eff.Concurrent
import           Test.Tasty
import           Test.Tasty.HUnit
import           Common
import           Control.Lens
import           Control.Monad.Trans.Control (liftBaseOp)




test_Logging :: TestTree
test_Logging = setTravisTestOptions $ testGroup "logging"
  [ cencoredLogging
  , strictness
  , testGroup "IO"
    [ liftedIoLogging
    , udpLogging
    , udpNestedLogging
    , asyncLogging
    , asyncNestedLogging
    ]
  ]

cencoredLogging :: HasCallStack => TestTree
cencoredLogging =
  testCase "log cencorship works" $ do
      res <- fmap (view lmMessage) <$> censoredLoggingTestImpl demo
      res @?=
        view lmMessage <$>
        [ infoMessage "1"
        , debugMessage "2"
        , infoMessage "x 1"
        , debugMessage "x 2"
        , infoMessage "x y 1"
        , debugMessage "x y 2"
        , infoMessage "x 1"
        , debugMessage "x 2"
        , infoMessage "1"
        , debugMessage "2"
        ]
 where

    demo :: ('[Logs] <:: e) => Eff e ()
    demo = do
      logDebug "2"
      logInfo "1"

    censoredLoggingTestImpl :: Eff '[Logs, LogWriterReader, Lift IO] () -> IO [LogEvent]
    censoredLoggingTestImpl e = do
      logs <- newMVar []
      runLift
       $ withLogging (MkLogWriter (\lm -> modifyMVar_ logs (\lms -> return (lm : lms))))
       $ do
           e
           censorLogs (lmMessage %~ ("x " <>)) $ do
              e
              censorLogs (lmMessage %~ ("y " <>)) e
              e
           e
      takeMVar logs

strictness :: HasCallStack => TestTree
strictness =
  testCase "messages failing the predicate are not deeply evaluated"
    $ runLift
    $ withConsoleLogging "test-app" local0 allLogMessages
    $ blacklistLogEvents (lmSeverityIs errorSeverity)
    $ do logDebug "test"
         logError' ("test" <> error "TEST FAILED: this log statement should not have been evaluated deeply")


liftedIoLogging :: HasCallStack => TestTree
liftedIoLogging =
  testCase "logging vs. MonadBaseControl"
    $ do outVar <- newEmptyMVar
         runLift
          $ withConsoleLogging "test-app" local0 allLogMessages
          $ (\ e -> liftBaseOp
                      (testWriter outVar)
                      (\doWrite ->
                            addLogWriter (MkLogWriter doWrite) e))
          $ logDebug "test"
         actual <- takeMVar outVar
         assertEqual "wrong log message" "test" actual
   where
     testWriter :: MVar String -> ((LogMessage -> IO ()) -> IO ()) -> IO ()
     testWriter outVar withWriter =
       withWriter (putMVar outVar . show)

test1234 :: Member Logs e => Eff e ()
test1234 = do
  logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test 1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test 2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test 3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  logNotice "~~~~~~~~~~~~~~~~~~~~~~~~~~test 4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  let yumi = Yumi 123
      bubu = Bubu
      this = True
      that = 0.4
      t :: Text
      t = "test"
  logNoticeX t


data Yumi = Yumi Int

data Bubu = Bubu

newtype LogMsgTxt = LogMsgTxt Text

class LogBuilder a where
  buildMsg :: Severity -> LogMsgTxt -> a

instance (Member Logs e) => LogBuilder (Eff e ()) where
  buildMsg s (LogMsgTxt m) = log  m

instance (LogBuilder b, ToLogMsg a) => LogBuilder (a -> b) where
  buildMsg m a =
    buildMsg (m <> toLogMsg a)


class ToLogMsg a where
  toLogMsg :: a -> LogMsgTxt

instance ToLogMsg Text where
  toLogMsg = LogMsgTxt

logNoticeX :: LogBuilder a => a
logNoticeX = undefined


udpLogging :: HasCallStack => TestTree
udpLogging =
  testCase "udp logging"
    $ runLift
    $ UDP.withUDPLogging renderRFC5424NoLocation "localhost" "9999" "test-app" local0 allLogMessages
      test1234

udpNestedLogging :: HasCallStack => TestTree
udpNestedLogging =
  testCase "udp nested filteredlogging"
    $ runLift
        $ withConsoleLogging "test-app" local0 allLogMessages
        $ UDP.withUDPLogWriter renderRFC5424 "localhost" "9999"
          test1234

asyncLogging :: HasCallStack => TestTree
asyncLogging =
  testCase "async filteredlogging"
    $ do lw <- consoleLogWriter
         runLift
            $ Async.withAsyncLogging lw (1000::Int) "app-name" local0 allLogMessages
              test1234

asyncNestedLogging :: HasCallStack => TestTree
asyncNestedLogging =
  testCase "async nested filteredlogging"
    $ do lw <- consoleLogWriter
         runLift
          $ withLogging lw
          $ Async.withAsyncLogWriter (1000::Int)
            test1234
