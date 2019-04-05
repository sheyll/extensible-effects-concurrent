-- | A logging effect based on 'Control.Monad.Log.MonadLog'.
module Control.Eff.Log
  ( module Control.Eff.Log.Channel
  , module Control.Eff.Log.Handler
  , module Control.Eff.Log.Message
  , module Control.Eff.Log.Writer
  )
where

import           Control.Eff.Log.Channel
import           Control.Eff.Log.Handler
import           Control.Eff.Log.Message
import           Control.Eff.Log.Writer

import           Control.Eff
import           Control.Lens (view, (%~), to)

_example1 :: IO ()
_example1 =
    runLift
  $ runLogWriterReader (ioLogWriter printLogMessage)
  $ runLogs
  $ do
      logDebug "test 1.1"
      logDebug "test 1.2"
      setThreadIdAndTimestamp
       $ do
            addLogWriter debugTraceLogWriter
             $ setLogPredicate (\m -> (view lmMessage m) /= "not logged")
             $ do
                  logDebug "not logged"
                  logDebug "test 2.1"
            logDebug "test 2.2"
      logDebug "test 1.3"

_example2 :: IO Int
_example2 =
    runLift
  $ runLogWriterReader (ioLogWriter printLogMessage)
  $ runLogs @IO
  $ do logMsg "test"
       setLogPredicate (\ msg -> case view lmMessage msg of
                                  'O':'M':'G':_ -> True
                                  _             -> False)
                         (do logMsg "this message will not be logged"
                             logMsg "OMG logged"
                             return 42)


_example3 :: IO ()
_example3 = go >>= putStrLn
 where go = fmap (unlines . map renderLogMessage . snd)
              $  runLift
              $  runCapturedLogsWriter
              $  runLogWriterReader listLogWriter
              $  runLogs
              $  addLogWriter (mappingLogWriter (lmMessage %~ ("CAPTURED "++)) listLogWriter)
              $  addLogWriter (filteringLogWriter testPred (mappingLogWriter (lmMessage %~ ("TRACED "++)) debugTraceLogWriter))
              $  do
                    logEmergency "test emergencySeverity 1"
                    logCritical "test criticalSeverity 2"
                    logAlert "test alertSeverity 3"
                    logError "test errorSeverity 4"
                    logWarning "test warningSeverity 5"
                    logInfo "test informationalSeverity 6"
                    logDebug "test debugSeverity 7"
       testPred = view (lmSeverity . to (<= errorSeverity))
