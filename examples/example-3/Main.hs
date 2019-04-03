module Main where

import           Control.Concurrent
import           Control.Eff
import           Control.Eff.Log
import           Control.Exception             as IOException
import           System.Directory
import           System.FilePath
import           System.IO

main :: IO ()
main =
  withLogFileAppender  "extensible-effects-concurrent-example-3.log" $ \fileAppender ->
  withAsyncLogChannel
  (1000 :: Int)
  fileAppender
  (handleLoggingAndIO
    (do
      logInfo "test 1"
      lift (threadDelay 1000000)
      logDebug "test 2"
      lift (threadDelay 1000000)
      logCritical "test 3"
      lift (threadDelay 1000000)
    )
  )
