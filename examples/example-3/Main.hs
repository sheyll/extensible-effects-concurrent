
module Main where

import           System.Directory
import           System.FilePath
import           System.IO
import           Control.Exception             as IOException
import           Control.Eff.Log
import           Control.Eff.Lift
import           Control.Concurrent

fileAppender :: FilePath -> LogWriter String IO
fileAppender fnIn = multiMessageLogWriter
  (\writeLogMessageWith -> bracket
    (do
      fnCanon <- canonicalizePath fnIn
      createDirectoryIfMissing True (takeDirectory fnCanon)
      h <- openFile fnCanon AppendMode
      hSetBuffering h (BlockBuffering (Just 1024))
      return h
    )
    (\h -> IOException.try @SomeException (hFlush h) >> hClose h)
    (writeLogMessageWith . hPutStrLn)
  )

fileAppenderSimple :: FilePath -> LogWriter String IO
fileAppenderSimple fnIn =
  singleMessageLogWriter (\msg -> withFile fnIn AppendMode (`hPutStrLn` msg))




main :: IO ()
main =
      -- putStrLn "Select Log Backend: (1) traceM - (2) fileAppenderSimple - (3) fileAppender - (4) stdout $ ")
      -- ui <- getLine
      -- let logH =
      --       case ui of
      --         '1':_ -> traceLogMessageWriter
      --         '2':_ -> fileAppender
  withAsyncLogChannel
  1000
  (ioLogMessageWriter
    (fileAppender "extensible-effects-concurrent-example-3-3.log")
  )
  (handleLoggingAndIO
    (do

      logInfo "test 1"
      logDebug "test 2"
      logCritical "test 3"
      lift (threadDelay 1000000)
    )
  )
