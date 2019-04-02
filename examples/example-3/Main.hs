module Main where

import           Control.Concurrent
import           Control.Eff
import           Control.Eff.Log
import           Control.Exception             as IOException
import           System.Directory
import           System.FilePath
import           System.IO

main :: IO ()
main = withAsyncLogChannel
  (1000 :: Int)
  (ioLogMessageWriter
    (fileAppender "extensible-effects-concurrent-example-3.log")
  )
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

fileAppender :: FilePath -> LogWriter String IO
fileAppender fnIn = MkLogWriter
  (\m -> bracket
    (do
      fnCanon <- canonicalizePath fnIn
      createDirectoryIfMissing True (takeDirectory fnCanon)
      h <- openFile fnCanon AppendMode
      hSetBuffering h (BlockBuffering (Just 1024))
      return h
    )
    (\h -> IOException.try @SomeException (hFlush h) >> hClose h)
    (\h -> hPutStrLn h m)
  )

fileAppenderSimple :: FilePath -> LogWriter String IO
fileAppenderSimple fnIn =
  MkLogWriter (\msg -> withFile fnIn AppendMode (`hPutStrLn` msg))
