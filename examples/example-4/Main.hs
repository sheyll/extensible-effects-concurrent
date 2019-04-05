module Main where

import           Control.Eff
import           Control.Eff.Concurrent
import           Data.Dynamic
import           Control.Concurrent
import           Control.DeepSeq
import Debug.Trace
import GHC.Stack (HasCallStack)

main :: IO ()
main = defaultMain  $ logTo debugTraceLogWriter
  (do
    lift (threadDelay 100000) -- because of async logging
    firstExample
    lift (threadDelay 1000000) -- ... async logging
  )

newtype WhoAreYou = WhoAreYou ProcessId deriving (Typeable, NFData, Show)

firstExample :: (HasCallStack, Member Logs q) => Eff (InterruptableProcess q) ()
firstExample = do
  traceShowM "A - 0000"
  person <- spawn
    (do
      traceShowM "B - 0000"
      logInfo "I am waiting for someone to ask me..."
      traceShowM "B - 0001"
      WhoAreYou replyPid <- receiveMessage
      traceShowM "B - 0002"
      sendMessage replyPid "Alice"
      traceShowM "B - 0003"
      logInfo (show replyPid ++ " just needed to know it.")
      traceShowM "B - 0004"
    )
  traceShowM "A - 0001"
  me <- self
  traceShowM "A - 0002"
  sendMessage person (WhoAreYou me)
  traceShowM "A - 0003"
  personName <- receiveMessage
  traceShowM "A - 0004"
  logInfo ("I just met " ++ personName)
  traceShowM "A - 0005"
