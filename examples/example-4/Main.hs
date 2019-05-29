module Main where

import           Control.Eff
import           Control.Eff.Concurrent
import           Data.Dynamic
import           Control.DeepSeq
import           GHC.Stack (HasCallStack)

main :: IO ()
main =
    defaultMainWithLogWriter
      (defaultIoLogWriter "example-4" local0 consoleLogWriter)
      firstExample

newtype WhoAreYou = WhoAreYou ProcessId deriving (Typeable, NFData, Show)

firstExample :: (HasCallStack, Member Logs q) => Eff (InterruptableProcess q) ()
firstExample = do
  person <- spawn "first-example"
    (do
      logInfo "I am waiting for someone to ask me..."
      WhoAreYou replyPid <- receiveMessage
      sendMessage replyPid ("Alice" :: String)
      logInfo' (show replyPid ++ " just needed to know it.")
    )
  me <- self
  sendMessage person (WhoAreYou me)
  personName <- receiveMessage
  logInfo' ("I just met " ++ personName)
