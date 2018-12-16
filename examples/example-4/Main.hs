module Main where

import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Concurrent
import           Data.Dynamic
import           Control.Concurrent
import           Control.DeepSeq

main :: IO ()
main = defaultMain
  (do
    lift (threadDelay 100000) -- because of async logging
    firstExample
    lift (threadDelay 100000) -- ... async logging
  )
    -- The SchedulerProxy paremeter contains the effects of a specific scheduler
    -- implementation.


newtype WhoAreYou = WhoAreYou ProcessId deriving (Typeable, NFData, Show)

firstExample :: (HasLogging IO q) => Eff (InterruptableProcess q) ()
firstExample = do
  person <- spawn
    (do
      logInfo "I am waiting for someone to ask me..."
      WhoAreYou replyPid <- receiveMessage
      sendMessage replyPid "Alice"
      logInfo (show replyPid ++ " just needed to know it.")
    )
  me <- self
  sendMessage person (WhoAreYou me)
  personName <- receiveMessage
  logInfo ("I just met " ++ personName)
