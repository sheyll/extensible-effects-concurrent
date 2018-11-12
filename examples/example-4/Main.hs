module Main where

import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Concurrent
import           Data.Dynamic
import           Control.Concurrent
import           Control.Applicative
import           Control.DeepSeq

main :: IO ()
main = defaultMain
  (do
    lift (threadDelay 100000) -- because of async logging
    firstExample forkIoScheduler
    lift (threadDelay 100000) -- ... async logging
  )
    -- The SchedulerProxy paremeter contains the effects of a specific scheduler
    -- implementation.

newtype WhoAreYou = WhoAreYou ProcessId deriving (Typeable, NFData)

firstExample
  :: (HasLogging IO q) => SchedulerProxy q -> Eff (InterruptableProcess q) ()
firstExample px = do
  person <- spawn
    (do
      logInfo "I am waiting for someone to ask me..."
      WhoAreYou replyPid <- receiveMessage px
      sendMessage px replyPid "Alice"
      logInfo (show replyPid ++ " just needed to know it.")
    )
  me <- self px
  sendMessage px person (WhoAreYou me)
  personName <- receiveMessage px
  logInfo ("I just met " ++ personName)


selectInt :: MessageSelector Int
selectInt = selectMessage

selectString :: MessageSelector String
selectString = selectMessage

selectIntOrString :: MessageSelector (Either Int String)
selectIntOrString = Left <$> selectInt <|> Right <$> selectString
