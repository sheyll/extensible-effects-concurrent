module Main where

import           Control.Eff
import           Control.Eff.Concurrent

main :: IO ()
main = defaultMain example

example :: Eff Effects ()
example = do
  person <- spawn "alice" alice
  replyToMe <- self
  sendMessage person replyToMe
  personName <- receiveMessage
  logInfo' ("I just met " ++ personName)

alice :: Eff Effects ()
alice = do
  logInfo "I am waiting for someone to ask me..."
  sender <- receiveMessage
  sendMessage sender ("Alice" :: String)
  logInfo' (show sender ++ " message received.")
