module Main where

import           Data.String
import           Control.Eff
import           Control.Eff.Concurrent

main :: IO ()
main = defaultMain example

example :: Eff Effects ()
example = do
  person <- spawn (fromString "alice") alice
  replyToMe <- self
  sendMessage person replyToMe
  personName <- receiveMessage
  logInfo "I just met " (personName :: String)

alice :: Eff Effects ()
alice = do
  logInfo "I am waiting for someone to ask me..."
  sender <- receiveMessage
  sendMessage sender "Alice"
  logInfo sender " message received."
