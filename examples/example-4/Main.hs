module Main where

import Control.Eff
import Control.Eff.Concurrent

main :: IO ()
main = defaultMain example

example :: Eff Effects ()
example = do
  person <- spawn "alice" alice
  replyToMe <- self
  sendMessage person replyToMe
  personName <- receiveMessage
  logInfo (LABEL "I just met " (MSG personName))

alice :: Eff Effects ()
alice = do
  logInfo (MSG "I am waiting for someone to ask me...")
  sender <- receiveMessage
  sendMessage sender ("Alice" :: String)
  logInfo sender (MSG " message received.")
