{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Control.Eff.Interactive
  ( Interaction(..)
  , printLine
  , printStep
  , promptStep
  , step
  , Interactive(..)
  , interactiveProgram
  , runInteractionIOE
  , runInteractionIO
  )
where

import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Operational
import           Control.Monad                  ( void )
import           GHC.Stack

data Interaction a where
  PrintLine :: String -> Interaction ()
  ReadLine :: (String -> a) -> Interaction a
  Step :: Interaction ()

printLine
  :: (Member (Program Interaction) r, HasCallStack) => String -> Eff r ()
printLine = singleton . PrintLine

printStep
  :: (Member (Program Interaction) r, HasCallStack) => String -> Eff r ()
printStep m = do
  printLine m
  step

step :: (Member (Program Interaction) r, HasCallStack) => Eff r ()
step = do
  printLine "\nPress ENTER to continue."
  singleton Step

promptStep
  :: (Member (Program Interaction) r, HasCallStack)
  => String
  -> (String -> a)
  -> Eff r a
promptStep m p = do
  singleton (PrintLine m)
  singleton (ReadLine p)


runInteractionIOE
  :: (SetMember Lift (Lift IO) r, HasCallStack)
  => Eff (Program Interaction ': r) a
  -> Eff r a
runInteractionIOE = runProgram go
 where
  go :: (SetMember Lift (Lift IO) r, HasCallStack) => Interaction a -> Eff r a
  go (PrintLine m)    = lift (putStrLn m)
  go Step             = lift (void getLine)
  go (ReadLine parse) = lift (parse <$> getLine)

runInteractionIO :: Eff '[Program Interaction, Lift IO] a -> IO a
runInteractionIO = runLift . runInteractionIOE

-- ** Interactively work with 'Control.Eff.Operational.Program's.

class Interactive f where
  singleSteps :: (Member (Program Interaction) r, HasCallStack) => f a -> Eff r a

interactiveProgram
  :: (HasCallStack, Member (Program Interaction) r, Interactive f)
  => Eff (Program f ': r) a
  -> Eff r a
interactiveProgram = runProgram singleSteps
