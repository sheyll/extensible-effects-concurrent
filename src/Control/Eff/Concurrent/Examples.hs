{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Control.Eff.Concurrent.Examples where

import GHC.Stack
import Control.Eff
import Control.Eff.Lift
import Control.Monad
import Data.Dynamic

import Control.Eff.Concurrent.MessagePassing
import Control.Eff.Concurrent.GenServer
import Control.Eff.Concurrent.Dispatcher

data TestApi
  deriving Typeable

data instance Api TestApi x where
  SayHello :: String -> Api TestApi ('Synchronous Bool)
  Shout :: String -> Api TestApi 'Asynchronous
  deriving (Typeable)

deriving instance Show (Api TestApi x)

runExample :: IO ()
runExample = runLift $ runProcesses $ dispatchMessages example

example
  :: ( HasCallStack
    , HasProcesses r
    , Member MessagePassing r
    , Member Process r
    , SetMember Lift (Lift IO) r)
  => Eff r ()
example = do
  me <- self
  lift (putStrLn ("I am " ++ show me))
  server <- asServer @TestApi <$> spawn testServerLoop
  lift (putStrLn ("Started server " ++ show server))
  let go = do
        x <- lift (putStr ">>> " >> getLine)
        res <- call server (SayHello x)
        lift (putStrLn ("Result: " ++ show res))
        case x of
          ('q':_) -> return ()
          _ -> go
  go

testServerLoop
  :: forall r. (HasCallStack, Member MessagePassing r, Member Process r, SetMember Lift (Lift IO) r)
  => Eff r ()
testServerLoop = forever $ serve_ $ ApiHandler handleCast handleCall
  where
    handleCast :: Api TestApi 'Asynchronous -> Eff r ()
    handleCast (Shout x) = do
      me <- self
      lift (putStrLn (show me ++ " Shouting: " ++ x))
    handleCall :: Api TestApi ('Synchronous x) -> (x -> Eff r Bool) -> Eff r ()
    handleCall (SayHello x) reply = do
      me <- self
      lift (putStrLn (show me ++ " Got Hello: " ++ x))
      void (reply (length x > 3))
