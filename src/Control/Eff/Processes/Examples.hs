{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
module Control.Eff.Processes.Examples where

import GHC.Stack
import Control.Eff
import Control.Eff.Lift
import Control.Monad
import Data.Dynamic
import Data.Void

import Control.Eff.Processes
import Control.Eff.Processes.Server
import Control.Eff.Processes.STM

data TestServer
  deriving Typeable

data instance Api TestServer x where
  SayHello :: String -> Api TestServer Bool
  Shout :: String -> Api TestServer Void
  deriving Typeable

example :: (HasCallStack, HasScheduler r, Member Process r, SetMember Lift (Lift IO) r) => Eff r ()
example = do
  me <- self
  lift (putStrLn ("I am " ++ show me))
  server <- asServer @TestServer <$> spawn testa
  lift (putStrLn ("Started server " ++ show server))
  let go = do
        x <- lift (putStr ">>> " >> getLine)
        res <- call server (SayHello x)
        lift (putStrLn ("Result: " ++ show res))
        pid2 <- asServer @TestServer <$>
               spawn (do Just m <- serve (\(Shout m) k -> (m, k undefined))
                         cast_ server (Shout ("Who: " ++ m)))
        x2 <- lift (putStr "<<< " >> getLine)
        cast_ pid2 (Shout x2)
        case x2 of
          ('q':_) -> return ()
          _ -> go
  go

testa :: (HasCallStack, Member Process r, SetMember Lift (Lift IO) r) => Eff r ()
testa =
  forever $
     do me <- self
        Just msg <- serve (testaRespond me)
        lift (putStrLn msg)
        return ()

testaRespond :: ProcessId -> Api TestServer x -> (x -> z) -> (String, z)
testaRespond me (SayHello x) k =
   (show me ++ " Got Hello: " ++ x, k (length x > 3))
testaRespond me (Shout x) k =
   (show me ++ " Shouting: " ++ x, k undefined )
