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

runExample :: IO (Either ProcessException ())
runExample = runProcIOWithScheduler example

example
  :: ( HasCallStack
    , HasProcesses r
    , Member MessagePassing r
    , Member Process r
    , SetMember Lift (Lift IO) r)
  => Eff r ()
example = do
  me <- self
  trapExit False
  lift (putStrLn ("I am " ++ show me))
  server <- asServer @TestApi <$> spawn testServerLoop
  lift (putStrLn ("Started server " ++ show server))
  let go = do
        x <- lift (putStr ">>> " >> getLine)
        res <- ignoreProcessError (call server (SayHello x))
        lift (putStrLn ("Result: " ++ show res))
        case x of
          ('k':_) -> do
            res2 <- kill server
            lift (putStrLn ("Result of killing: " ++ show res2))
            go
          ('q':_) -> lift (putStrLn "Done.")
          _ ->
            go
  go

testServerLoop
  :: forall r. (HasCallStack, Member MessagePassing r, Member Process r, SetMember Lift (Lift IO) r)
  => Eff r ()
testServerLoop =
  trapExit True
    >> (forever $ serve_ $ ApiHandler handleCast handleCall handleShutdown)
  where
    handleCast :: Api TestApi 'Asynchronous -> Eff r ()
    handleCast (Shout x) = do
      me <- self
      lift (putStrLn (show me ++ " Shouting: " ++ x))
    handleCall :: Api TestApi ('Synchronous x) -> (x -> Eff r Bool) -> Eff r ()
    handleCall (SayHello x) reply = do
      me <- self
      lift (putStrLn (show me ++ " Got Hello: " ++ x))
      catchProcessError
        (\ er -> lift (putStrLn ("WOW: " ++ show er ++ " - No. This is wrong!")))
        (when (x == "die") (raiseError "No body loves me... :,("))
      void (reply (length x > 3))
    handleShutdown = do
      me <- self
      lift (putStrLn (show me ++ " is exiting!"))
