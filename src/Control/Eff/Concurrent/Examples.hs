{-# LANGUAGE IncoherentInstances #-}
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
import Control.Eff.Log

data TestApi
  deriving Typeable

data instance Api TestApi x where
  SayHello :: String -> Api TestApi ('Synchronous Bool)
  Shout :: String -> Api TestApi 'Asynchronous
  deriving (Typeable)

deriving instance Show (Api TestApi x)

runExample :: IO (Either ProcessException ())
runExample =
  runLoggingT (runProcIOWithScheduler example) (print :: String -> IO ())


example
  :: ( HasCallStack
    , Member (Logs String) r
    , HasScheduler r
    , Member MessagePassing r
    , Member Process r
    , MonadLog String (Eff r)
    , SetMember Lift (Lift IO) r)
  => Eff r ()
example = do
  me <- self
  trapExit False
  logMessage ("I am " ++ show me)
  server <- asServer @TestApi <$> spawn testServerLoop
  logMessage ("Started server " ++ show server)
  let go = do
        x <- lift (putStr ">>> " >> getLine)
        res <- ignoreProcessError (call server (SayHello x))
        logMessage ("Result: " ++ show res)
        case x of
          ('k':_) -> do
            res2 <- kill server
            logMessage ("Result of killing: " ++ show res2)
            go
          ('q':_) -> logMessage "Done."
          _ ->
            go
  go

testServerLoop
  :: forall r. (HasCallStack, Member MessagePassing r, Member Process r
         , MonadLog String (Eff r)
         , SetMember Lift (Lift IO) r)
  => Eff r ()
testServerLoop =
  trapExit True
    >> (forever $ serve_ $ ApiHandler handleCast handleCall handleShutdown)
  where
    handleCast :: Api TestApi 'Asynchronous -> Eff r ()
    handleCast (Shout x) = do
      me <- self
      logMessage (show me ++ " Shouting: " ++ x)
    handleCall :: Api TestApi ('Synchronous x) -> (x -> Eff r Bool) -> Eff r ()
    handleCall (SayHello x) reply = do
      me <- self
      logMessage (show me ++ " Got Hello: " ++ x)
      catchProcessError
        (\ er -> logMessage ("WOW: " ++ show er ++ " - No. This is wrong!"))
        (when (x == "die") (raiseError "No body loves me... :,("))
      void (reply (length x > 3))
    handleShutdown = do
      me <- self
      logMessage (show me ++ " is exiting!")
