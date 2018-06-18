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
import Control.Eff.Concurrent.Api
import Control.Eff.Concurrent.Api.Client
import Control.Eff.Concurrent.Api.Server
import Control.Eff.Concurrent.MessagePassing
import Control.Eff.Concurrent.Dispatcher
import Control.Eff.Log
import qualified Control.Exception as Exc

data TestApi
  deriving Typeable

data instance Api TestApi x where
  SayHello :: String -> Api TestApi ('Synchronous Bool)
  Shout :: String -> Api TestApi 'Asynchronous
  SetTrapExit :: Bool -> Api TestApi ('Synchronous ())
  Terminate :: Api TestApi ('Synchronous ())
  deriving (Typeable)

data MyException = MyException
    deriving Show

instance Exc.Exception MyException

deriving instance Show (Api TestApi x)

main :: IO ()
main = defaultMain example

example
  :: ( HasCallStack
    , Member (Logs String) r
    , HasDispatcherIO r
    , Member MessagePassing r
    , Member Process r
    , MonadLog String (Eff r)
    , SetMember Lift (Lift IO) r)
  => Eff r ()
example = do
  me <- self
  trapExit True
  logMessage ("I am " ++ show me)
  server <- asServer @TestApi <$> spawn testServerLoop
  logMessage ("Started server " ++ show server)
  let go = do
        x <- lift getLine
        case x of
          ('k':_) -> do
            callRegistered Terminate
            go
          ('c':_) -> do
            castRegistered (Shout x)
            go
          ('t':'0':_) -> do
            callRegistered (SetTrapExit False)
            go
          ('t':'1':_) -> do
            callRegistered (SetTrapExit True)
            go
          ('q':_) ->
            logMsg "Done."
          _ ->
            do res <- ignoreProcessError (callRegistered (SayHello x))
               logMsg ("Result: " ++ show res)
               go
  registerServer server go

testServerLoop
  :: forall r. (HasCallStack, Member MessagePassing r, Member Process r
         , MonadLog String (Eff r)
         , SetMember Lift (Lift IO) r)
  => Eff r ()
testServerLoop =
  -- trapExit True
    -- >>
    (forever $ serve $ ApiHandler handleCast handleCall handleTerminate)
  where
    handleCast :: Api TestApi 'Asynchronous -> Eff r ()
    handleCast (Shout x) = do
      me <- self
      logMessage (show me ++ " Shouting: " ++ x)
    handleCall :: Api TestApi ('Synchronous x) -> (x -> Eff r Bool) -> Eff r ()
    handleCall (SayHello "e1") _reply = do
      me <- self
      logMessage (show me ++ " raising an error")
      raiseError "No body loves me... :,("
    handleCall (SayHello "e2") _reply = do
      me <- self
      logMessage (show me ++ " throwing a MyException ")
      lift (Exc.throw MyException)
    handleCall (SayHello "self") reply = do
      me <- self
      logMessage (show me ++ " casting to self")
      cast (asServer @TestApi me) (Shout "from me")
      void (reply False)
    handleCall (SayHello "die") reply = do
      me <- self
      logMessage (show me ++ " throwing and catching ")
      catchProcessError
        (\ er -> logMessage ("WOW: " ++ show er ++ " - No. This is wrong!"))
        (raiseError "No body loves me... :,(")
      void (reply True)
    handleCall (SayHello x) reply = do
      me <- self
      logMessage (show me ++ " Got Hello: " ++ x)
      void (reply (length x > 3))
    handleCall (SetTrapExit x) reply = do
      me <- self
      logMessage (show me ++ " setting trap exit to " ++ show x)
      trapExit x
      void (reply ())
    handleCall Terminate reply = do
      me <- self
      logMessage (show me ++ " exitting")
      void (reply ())
      raiseError "DONE"
    handleTerminate msg = do
      me <- self
      logMessage (show me ++ " is exiting: " ++ msg)
      raiseError msg
