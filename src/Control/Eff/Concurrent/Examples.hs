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
import Control.Eff.Concurrent.Dispatcher as Dispatcher
import Control.Eff.Log
import qualified Control.Exception as Exc

data TestApi
  deriving Typeable

data instance Api TestApi x where
  SayHello :: String -> Api TestApi ('Synchronous Bool)
  Shout :: String -> Api TestApi 'Asynchronous
  Terminate :: Api TestApi ('Synchronous ())
  TerminateError :: String -> Api TestApi ('Synchronous ())
  deriving (Typeable)

data MyException = MyException
    deriving Show

instance Exc.Exception MyException

deriving instance Show (Api TestApi x)

main :: IO ()
main = defaultMain (example usingIoDispatcher)

mainProcessSpawnsAChildAndReturns
  :: ( HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q -> Eff r ()
mainProcessSpawnsAChildAndReturns px =
  void (spawn (void (receiveMessage px)))

example
  :: ( HasCallStack
    , SetMember Process (Process q) r
    , Member (Logs String) r
    , Member (Logs String) q
    , SetMember Lift (Lift IO) q
    , SetMember Lift (Lift IO) r)
  => SchedulerProxy q -> Eff r ()
example px = do
  me <- self px
  logMsg ("I am " ++ show me)
  server <- asServer @TestApi <$> spawn testServerLoop
  logMsg ("Started server " ++ show server)
  let go = do
        x <- lift getLine
        case x of
          ('k':rest) -> do
            callRegistered px (TerminateError rest)
            go
          ('s':_) -> do
            callRegistered px Terminate
            go
          ('c':_) -> do
            castRegistered px (Shout x)
            go
          ('r':rest) -> do
            void (replicateM
                  (read rest)
                  (castRegistered px (Shout x)))
            go
          ('q':_) ->
            logMsg "Done."
          _ ->
            do res <- ignoreProcessError px (callRegistered px (SayHello x))
               logMsg ("Result: " ++ show res)
               go
  registerServer server go

testServerLoop
  :: forall r .
    (HasCallStack
    , Member (Logs String) r
    , SetMember Lift (Lift IO) r)
  => Eff (Process r ': r) ()
testServerLoop =
    (forever $ serve px $ ApiHandler handleCast handleCall handleTerminate)
  where
    px :: SchedulerProxy r
    px = SchedulerProxy
    handleCast :: Api TestApi 'Asynchronous -> Eff (Process r ': r) ()
    handleCast (Shout x) = do
      me <- self px
      logMsg (show me ++ " Shouting: " ++ x)
    handleCall :: Api TestApi ('Synchronous x) -> (x -> Eff (Process r ': r) Bool) -> Eff (Process r ': r) ()
    handleCall (SayHello "e1") _reply = do
      me <- self px
      logMsg (show me ++ " raising an error")
      raiseError px "No body loves me... :,("
    handleCall (SayHello "e2") _reply = do
      me <- self px
      logMsg (show me ++ " throwing a MyException ")
      lift (Exc.throw MyException)
    handleCall (SayHello "self") reply = do
      me <- self px
      logMsg (show me ++ " casting to self")
      cast px (asServer @TestApi me) (Shout "from me")
      void (reply False)
    handleCall (SayHello "die") reply = do
      me <- self px
      logMsg (show me ++ " throwing and catching ")
      catchProcessError px
        (\ er -> logMsg ("WOW: " ++ show er ++ " - No. This is wrong!"))
        (raiseError px "No body loves me... :,(")
      void (reply True)
    handleCall (SayHello x) reply = do
      me <- self px
      logMsg (show me ++ " Got Hello: " ++ x)
      void (reply (length x > 3))
    handleCall Terminate reply = do
      me <- self px
      logMsg (show me ++ " exiting")
      void (reply ())
      exitNormally px
    handleCall (TerminateError msg) reply = do
      me <- self px
      logMsg (show me ++ " exiting with error: " ++ msg)
      void (reply ())
      exitWithError px msg
    handleTerminate msg = do
      me <- self px
      logMsg (show me ++ " is exiting: " ++ show msg)
      maybe (exitNormally px) (raiseError px) msg
