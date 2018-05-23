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
import qualified Control.Exception as Exc

data TestApi
  deriving Typeable

data instance Api TestApi x where
  SayHello :: String -> Api TestApi ('Synchronous Bool)
  Shout :: String -> Api TestApi 'Asynchronous
  deriving (Typeable)

data MyException = MyException
    deriving Show

instance Exc.Exception MyException

deriving instance Show (Api TestApi x)

runExample :: IO (Either ProcessException ())
runExample =
  runLoggingT (logChannelBracket (Just "hello") (Just "KTHXBY") (\ lc ->
                                                                   do r <- runProcIOWithScheduler example lc
                                                                      _ <- getLine
                                                                      return r))
                (print :: String -> IO ())


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
        x <- lift getLine
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
  -- trapExit True
    -- >>
    (forever $ serve_ $ ApiHandler handleCast handleCall handleShutdown)
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
    handleShutdown = do
      me <- self
      logMessage (show me ++ " is exiting!")
