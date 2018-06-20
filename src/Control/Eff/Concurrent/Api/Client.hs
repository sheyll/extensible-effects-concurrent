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

-- | Type safe /server/ API processes

module Control.Eff.Concurrent.Api.Client
  ( cast
  , castChecked
  , call
  , castRegistered
  , callRegistered
  , callRegisteredA
  , ServesApi
  , registerServer
  )
where

import           Control.Applicative
import           Control.Eff
import           Control.Eff.Reader.Lazy
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Internal
import           Control.Eff.Concurrent.Process
import           Control.Monad
import           Data.Dynamic
import           Data.Proxy
import           Data.Typeable (Typeable, typeRep)
import           GHC.Stack

castChecked
  :: forall r q o
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Typeable o
     , Typeable (Api o 'Asynchronous)
     )
  => SchedulerProxy q
  -> Server o
  -> Api o 'Asynchronous
  -> Eff r Bool
castChecked px (Server pid) callMsg =
  sendMessage px pid (toDyn  (Cast callMsg))

cast
  :: forall r q o
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Typeable o
     , Typeable (Api o 'Asynchronous)
     )
  => SchedulerProxy q
  -> Server o
  -> Api o 'Asynchronous
  -> Eff r ()
cast px toServer apiRequest =
  do _ <- castChecked px toServer apiRequest
     return ()

call
  :: forall result api r q
   . ( SetMember Process (Process q) r
     , Typeable api
     , Typeable (Api api ( 'Synchronous result))
     , Typeable result
     , HasCallStack
     )
  => SchedulerProxy q
  -> Server api
  -> Api api ('Synchronous result)
  -> Eff r result
call px (Server pidInt) req = do
  fromPid <- self px
  let requestMessage = Call fromPid req
  wasSent <- sendMessage px pidInt (toDyn requestMessage)
  if wasSent
    then
      let extractResult :: Response api result -> result
          extractResult (Response _pxResult result) = result
      in extractResult <$> receiveMessageAs px
    else raiseError px
      ("Could not send request message " ++ show (typeRep requestMessage))


type ServesApi o r q =
  ( Typeable o
  , SetMember Process (Process q) r
  , Member (Reader (Server o)) r
  )

registerServer :: Server o -> Eff ( Reader (Server o) ': r ) a -> Eff r a
registerServer = flip runReader

callRegistered :: (Typeable reply, ServesApi o r q)
      => SchedulerProxy q -> Api o ('Synchronous reply) -> Eff r reply
callRegistered px method = do
  serverPid <- ask
  call px serverPid method

callRegisteredA
  :: forall r q o f reply .
    (Alternative f, Typeable f, Typeable reply, ServesApi o r q)
  => SchedulerProxy q -> Api o ('Synchronous (f reply))
  -> Eff r (f reply)
callRegisteredA px method = do
  catchProcessError px
    (const (return (empty @f)))
    (callRegistered px method)

castRegistered :: (Typeable o, ServesApi o r q)
               => SchedulerProxy q -> Api o 'Asynchronous -> Eff r ()
castRegistered px method = do
  serverPid <- ask
  cast px serverPid method
