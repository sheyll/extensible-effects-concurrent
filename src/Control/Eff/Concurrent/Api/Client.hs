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
import           Control.Eff.Concurrent.MessagePassing
import           Control.Monad
import           Data.Dynamic
import           Data.Proxy
import           Data.Typeable (Typeable, typeRep)
import           GHC.Stack

castChecked
  :: forall r o
   . ( HasCallStack
     , Member Process r
     , Typeable o
     , Typeable (Api o 'Asynchronous)
     )
  => Server o
  -> Api o 'Asynchronous
  -> Eff r Bool
castChecked (Server pid) callMsg =
  sendMessage pid (toDyn  (Cast callMsg))

cast
  :: forall r o
   . ( HasCallStack
     , Member Process r
     , Typeable o
     , Typeable (Api o 'Asynchronous)
     )
  => Server o
  -> Api o 'Asynchronous
  -> Eff r ()
cast toServer apiRequest =
  do _ <- castChecked toServer apiRequest
     return ()

call
  :: forall result api r
   . ( Member Process r
     , Typeable api
     , Typeable (Api api ( 'Synchronous result))
     , Typeable result
     , HasCallStack
     )
  => Server api
  -> Api api ('Synchronous result)
  -> Eff r result
call (Server pidInt) req = do
  fromPid <- self
  let requestMessage = Call fromPid req
  wasSent <- sendMessage pidInt (toDyn requestMessage)
  if wasSent
    then
      let extractResult :: Response api result -> result
          extractResult (Response _pxResult result) = result
      in extractResult <$> receiveMessageAs
    else raiseError
      ("Could not send request message " ++ show (typeRep requestMessage))


type ServesApi o r =
  ( Typeable o
  , Member Process r
  , Member (Reader (Server o)) r
  )

registerServer :: Server o -> Eff ( Reader (Server o) ': r ) a -> Eff r a
registerServer = flip runReader

callRegistered :: (Typeable reply, ServesApi o r)
      => Api o ('Synchronous reply) -> Eff r reply
callRegistered method = do
  serverPid <- ask
  call serverPid method

callRegisteredA
  :: forall r o f reply .
    (Alternative f, Typeable f, Typeable reply, ServesApi o r)
  => Api o ('Synchronous (f reply))
  -> Eff r (f reply)
callRegisteredA method = do
  catchProcessError
    (const (return (empty @f)))
    (callRegistered method)

castRegistered :: (Typeable o, ServesApi o r)
               => Api o 'Asynchronous -> Eff r ()
castRegistered method = do
  serverPid <- ask
  cast serverPid method
