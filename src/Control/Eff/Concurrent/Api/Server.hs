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

module Control.Eff.Concurrent.Api.Server
  ( ApiHandler (..), serve, serve_, unhandledCallError, unhandledCastError )
where

import           Control.Eff
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Internal
import           Control.Eff.Concurrent.MessagePassing
import           Control.Lens
import           Control.Monad
import           Data.Kind
import           Data.Proxy
import           Data.Typeable (Typeable, typeRep)
import           GHC.Stack

data ApiHandler p r e where
  ApiHandler ::
     { _handleCast
         :: HasCallStack
         => Api p 'Asynchronous -> Eff r e
     , _handleCall
         :: forall x . HasCallStack
         => Api p ('Synchronous x) -> (x -> Eff r Bool) -> Eff r e
     , _handleTerminate
         :: HasCallStack
         => String -> Eff r ()
     } -> ApiHandler p r e

serve_
  :: forall r p
   . (Typeable p, Member MessagePassing r, Member Process r, HasCallStack)
  => ApiHandler p r ()
  -> Eff r ()
serve_ = void . serve

serve
  :: forall r p e
   . (Typeable p, Member MessagePassing r, Member Process r, HasCallStack)
  => ApiHandler p r e
  -> Eff r (Message e)
serve (ApiHandler handleCast handleCall handleTerminate) = do
  mReq <- receiveMessage (Proxy @(Request p))
  mapM receiveCallReq mReq >>= catchProcessControlMessage
 where
  catchProcessControlMessage :: Message e -> Eff r (Message e)
  catchProcessControlMessage s@(ProcessControlMessage msg) =
    handleTerminate msg >> return s
  catchProcessControlMessage s = return s

  receiveCallReq :: Request p -> Eff r e
  receiveCallReq (Cast request        ) = handleCast request
  receiveCallReq (Call fromPid request) = handleCall request
                                                     (sendReply request)
   where
    sendReply :: Typeable x => Api p ( 'Synchronous x) -> x -> Eff r Bool
    sendReply _ reply = sendMessage fromPid (Response (Proxy :: Proxy p) reply)

unhandledCallError
  :: forall p x r e .
    ( Show (Api p ( 'Synchronous x))
    , Typeable p
    , HasCallStack
    , Member Process r
    )
  => Api p ( 'Synchronous x)
  -> (x -> Eff r Bool)
  -> Eff r e
unhandledCallError api _ = raiseError
  ("Unhandled call: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep (Proxy @p)) ++ ")")

unhandledCastError
  :: forall p r e .
    ( Show (Api p 'Asynchronous)
    , Typeable p
    , HasCallStack
    , Member Process r
    )
  => Api p 'Asynchronous
  -> Eff r e
unhandledCastError api = raiseError
  ("Unhandled cast: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep (Proxy @p)) ++ ")")
