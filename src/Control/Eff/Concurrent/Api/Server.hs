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
  ( ApiHandler (..), serve, unhandledCallError, unhandledCastError )
where

import           Control.Eff
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Internal
import           Control.Eff.Concurrent.MessagePassing
import           Data.Proxy
import           Data.Typeable (Typeable, typeRep)
import           Data.Dynamic
import           GHC.Stack

data ApiHandler p r where
  ApiHandler ::
     { _handleCast
         :: HasCallStack
         => Api p 'Asynchronous -> Eff r ()
     , _handleCall
         :: forall x . HasCallStack
         => Api p ('Synchronous x) -> (x -> Eff r Bool) -> Eff r ()
     , _handleTerminate
         :: HasCallStack
         => Maybe String -> Eff r ()
     } -> ApiHandler p r

serve
  :: forall r p
   . (Typeable p, Member Process r, HasCallStack)
  => ApiHandler p r
  -> Eff r ()
serve handlers@(ApiHandler handleCast handleCall handleTerminate) =
   do mReq <- send ReceiveMessage
      case mReq of
        RetryLastAction -> serve handlers
        ShutdownRequested -> handleTerminate Nothing
        OnError reason -> handleTerminate (Just reason)
        ResumeWith dyn ->
          case fromDynamic dyn of
            Just request ->
              handleRequest request
            Nothing ->
              handleTerminate
              (Just ("bad message received: " ++ show dyn))
   where
     handleRequest :: Request p -> Eff r ()
     handleRequest (Cast request        ) = handleCast request
     handleRequest (Call fromPid request) =
       handleCall request sendReply
      where
       sendReply :: Typeable x => x -> Eff r Bool
       sendReply reply =
         sendMessage fromPid (toDyn (Response (Proxy @p) reply))

unhandledCallError
  :: forall p x r .
    ( Show (Api p ( 'Synchronous x))
    , Typeable p
    , HasCallStack
    , Member Process r
    )
  => Api p ( 'Synchronous x)
  -> (x -> Eff r Bool)
  -> Eff r ()
unhandledCallError api _ = raiseError
  ("Unhandled call: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep (Proxy @p)) ++ ")")

unhandledCastError
  :: forall p r .
    ( Show (Api p 'Asynchronous)
    , Typeable p
    , HasCallStack
    , Member Process r
    )
  => Api p 'Asynchronous
  -> Eff r ()
unhandledCastError api = raiseError
  ("Unhandled cast: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep (Proxy @p)) ++ ")")
