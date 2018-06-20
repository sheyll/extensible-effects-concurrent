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
import           Control.Eff.Concurrent.Process
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
  :: forall r q p
   . (Typeable p, SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> ApiHandler p r
  -> Eff r ()
serve px handlers@(ApiHandler handleCast handleCall handleTerminate) =
   do mReq <- send (ReceiveMessage @q)
      case mReq of
        RetryLastAction -> serve px handlers
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
         sendMessage px fromPid (toDyn (Response (Proxy @p) reply))

unhandledCallError
  :: forall p x r q .
    ( Show (Api p ( 'Synchronous x))
    , Typeable p
    , HasCallStack
    , SetMember Process (Process q) r
    )
  => SchedulerProxy q
  -> Api p ( 'Synchronous x)
  -> (x -> Eff r Bool)
  -> Eff r ()
unhandledCallError px api _ = raiseError px
  ("Unhandled call: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep (Proxy @p)) ++ ")")

unhandledCastError
  :: forall p r q .
    ( Show (Api p 'Asynchronous)
    , Typeable p
    , HasCallStack
    , SetMember Process (Process q) r
    )
  => SchedulerProxy q
  -> Api p 'Asynchronous
  -> Eff r ()
unhandledCastError px api = raiseError px
  ("Unhandled cast: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep (Proxy @p)) ++ ")")
