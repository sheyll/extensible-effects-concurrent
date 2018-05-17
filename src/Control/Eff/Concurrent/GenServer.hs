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

module Control.Eff.Concurrent.GenServer
  ( Api
  , Synchronicity(..)
  , ApiHandler(ApiHandler)
  , Server(..)
  , fromServer
  , proxyAsServer
  , asServer
  , cast
  , cast_
  , call
  , serve
  , serve_
  , unhandledCallError
  , unhandledCastError
  )
where

import           GHC.Stack
import           Data.Kind
import           Control.Eff
import           Control.Lens
import           Control.Monad
import           Data.Dynamic
import           Data.Typeable                  ( typeRep )
import           Data.Proxy

import           Control.Eff.Concurrent.MessagePassing

-- | This data family defines an API implemented by a server.
-- The first parameter is the API /index/ and the second parameter
-- (the @* -> *@)
data family Api o :: Synchronicity -> *

data Synchronicity =
  Synchronous Type | Asynchronous
    deriving (Typeable)

newtype Server o = Server { _fromServer :: ProcessId }
  deriving (Eq,Ord,Typeable)

instance Read (Server o) where
  readsPrec _ ('[':'#':rest1) =
    case reads (dropWhile (/= '⇒') rest1) of
      [(c, ']':rest2)] -> [(Server c, rest2)]
      _ -> []
  readsPrec _ _ = []

instance Typeable o => Show (Server o) where
  show s@(Server c) =
    "[#" ++ show (typeRep s) ++ "⇒" ++ show c ++ "]"

makeLenses ''Server

proxyAsServer :: proxy api -> ProcessId -> Server api
proxyAsServer = const Server

asServer :: ProcessId -> Server api
asServer = Server

data Request p where
  Call :: forall p x . (Typeable p, Typeable x, Typeable (Api p ('Synchronous x)))
         => ProcessId -> Api p ('Synchronous x) -> Request p
  Cast :: forall p . (Typeable p, Typeable (Api p 'Asynchronous))
         => Api p 'Asynchronous -> Request p
  deriving Typeable

data Response p x where
  Response :: (Typeable p, Typeable x) => Proxy p -> x -> Response p x
  deriving Typeable

cast
  :: forall r o
   . ( HasCallStack
     , Member MessagePassing r
     , Typeable o
     , Typeable (Api o 'Asynchronous)
     )
  => Server o
  -> Api o 'Asynchronous
  -> Eff r Bool
cast (Server pid) callMsg = sendMessage pid (Cast callMsg)

cast_
  :: forall r o
   . ( HasCallStack
     , Member MessagePassing r
     , Typeable o
     , Typeable (Api o 'Asynchronous)
     )
  => Server o
  -> Api o 'Asynchronous
  -> Eff r ()
cast_ = ((.) . (.)) void cast

call
  :: forall result o r
   . ( Member MessagePassing r
     , Member Process r
     , Typeable o
     , Typeable (Api o ('Synchronous result))
     , Typeable result
     , HasCallStack
     )
  => Server o
  -> Api o ('Synchronous result)
  -> Eff r (Maybe result)
call (Server pidInt) req = do
  fromPid <- self
  let requestMessage = Call fromPid req
  wasSent <- sendMessage pidInt requestMessage
  if wasSent
    then
      let extractResult :: Response o result -> result
          extractResult (Response _pxResult result) = result
      in do mResp <- receiveMessage (Proxy @(Response o result))
            return (extractResult <$> mResp)
    else fail "Could not send request message " >> return Nothing

data ApiHandler p r e where
  ApiHandler ::
     { _handleCast
         :: (Typeable p, Typeable (Api p 'Asynchronous), HasCallStack)
         => Api p 'Asynchronous -> Eff r e
     , _handleCall
         :: forall x . (Typeable p, Typeable (Api p ('Synchronous x)), Typeable x, HasCallStack)
         => Api p ('Synchronous x) -> (x -> Eff r Bool) -> Eff r e
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
  -> Eff r (Maybe e)
serve (ApiHandler handleCast handleCall) = do
  mReq <- receiveMessage (Proxy @(Request p))
  mapM receiveCallReq mReq
 where
  receiveCallReq :: Request p -> Eff r e
  receiveCallReq (Cast request        ) = handleCast request
  receiveCallReq (Call fromPid request) = handleCall request
                                                     (sendReply request)
   where
    sendReply :: Typeable x => Api p ('Synchronous x) -> x -> Eff r Bool
    sendReply _ reply = sendMessage fromPid (Response (Proxy :: Proxy p) reply)

unhandledCallError
  :: ( Show (Api p ('Synchronous x))
     , Typeable p
     , Typeable (Api p ('Synchronous x))
     , Typeable x
     , HasCallStack
     , Member Process r
     )
  => Api p ('Synchronous x)
  -> (x -> Eff r Bool)
  -> Eff r e
unhandledCallError api _ = do
  me <- self
  fail
    (  show me
    ++ " Unhandled call: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep api)
    ++ ")"
    )

unhandledCastError
  :: ( Show (Api p 'Asynchronous)
     , Typeable p
     , Typeable (Api p 'Asynchronous)
     , HasCallStack
     , Member Process r
     )
  => Api p 'Asynchronous
  -> Eff r e
unhandledCastError api = do
  me <- self
  fail
    (  show me
    ++ " Unhandled cast: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep api)
    ++ ")"
    )
