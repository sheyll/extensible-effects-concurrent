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
data family Api (genServerModule :: Type) (replyType :: Synchronicity)

data Synchronicity =
  Synchronous Type | Asynchronous
    deriving (Typeable)

newtype Server genServerModule = Server { _fromServer :: ProcessId }
  deriving (Eq,Ord,Typeable)

instance Read (Server genServerModule) where
  readsPrec _ ('[':'#':rest1) =
    case reads (dropWhile (/= '#') rest1) of
      [(c, ']':rest2)] -> [(Server c, rest2)]
      _ -> []
  readsPrec _ _ = []

instance Typeable genServerModule => Show (Server genServerModule) where
  show s@(Server c) =
    "[#" ++ show (typeRep s) ++ "#" ++ show c ++ "]"

makeLenses ''Server

proxyAsServer :: proxy genServerModule -> ProcessId -> Server genServerModule
proxyAsServer = const Server

asServer :: forall genServerModule . ProcessId -> Server genServerModule
asServer = Server

data Request genServerModule where
  Call :: forall genServerModule apiCallReplyType . (Typeable genServerModule, Typeable apiCallReplyType, Typeable (Api genServerModule ('Synchronous apiCallReplyType)))
         => ProcessId -> Api genServerModule ('Synchronous apiCallReplyType) -> Request genServerModule
  Cast :: forall genServerModule . (Typeable genServerModule, Typeable (Api genServerModule 'Asynchronous))
         => Api genServerModule 'Asynchronous -> Request genServerModule
  deriving Typeable

data Response genServerModule apiCallReplyType where
  Response :: (Typeable genServerModule, Typeable apiCallReplyType) => Proxy genServerModule -> apiCallReplyType -> Response genServerModule apiCallReplyType
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
  :: forall result genServerModule r
   . ( Member MessagePassing r
     , Member Process r
     , Typeable genServerModule
     , Typeable (Api genServerModule ( 'Synchronous result))
     , Typeable result
     , HasCallStack
     )
  => Server genServerModule
  -> Api genServerModule ( 'Synchronous result)
  -> Eff r (Message result)
call (Server pidInt) req = do
  fromPid <- self
  let requestMessage = Call fromPid req
  wasSent <- sendMessage pidInt requestMessage
  if wasSent
    then
      let extractResult :: Response genServerModule result -> result
          extractResult (Response _pxResult result) = result
      in  do
            mResp <- receiveMessage (Proxy @(Response genServerModule result))
            return (extractResult <$> mResp)
    else raiseError
      ("Could not send request message " ++ show (typeRep requestMessage))

data ApiHandler p r e where
  ApiHandler ::
     { _handleCast
         :: (Typeable p, Typeable (Api p 'Asynchronous), HasCallStack)
         => Api p 'Asynchronous -> Eff r e
     , _handleCall
         :: forall x . (Typeable p, Typeable (Api p ('Synchronous x)), Typeable x, HasCallStack)
         => Api p ('Synchronous x) -> (x -> Eff r Bool) -> Eff r e
     , _handleTerminate
         :: (Typeable p, HasCallStack)
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
  :: ( Show (Api p ( 'Synchronous x))
     , Typeable p
     , Typeable (Api p ( 'Synchronous x))
     , Typeable x
     , HasCallStack
     , Member Process r
     )
  => Api p ( 'Synchronous x)
  -> (x -> Eff r Bool)
  -> Eff r e
unhandledCallError api _ = raiseError
  ("Unhandled call: (" ++ show api ++ " :: " ++ show (typeRep api) ++ ")")

unhandledCastError
  :: ( Show (Api p 'Asynchronous)
     , Typeable p
     , Typeable (Api p 'Asynchronous)
     , HasCallStack
     , Member Process r
     )
  => Api p 'Asynchronous
  -> Eff r e
unhandledCastError api = raiseError
  ("Unhandled cast: (" ++ show api ++ " :: " ++ show (typeRep api) ++ ")")
