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
{-# LANGUAGE GADTs #-}

-- | Type safe /server/ API processes

module Control.Eff.Processes.Server
  ( Api
  , Server(..)
  , fromServer
  , proxyAsServer
  , asServer
  , cast
  , cast_
  , call
  , serve
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

import           Control.Eff.Processes

-- | This data family defines an API implemented by a server.
-- The first parameter is the API /index/ and the second parameter
-- (the @* -> *@)
data family Api o :: * -> *

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
  Call :: forall p x . (Typeable p, Typeable x, Typeable (Api p x)) => Maybe ProcessId -> Api p x -> Request p
  Cast :: forall p x . (Typeable p, Typeable x, Typeable (Api p x)) => Api p x -> Request p
  deriving Typeable

data Response p x where
  Response :: (Typeable p, Typeable x) => Proxy p -> x -> Response p x
  deriving Typeable

cast
  :: forall r o result
   . (HasCallStack, Member Process r, Typeable o, Typeable result, Typeable (Api o result))
  => Server o
  -> Api o result
  -> Eff r Bool
cast (Server pid) callMsg = sendMessage pid (Cast callMsg)

cast_
  :: forall r o result
   . (HasCallStack, Member Process r, Typeable o, Typeable result, Typeable (Api o result))
  => Server o
  -> Api o result
  -> Eff r ()
cast_ = ((.).(.)) void cast

call
  :: forall result o r
   . ( Member Process r
     , Typeable o
     , Typeable (Api o result)
     , Typeable result
     , HasCallStack
     )
  => Server o
  -> Api o result
  -> Eff r (Maybe result)
call (Server pidInt) req = do
  fromPid <- self
  let requestMessage = Call (Just fromPid) req
  wasSent <- sendMessage pidInt requestMessage
  if wasSent
    then
      let responseMessage :: Response o result -> result
          responseMessage (Response _pxReply reply) = reply
      in  send (ReceiveMessage responseMessage)
    else fail "Could not send request message " >> return Nothing

serve
  :: forall r p e
   . (Member Process r, Typeable p, HasCallStack)
  => (  forall x
      . (Typeable (Api p x), Typeable x)
     => Api p x
     -> (x -> Maybe (Process Bool))
     -> (e, Maybe (Process Bool))
     )
  -> Eff r (Maybe e)
serve handle = do
  mrequest <- send (ReceiveMessage receiveCallReq)
  case mrequest of
    Just (result, mReplyCast) -> do
      mapM_ (void . send) mReplyCast
      return (Just result)
    _ -> fail "sdf4446" >> return Nothing
 where
  receiveCallReq :: Request p -> (e, Maybe (Process Bool))
  receiveCallReq (Cast request         ) = handle request (const Nothing)
  receiveCallReq (Call mFromPid request) = handle request (packReply request)
   where
    packReply :: Typeable x => Api p x -> x -> Maybe (Process Bool)
    packReply _ reply = do
      fromPid <- mFromPid
      return
        (SendMessage fromPid (Response (Proxy :: Proxy p) reply))
