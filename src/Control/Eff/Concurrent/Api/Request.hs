{-# LANGUAGE UndecidableInstances #-}
-- | Proxies and containers for casts and calls.
--
-- @since 0.15.0
module Control.Eff.Concurrent.Api.Request
  ( Request(..)
  , Reply(..)
  , mkRequestOrigin
  , RequestOrigin(..)
  , sendReply
  )
where

import           Data.Typeable                  ( Typeable )
import           Data.Proxy
import           Control.Eff
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Process
import           GHC.TypeLits
import           Control.DeepSeq
import           GHC.Generics

-- | A wrapper sum type for calls and casts for the methods of an 'Api' subtype
--
-- @since 0.15.0
data Request api where
  Call :: forall api reply . (Typeable api, Typeable reply, Typeable (Api api ('Synchronous reply)))
         => Int -> ProcessId -> Api api ('Synchronous reply) -> Request api

  Cast :: forall api . (Typeable api, Typeable (Api api 'Asynchronous))
         => Api api 'Asynchronous -> Request api
  deriving Typeable

-- | The wrapper around replies to 'Call's.
--
-- @since 0.15.0
data Reply request where
  Reply :: (Typeable api, Typeable reply) => Proxy (Api api ('Synchronous reply)) -> Int -> reply -> Reply (Api api ('Synchronous reply))
  deriving Typeable

-- | Get the @reply@ of an @Api foo ('Synchronous reply)@.
--
-- @since 0.15.0
type family ReplyType request where
  ReplyType (Api api ('Synchronous reply)) = reply
  ReplyType (Api api 'Asynchronous) = TypeError ('Text "Asynchronous requests (aka casts) have no reply type." )

-- | Get the @reply@ of an @Api foo ('Synchronous reply)@.
--
-- @since 0.15.0
type family ApiType request where
  ApiType (Api api 'Asynchronous) = api
  ApiType (Api api ('Synchronous reply)) = api

-- | TODO remove
mkRequestOrigin :: request -> ProcessId -> Int -> RequestOrigin request
mkRequestOrigin _ = RequestOrigin

-- | Wraps the source 'ProcessId' and a unique identifier for a 'Call'.
--
-- @since 0.15.0
data RequestOrigin request =
  RequestOrigin { _requestOriginPid :: !ProcessId, _requestOriginCallRef :: !Int}
  deriving (Eq, Ord, Typeable, Show, Generic)

instance NFData (RequestOrigin request) where

-- | Send a 'Reply' to a 'Call'.
--
-- @since 0.15.0
sendReply
  :: forall request reply api eff q
   . ( SetMember Process (Process q) eff
     , Member Interrupts eff
     , Typeable api
     , ApiType request ~ api
     , ReplyType request ~ reply
     , request ~ Api api ( 'Synchronous reply)
     , Typeable reply
     )
  => RequestOrigin request
  -> reply
  -> Eff eff ()
sendReply origin reply = sendMessage
  (_requestOriginPid origin)
  (Reply (Proxy @request) (_requestOriginCallRef origin) $! reply)
