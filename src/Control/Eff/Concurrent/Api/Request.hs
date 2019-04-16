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
  Call :: forall api reply . (Typeable api, Typeable reply, NFData reply, Typeable (Api api ('Synchronous reply)), NFData (Api api ('Synchronous reply)))
         => Int -> ProcessId -> Api api ('Synchronous reply) -> Request api

  Cast :: forall api . (Typeable api, Typeable (Api api 'Asynchronous), NFData (Api api 'Asynchronous ))
         => Api api 'Asynchronous -> Request api
  deriving Typeable

instance NFData (Request api) where
  rnf (Call i p req) = rnf i `seq` rnf p `seq` rnf req
  rnf (Cast req)     = rnf req

-- | The wrapper around replies to 'Call's.
--
-- @since 0.15.0
data Reply request where
  Reply :: (Typeable api, Typeable reply, NFData reply)
        => Proxy (Api api ('Synchronous reply)) -> Int -> reply -> Reply (Api api ('Synchronous reply))
  deriving Typeable

instance NFData (Reply request) where
  rnf (Reply _ i r) = rnf i `seq` rnf r


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
  deriving (Eq, Ord, Typeable, Generic)

instance Show (RequestOrigin r) where
  showsPrec d (RequestOrigin o r) =
    showParen (d >= 10) (showString "caller: " . shows o . showChar ' ' . shows r)

instance NFData (RequestOrigin request) where

-- | Send a 'Reply' to a 'Call'.
--
-- The reply will be deeply evaluated to 'rnf'.
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
     , NFData reply
     )
  => RequestOrigin request
  -> reply
  -> Eff eff ()
sendReply origin reply = sendMessage
  (_requestOriginPid origin)
  (Reply (Proxy @request) (_requestOriginCallRef origin) $! force reply)
