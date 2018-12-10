{-# LANGUAGE UndecidableInstances #-}
-- | Internal request and response type for casts and calls

module Control.Eff.Concurrent.Api.Internal
  ( Request(..)
  , Reply(..)
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

data Request api where
  Call :: forall api reply . (Typeable api, Typeable reply, Typeable (Api api ('Synchronous reply)))
         => Int -> ProcessId -> Api api ('Synchronous reply) -> Request api

  Cast :: forall api . (Typeable api, Typeable (Api api 'Asynchronous))
         => Api api 'Asynchronous -> Request api
  deriving Typeable

data Reply request where
  Reply :: (Typeable api, Typeable reply) => Proxy (Api api ('Synchronous reply)) -> Int -> reply -> Reply (Api api ('Synchronous reply))
  deriving Typeable


-- | TODO doc
type family ReplyType request where
  ReplyType (Api api ('Synchronous reply)) = reply
  ReplyType (Api api 'Asynchronous) = TypeError ('Text "Asynchronous requests (aka casts) have no reply type." )

-- | TODO doc
type family ApiType request where
  ApiType (Api api 'Asynchronous) = api
  ApiType (Api api ('Synchronous reply)) = api
