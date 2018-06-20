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

module Control.Eff.Concurrent.Api
  ( Api
  , Synchronicity(..)
  , Server(..)
  , fromServer
  , proxyAsServer
  , asServer
  )
where

import           GHC.Stack
import           Data.Kind
import           Control.Eff
import           Control.Lens
import           Control.Monad
import           Data.Typeable (Typeable, typeRep)
import           Data.Proxy
import           Control.Eff.Reader.Lazy
import           Control.Eff.Concurrent.Process

-- | This data family defines an API implemented by a server.
-- The first parameter is the API /index/ and the second parameter
-- (the @* -> *@)
data family Api (api :: Type) (reply :: Synchronicity)

data Synchronicity = Synchronous Type | Asynchronous
    deriving (Typeable)

newtype Server api = Server { _fromServer :: ProcessId }
  deriving (Eq,Ord,Typeable)

instance Read (Server api) where
  readsPrec _ ('[':'#':rest1) =
    case reads (dropWhile (/= '#') rest1) of
      [(c, ']':rest2)] -> [(Server c, rest2)]
      _ -> []
  readsPrec _ _ = []

instance Typeable api => Show (Server api) where
  show s@(Server c) =
    "[#" ++ show (typeRep s) ++ "#" ++ show c ++ "]"

makeLenses ''Server

proxyAsServer :: proxy api -> ProcessId -> Server api
proxyAsServer = const Server

asServer :: forall api . ProcessId -> Server api
asServer = Server
