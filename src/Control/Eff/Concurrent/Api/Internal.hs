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

module Control.Eff.Concurrent.Api.Internal
  ( Request (..)
  , Response (..)
  )
where

import           GHC.Stack
import           Data.Kind
import           Control.Eff
import           Control.Lens
import           Control.Monad
import           Data.Typeable (Typeable, typeRep)
import           Data.Proxy

import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.MessagePassing


data Request api where
  Call :: forall api apiCallReplyType . (Typeable api, Typeable apiCallReplyType, Typeable (Api api ('Synchronous apiCallReplyType)))
         => ProcessId -> Api api ('Synchronous apiCallReplyType) -> Request api
  Cast :: forall api . (Typeable api, Typeable (Api api 'Asynchronous))
         => Api api 'Asynchronous -> Request api
  deriving Typeable

data Response api apiCallReplyType where
  Response :: (Typeable api, Typeable apiCallReplyType) => Proxy api -> apiCallReplyType -> Response api apiCallReplyType
  deriving Typeable
