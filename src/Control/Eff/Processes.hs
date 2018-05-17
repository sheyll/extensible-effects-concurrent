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
module Control.Eff.Processes
  ( ProcessId(..)
  , self
  , sendMessage
  , receiveMessage
  , Process(..)
  , fromProcessId
  )
where

import           Control.Eff
import           Control.Lens
import           Data.Dynamic
import           Data.Proxy
import           Text.Printf

-- * Process Types

newtype ProcessId = ProcessId { _fromProcessId :: Int }
  deriving (Eq,Ord,Typeable,Bounded,Num, Enum, Integral, Real)

instance Read ProcessId where
  readsPrec _ ('<':'0':'.':rest1) =
    case reads rest1 of
      [(c, '.':'0':'>':rest2)] -> [(ProcessId c, rest2)]
      _ -> []
  readsPrec _ _ = []

instance Show ProcessId where
  show (ProcessId c) =
    printf "<0.%d.0>" c

makeLenses ''ProcessId

-- * Process Effect

data Process b where
  SendMessage :: Typeable m
          => ProcessId
          -> m
          -> Process Bool
  ReceiveMessage
          :: forall e m . (Typeable m)
          => (m -> e)
          -> Process (Maybe e)
  SelfPid :: Process ProcessId

-- * Process Effects

self :: Member Process r => Eff r ProcessId
self = send SelfPid

sendMessage
  :: forall o r . (Member Process r, Typeable o) => ProcessId -> o -> Eff r Bool
sendMessage pid message = send (SendMessage pid message)

receiveMessage
  :: forall o r . (Member Process r, Typeable o) => Proxy o -> Eff r (Maybe o)
receiveMessage _ = send (ReceiveMessage id)
