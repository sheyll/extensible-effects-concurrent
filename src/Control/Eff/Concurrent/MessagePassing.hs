{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
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
module Control.Eff.Concurrent.MessagePassing
  ( ProcessId(..)
  , fromProcessId
  , Process(..)
  , self
  , trapExit
  , raiseError
  , catchProcessError
  , ignoreProcessError
  , MessagePassing(..)
  , Message (..)
  , sendMessage
  , sendShutdown
  , receiveMessage
  )
where

import           GHC.Stack
import           Control.Eff
import           Control.Lens
import           Data.Dynamic
import           Data.Kind
import           Data.Proxy
import           Text.Printf


-- * Process Effects

data Process b where
  SelfPid :: Process ProcessId
  TrapExit :: Bool -> Process Bool
  RaiseError :: String -> Process b
  --  LinkProcesses :: ProcessId -> ProcessId -> Process ()

self :: Member Process r => Eff r ProcessId
self = send SelfPid

trapExit :: Member Process r => Bool -> Eff r Bool
trapExit = send . TrapExit

raiseError :: Member Process r => String -> Eff r b
raiseError = send . RaiseError

catchProcessError
  :: forall r w . Member Process r => (String -> Eff r w) -> Eff r w -> Eff r w
catchProcessError onErr =
  interpose return go
   where
     go :: forall b . Process b -> (b -> Eff r w) -> Eff r w
     go (RaiseError emsg) _k = onErr emsg
     go s k = send s >>= k

ignoreProcessError :: (HasCallStack, Member Process r) => Eff r a -> Eff r (Either String a)
ignoreProcessError = catchProcessError (return . Left) . fmap Right

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


-- * MessagePassing Effect

data MessagePassing b where
  SendMessage :: Typeable m
              => ProcessId
              -> Message m
              -> MessagePassing Bool
  ReceiveMessage :: forall e m . (Typeable m, Typeable (Message m))
                 => (m -> e)
                 -> MessagePassing (Message e)

data Message m where
  Shutdown :: Message m
  Message :: m -> Message m
  deriving (Typeable, Functor, Show, Eq, Ord, Foldable, Traversable)

sendMessage
  :: forall o r
   . (HasCallStack, Member MessagePassing r, Typeable o)
  => ProcessId
  -> o
  -> Eff r Bool
sendMessage pid message = send (SendMessage pid (Message message))

sendShutdown :: forall (m :: Type) proxy r . (HasCallStack, Member MessagePassing r, Typeable m)
     => proxy m -> ProcessId -> Eff r Bool
sendShutdown _ pid = send (SendMessage pid (Shutdown @m))

receiveMessage
  :: forall o r . (HasCallStack, Member MessagePassing r, Member Process r, Typeable o)
    => Proxy o -> Eff r (Message o)
receiveMessage _ = send (ReceiveMessage id)
