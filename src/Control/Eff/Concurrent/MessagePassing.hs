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
module Control.Eff.Concurrent.MessagePassing
  ( ProcessId(..)
  , fromProcessId
  , Process(..)
  , self
  , MessagePassing(..)
  , sendMessage
  , kill
  , receiveMessage
  )
where

import           GHC.Stack
import           Control.Eff
import           Control.Lens
import           Data.Dynamic
import           Data.Proxy
import           Data.Void
import           Text.Printf


-- * Process Effects

data Process b where
  SelfPid :: Process ProcessId
  --  LinkProcesses :: ProcessId -> ProcessId -> Process ()

self :: Member Process r => Eff r ProcessId
self = send SelfPid

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
  ReceiveMessage
          :: forall e m . (Typeable m, Typeable (Message m))
          => (Message m -> e)
          -> MessagePassing (Maybe e)

data Message m where
  Shutdown :: Message Void
  Message :: m -> Message m
  deriving Typeable

sendMessage
  :: forall o r
   . (HasCallStack, Member MessagePassing r, Typeable o)
  => ProcessId
  -> o
  -> Eff r Bool
sendMessage pid message = send (SendMessage pid (Message message))

kill :: (HasCallStack, Member MessagePassing r)
     => ProcessId -> Eff r Bool
kill pid = send (SendMessage pid Shutdown)

receiveMessage
  :: forall o r . (HasCallStack, Member MessagePassing r, Member Process r, Typeable o)
    => Proxy o -> Eff r (Maybe o)
receiveMessage _ = do
  me   <- self
  mRes <- send (ReceiveMessage id)
  case mRes of
    Just Shutdown -> fail (show me ++ " SHUTDOWN")
    Just (Message m) -> return (Just m)
    Nothing -> return Nothing
