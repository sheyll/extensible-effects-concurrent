-- | The message passing effect.
--
-- This module describes an abstract message passing effect, and a process
-- effect, mimicking Erlang's process and message semantics.
--
-- An implementation of a handler for these effects can be found in
-- 'Control.Eff.Concurrent.Dispatcher'.
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
  , getTrapExit
  , raiseError
  , catchProcessError
  , ignoreProcessError
  , MessagePassing(..)
  , Message(..)
  , sendMessage
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

-- | The process effect is the basis for message passing concurrency. This binds
-- the semantics of a process with a process-id, and some process flags, and the
-- ability to leave a process early with an error.
data Process b where
  SelfPid :: Process ProcessId
  TrapExit :: Bool -> Process ()
  GetTrapExit :: Process Bool
  RaiseError :: String -> Process b
  --  LinkProcesses :: ProcessId -> ProcessId -> Process ()

-- | Returns the 'ProcessId' of the current process.
self :: Member Process r => Eff r ProcessId
self = send SelfPid

-- | Set the flag that controls a process reaction to
-- exit messages from linked/monitored processes.
trapExit :: Member Process r => Bool -> Eff r ()
trapExit = send . TrapExit

-- | Return the 'trapExit' flag.
getTrapExit :: Member Process r => Eff r Bool
getTrapExit = send GetTrapExit

-- | Thrown an error, can be caught by 'catchProcessError'.
raiseError :: Member Process r => String -> Eff r b
raiseError = send . RaiseError

-- | Catch and handle an error raised by 'raiseError'. Works independent of the
-- handler implementation.
catchProcessError
  :: forall r w . Member Process r => (String -> Eff r w) -> Eff r w -> Eff r w
catchProcessError onErr = interpose return go
 where
  go :: forall b . Process b -> (b -> Eff r w) -> Eff r w
  go (RaiseError emsg) _k = onErr emsg
  go s                 k  = send s >>= k

-- | Like 'catchProcessError' it catches 'raiseError', but instead of invoking a
-- user provided handler, the result is wrapped into an 'Either'.
ignoreProcessError
  :: (HasCallStack, Member Process r) => Eff r a -> Eff r (Either String a)
ignoreProcessError = catchProcessError (return . Left) . fmap Right

-- | Each process is identified by a single process id, that stays constant
-- throughout the life cycle of a process. Also, message sending relies on these
-- values to address messages to processes.
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


-- * 'MessagePassing' Effect

-- | An effect for sending and receiving messages.
data MessagePassing b where
  -- | Send a message to a process addressed by the 'ProcessId'. Sending a
  -- message should **always succeed** and return **immediately**, even if the
  -- destination process does not exist, or does not accept messages of the
  -- given type.
  SendMessage :: ProcessId
              -> Dynamic
              -> MessagePassing Bool
  -- | Receive a message. This should block until an a message was received. The
  -- pure function may convert the incoming message into something, and the
  -- result is returned as 'Message' value. Another reason why this function
  -- returns, is if a process control message was sent to the process. This can
  -- only occur from inside the runtime system, aka the effect handler
  -- implementation. (Currently there is one in 'Control.Eff.Concurrent.Dispatcher'.)
  ReceiveMessage :: forall e . Typeable e => (Dynamic -> Maybe e)  -> MessagePassing (Message e)

-- | When a process invokes 'receiveMessage' a value of this type is returned.
-- There are more reasons that 'receiveMessage' might return, one is that a
-- message was sent to the process, another might be that in internal, handler
-- specific, event occurred for which the process should /wake-up/.
data Message m where
  ProcessControlMessage :: String -> Message m
  MessageIgnored :: Message m
  Message :: m -> Message m
  deriving (Typeable, Functor, Show, Eq, Ord, Foldable, Traversable)

-- | Send a message to a process addressed by the 'ProcessId'.
-- @see 'SendMessage'.
sendMessage
  :: forall o r
   . (HasCallStack, Member MessagePassing r, Typeable o)
  => ProcessId
  -> o
  -> Eff r Bool
sendMessage pid message = send (SendMessage pid (toDyn message))

-- | Block until a message was received. Expect a message of the type annotated
-- by the 'Proxy'.
-- Depending on 'trapExit', this will 'raiseError'.
-- @see 'ReceiveMessage'.
receiveMessage
  :: forall o r
   . (HasCallStack, Member MessagePassing r, Member Process r, Typeable o)
  => Proxy o
  -> Eff r (Message o)
receiveMessage px = do
  res <- send (ReceiveMessage fromDynamic)
  case res of
    Message  _                -> return res
    MessageIgnored            -> receiveMessage px
    ProcessControlMessage msg -> do
      isTrapExit <- getTrapExit
      if isTrapExit
        then return res
        else raiseError ("received exit message: " ++ msg)
