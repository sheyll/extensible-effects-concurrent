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
{-# LANGUAGE FlexibleInstances #-}
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
  , ResumeProcess(..)
  , yieldAndCatchProcess
  , sendMessage
  , receiveMessage
  , receiveMessageAs
  , self
  , raiseError
  , exitWithError
  , exitNormally
  , catchProcessError
  , ignoreProcessError
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
  -- | Return the current 'ProcessId'
  SelfPid :: Process (ResumeProcess ProcessId)
  -- | Process exit, this is the same as if the function that was applied to a
  -- spawn function returned.
  Shutdown :: Process a
  -- | Exit the process due to an error, this cannot be caught.
  ExitWithError :: String -> Process b
  -- | Raise an error, that can be handled.
  RaiseError :: String -> Process b
  --  LinkProcesses :: ProcessId -> ProcessId -> Process ()
  -- | Send a message to a process addressed by the 'ProcessId'. Sending a
  -- message should **always succeed** and return **immediately**, even if the
  -- destination process does not exist, or does not accept messages of the
  -- given type.
  SendMessage :: ProcessId -> Dynamic -> Process (ResumeProcess Bool)
  -- | Receive a message. This should block until an a message was received. The
  -- pure function may convert the incoming message into something, and the
  -- result is returned as 'ProcessMessage' value. Another reason why this function
  -- returns, is if a process control message was sent to the process. This can
  -- only occur from inside the runtime system, aka the effect handler
  -- implementation. (Currently there is one in 'Control.Eff.Concurrent.Dispatcher'.)
  ReceiveMessage :: Process (ResumeProcess Dynamic)

-- | Every 'Process' action returns it's actual result wrapped in this type. It
-- will allow to signal errors as well as pass on normal results such as
-- incoming messages.
data ResumeProcess v where
  -- | The process is required to exit.
  ShutdownRequested :: ResumeProcess v
  -- | The process is required to exit from an error condition, that cannot be
  -- recovered from.
  OnError :: String -> ResumeProcess v
  -- | The process may resume to do work, using the given result.
  ResumeWith :: a -> ResumeProcess a
  -- | This indicates that the action did not complete, and maybe retried
  RetryLastAction :: ResumeProcess v
  deriving (Typeable, Foldable, Functor, Show, Eq, Ord, Traversable)

-- | Execute a 'Process' action and resume the process, retry the action or exit
-- the process depending on the 'ResumeProcess' clause.
yieldProcess :: (Member Process r, HasCallStack)
             => Process (ResumeProcess v) -> Eff r v
yieldProcess processAction =
  do result <- send processAction
     case result of
       ResumeWith value -> return value
       RetryLastAction -> yieldProcess processAction
       ShutdownRequested -> send Shutdown
       OnError e -> send (ExitWithError e)

-- | Execute a and action and resume the process, retry the action, shutdown the process or return an error.
yieldAndCatchProcess :: (Member Process r, HasCallStack)
                     => Eff r (ResumeProcess v) -> Eff r (Either String v)
yieldAndCatchProcess processAction =
  do result <- processAction
     case result of
       ResumeWith value -> return (Right value)
       RetryLastAction -> yieldAndCatchProcess processAction
       ShutdownRequested -> send Shutdown
       OnError e -> return (Left e)

-- | Send a message to a process addressed by the 'ProcessId'.
-- @see 'SendMessage'.
sendMessage
  :: forall r
   . (HasCallStack, Member Process r)
  => ProcessId
  -> Dynamic
  -> Eff r Bool
sendMessage pid message =
  yieldProcess (SendMessage pid message)

-- | Block until a message was received.
receiveMessage
  :: forall r
   . (HasCallStack, Member Process r)
  => Eff r Dynamic
receiveMessage = do
  yieldProcess ReceiveMessage

-- | Receive and cast the message to some 'Typeable' instance.
receiveMessageAs
  :: forall r a . (HasCallStack, Typeable a, Member Process r)
  => Eff r a
receiveMessageAs =
  do messageDynamic <- receiveMessage
     let castAndCheck dm =
          case fromDynamic dm of
           Nothing ->
             Left ("Invalid message type received: " ++ show dm)
           Just m ->
             Right m
         maybeMessage = castAndCheck messageDynamic
     either raiseError return maybeMessage

-- | Returns the 'ProcessId' of the current process.
self :: (HasCallStack, Member Process r) => Eff r ProcessId
self = yieldProcess SelfPid

-- | Exit the process.
exitNormally :: (HasCallStack, Member Process r) => Eff r a
exitNormally = send Shutdown

-- | Exit the process with an error.
exitWithError :: (HasCallStack, Member Process r) => String -> Eff r a
exitWithError = send . RaiseError

-- | Thrown an error, can be caught by 'catchProcessError'.
raiseError :: (HasCallStack, Member Process r) => String -> Eff r b
raiseError = send . ExitWithError

-- | Catch and handle an error raised by 'raiseError'. Works independent of the
-- handler implementation.
catchProcessError
  :: forall r w . (HasCallStack, Member Process r) => (String -> Eff r w) -> Eff r w -> Eff r w
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
