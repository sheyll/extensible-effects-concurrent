{-# LANGUAGE UndecidableInstances #-}

-- | Proxies and containers for casts and calls.
--
-- @since 0.15.0
module Control.Eff.Concurrent.Protocol.Request
  ( Request(..)
  , Reply(..)
  , makeRequestOrigin
  , RequestOrigin(..)
  , sendReply
  ) where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Generics

-- | A wrapper sum type for calls and casts for the 'Pdu's of a protocol
--
-- @since 0.15.0
data Request protocol where
  Call
    :: forall protocol reply.
       ( Tangible reply
       , TangiblePdu protocol ('Synchronous reply)
       )
    => RequestOrigin protocol reply
    -> Pdu protocol ('Synchronous reply)
    -> Request protocol
  Cast
    :: forall protocol. (TangiblePdu protocol 'Asynchronous)
    => Pdu protocol 'Asynchronous
    -> Request protocol
  deriving (Typeable)

instance Show (Request protocol) where
  showsPrec d (Call o r) =
    showParen (d >= 10) (showString "call-request: " . showsPrec 11 o . showString ": " . showsPrec 11 r)
  showsPrec d (Cast r) =
    showParen (d >= 10) (showString "cast-request: " . showsPrec 11 r)

instance NFData (Request protocol) where
  rnf (Call o req) = rnf o `seq` rnf req
  rnf (Cast req) = rnf req

-- | The wrapper around replies to 'Call's.
--
-- @since 0.15.0
data Reply protocol reply where
  Reply :: (Tangible reply) => RequestOrigin protocol reply -> reply -> Reply protocol reply
  deriving (Typeable)

instance NFData (Reply p r) where
  rnf (Reply i r) = rnf i `seq` rnf r

instance Show r => Show (Reply p r) where
  showsPrec d (Reply o r) =
    showParen (d >= 10) (showString "request-reply: " . showsPrec 11 o . showString ": " . showsPrec 11 r)

-- | Wraps the source 'ProcessId' and a unique identifier for a 'Call'.
--
-- @since 0.15.0
data RequestOrigin (proto :: Type) reply = RequestOrigin
  { _requestOriginPid :: !ProcessId
  , _requestOriginCallRef :: !Int
  } deriving (Eq, Ord, Typeable, Generic)

instance Show (RequestOrigin p r) where
  showsPrec d (RequestOrigin o r) =
    showParen (d >= 10) (showString "origin: " . showsPrec 11 o . showChar ' ' . showsPrec 11 r)

-- | Create a new, unique 'RequestOrigin' value for the current process.
--
-- @since 0.24.0
makeRequestOrigin :: (SetMember Process (Process q0) e, '[Interrupts] <:: e) => Eff e (RequestOrigin p r)
makeRequestOrigin = RequestOrigin <$> self <*> makeReference

instance NFData (RequestOrigin p r)

-- | Send a 'Reply' to a 'Call'.
--
-- The reply will be deeply evaluated to 'rnf'.
--
-- @since 0.15.0
sendReply ::
     (SetMember Process (Process q) eff, Member Interrupts eff, Tangible reply, Typeable protocol)
  => RequestOrigin protocol reply
  -> reply
  -> Eff eff ()
sendReply origin reply = sendMessage (_requestOriginPid origin) (Reply origin $! force reply)
