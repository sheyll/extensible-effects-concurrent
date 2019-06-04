-- | Proxies and containers for casts and calls.
--
-- @since 0.15.0
module Control.Eff.Concurrent.Protocol.Request
  ( Request(..)
  , sendReply
  , RequestOrigin(..)
  , embedRequestOrigin
  , toEmbeddedOrigin
  , Reply(..)
  , embedReplySerializer
  , makeRequestOrigin
  )
  where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Data.Functor.Contravariant
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
    :: forall protocol. (TangiblePdu protocol 'Asynchronous, NFData (Pdu protocol 'Asynchronous))
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
  Reply :: (Tangible reply) =>
    { _replyTo :: RequestOrigin protocol reply
    , _replyValue :: reply
    } -> Reply protocol reply
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
  { _requestOriginPid     :: !ProcessId
  , _requestOriginCallRef :: !Int
  } deriving (Typeable, Generic, Eq, Ord)

instance Show (RequestOrigin p r) where
  showsPrec d (RequestOrigin o r) =
    showParen (d >= 10) (showString "origin: " . showsPrec 10 o . showChar ' ' . showsPrec 10 r)

-- | Create a new, unique 'RequestOrigin' value for the current process.
--
-- @since 0.24.0
makeRequestOrigin
  :: ( Typeable r
     , NFData r
     , SetMember Process (Process q0) e
     , '[Interrupts] <:: e)
  => Eff e (RequestOrigin p r)
makeRequestOrigin = RequestOrigin <$> self <*> makeReference

instance NFData (RequestOrigin p r)

-- | Send a 'Reply' to a 'Call'.
--
-- The reply will be deeply evaluated to 'rnf'.
--
-- To send replies for 'EmbedProtocol' instances use 'embedReplySerializer'
-- and 'toEmbeddedOrigin'.
--
-- @since 0.15.0
sendReply ::
     (SetMember Process (Process q) eff, Member Interrupts eff, Tangible reply, Typeable protocol)
  => Serializer (Reply protocol reply) -> RequestOrigin protocol reply -> reply -> Eff eff ()
sendReply ser o r = sendAnyMessage (_requestOriginPid o) $! runSerializer ser $! Reply o r

-- | Turn an 'RequestOrigin' to an origin for an embedded request (See 'EmbedProtocol').
--
-- This is useful of a server delegates the @calls@ and @casts@ for an embedded protocol
-- to functions, that require the 'Serializer' and 'RequestOrigin' in order to call
-- 'sendReply'.
--
-- See also 'embedReplySerializer'.
--
-- @since 0.24.3
toEmbeddedOrigin
  :: EmbedProtocol outer inner
  => RequestOrigin outer reply
  -> RequestOrigin inner reply
toEmbeddedOrigin (RequestOrigin !pid !ref) = RequestOrigin pid ref

-- | Turn an /embedded/ 'RequestOrigin' to a 'RequestOrigin' for the /bigger/ request.
--
-- This is the inverse of 'toEmbeddedOrigin'.
--
-- This function is strict in all parameters.
--
-- @since 0.24.2
embedRequestOrigin :: EmbedProtocol outer inner => RequestOrigin inner reply -> RequestOrigin outer reply
embedRequestOrigin (RequestOrigin !pid !ref) = RequestOrigin pid ref

-- | Turn a 'Serializer' for a 'Pdu' instance that contains embedded 'Pdu' values
-- into a 'Reply' 'Serializer' for the embedded 'Pdu'.
--
-- This is useful of a server delegates the @calls@ and @casts@ for an embedded protocol
-- to functions, that require the 'Serializer' and 'RequestOrigin' in order to call
-- 'sendReply'.
--
-- See also 'toEmbeddedOrigin'.
--
-- @since 0.24.2
embedReplySerializer :: EmbedProtocol outer inner => Serializer (Reply outer reply) -> Serializer (Reply inner reply)
embedReplySerializer = contramap embedReply

-- | Turn an /embedded/ 'Reply' to a 'Reply' for the /bigger/ request.
--
-- This function is strict in all parameters.
--
-- @since 0.24.2
embedReply :: EmbedProtocol outer inner => Reply inner reply -> Reply outer reply
embedReply (Reply (RequestOrigin !pid !ref) !v) = Reply (RequestOrigin pid ref) v
