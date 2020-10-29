{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

-- | Proxies and containers for casts and calls.
--
-- @since 0.15.0
module Control.Eff.Concurrent.Protocol.Wrapper
  ( Request (..),
    sendReply,
    ReplyTarget (..),
    replyTarget,
    replyTargetOrigin,
    replyTargetSerializer,
    embeddedReplyTarget,
    toEmbeddedReplyTarget,
    RequestOrigin (..),
    embedRequestOrigin,
    toEmbeddedOrigin,
    Reply (..),
    embedReplySerializer,
    makeRequestOrigin,
  )
where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import Control.Eff.Log.Message
import Control.Lens
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Semigroup
import Data.Typeable (Typeable)
import GHC.Generics

-- | A wrapper sum type for calls and casts for the 'Pdu's of a protocol
--
-- @since 0.15.0
data Request protocol where
  Call ::
    forall protocol reply.
    ( Tangible reply,
      TangiblePdu protocol ('Synchronous reply),
      ToLogMsg (Pdu protocol ('Synchronous reply))
    ) =>
    RequestOrigin protocol reply ->
    Pdu protocol ('Synchronous reply) ->
    Request protocol
  Cast ::
    forall protocol.
    ( TangiblePdu protocol 'Asynchronous,
      NFData (Pdu protocol 'Asynchronous),
      ToLogMsg (Pdu protocol 'Asynchronous)
    ) =>
    Pdu protocol 'Asynchronous ->
    Request protocol
  deriving (Typeable)

instance ToTypeLogMsg protocol => ToLogMsg (Request protocol) where
  toLogMsg = \case
    Call orig pdu -> packLogMsg "call from: " <> toLogMsg orig <> packLogMsg " pdu: " <> toLogMsg pdu
    Cast pdu -> packLogMsg "cast pdu: " <> toLogMsg pdu

instance NFData (Request protocol) where
  rnf (Call o req) = rnf o `seq` rnf req
  rnf (Cast req) = rnf req

-- | The wrapper around replies to 'Call's.
--
-- @since 0.15.0
data Reply protocol reply where
  Reply ::
    (Tangible reply) =>
    { _replyTo :: RequestOrigin protocol reply,
      _replyValue :: reply
    } ->
    Reply protocol
      reply
  deriving (Typeable)

instance NFData (Reply p r) where
  rnf (Reply i r) = rnf i `seq` rnf r

instance (ToLogMsg r, ToTypeLogMsg p) => ToLogMsg (Reply p r) where
  toLogMsg rp =
    packLogMsg "reply: "
      <> toLogMsg (_replyValue rp)
      <> packLogMsg " to: "
      <> toLogMsg (_replyTo rp)

-- | Wraps the source 'ProcessId' and a unique identifier for a 'Call'.
--
-- @since 0.15.0
data RequestOrigin (proto :: Type) reply
  = RequestOrigin
      { _requestOriginPid :: !ProcessId,
        _requestOriginCallRef :: !Int
      }
  deriving (Typeable, Generic, Eq, Ord)

instance ToTypeLogMsg p => ToLogMsg (RequestOrigin p r) where
  toLogMsg ro =
    toLogMsg (Endpoint @p (_requestOriginPid ro))
      <> packLogMsg ('?' : show (_requestOriginCallRef ro))

-- | Create a new, unique 'RequestOrigin' value for the current process.
--
-- @since 0.24.0
makeRequestOrigin :: HasProcesses e q0 => Eff e (RequestOrigin p r)
makeRequestOrigin = RequestOrigin <$> self <*> makeReference

instance NFData (RequestOrigin p r)

-- | Turn an 'RequestOrigin' to an origin for an embedded request (See 'Embeds').
--
-- This is useful of a server delegates the @calls@ and @casts@ for an embedded protocol
-- to functions, that require the 'Serializer' and 'RequestOrigin' in order to call
-- 'sendReply'.
--
-- See also 'embedReplySerializer'.
--
-- @since 0.24.3
toEmbeddedOrigin ::
  forall outer inner reply.
  RequestOrigin outer reply ->
  RequestOrigin inner reply
toEmbeddedOrigin (RequestOrigin !pid !ref) = RequestOrigin pid ref

-- | Turn an /embedded/ 'RequestOrigin' to a 'RequestOrigin' for the /bigger/ request.
--
-- This is the inverse of 'toEmbeddedOrigin'.
--
-- This function is strict in all parameters.
--
-- @since 0.24.2
embedRequestOrigin ::
  forall outer inner reply.
  RequestOrigin inner reply ->
  RequestOrigin outer reply
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
embedReplySerializer ::
  forall outer inner reply.
  Serializer (Reply outer reply) ->
  Serializer (Reply inner reply)
embedReplySerializer = contramap embedReply

-- | Turn an /embedded/ 'Reply' to a 'Reply' for the /bigger/ request.
--
-- This function is strict in all parameters.
--
-- @since 0.24.2
embedReply :: forall outer inner reply. Reply inner reply -> Reply outer reply
embedReply = coerce

-- (Reply (RequestOrigin !pid !ref) !v) = Reply (RequestOrigin pid ref) v

-- | Answer a 'Call' by sending the reply value to the client process.
--
-- The 'ProcessId', the 'RequestOrigin' and the 'Reply' 'Serializer' are
-- stored in the 'ReplyTarget'.
--
-- @since 0.25.1
sendReply ::
  ( HasProcesses eff q,
    Tangible reply
  ) =>
  ReplyTarget protocol reply ->
  reply ->
  Eff eff ()
sendReply (MkReplyTarget (Arg o ser)) r =
  sendAnyMessage (_requestOriginPid o) $! runSerializer ser $! Reply o r

-- | Target of a 'Call' reply.
--
-- This combines a 'RequestOrigin' with a 'Serializer' for a 'Reply' using 'Arg'.
-- There are to smart constructors for this type: 'replyTarget' and 'embeddedReplyTarget'.
--
-- Because of 'Arg' the 'Eq' and 'Ord' instances are implemented via
-- the 'RequestOrigin' instances.
--
-- @since 0.26.0
newtype ReplyTarget p r
  = MkReplyTarget (Arg (RequestOrigin p r) (Serializer (Reply p r)))
  deriving (Eq, Ord, Typeable)

instance NFData (ReplyTarget p r) where
  rnf (MkReplyTarget (Arg x y)) = rnf x `seq` y `seq` ()

-- | Smart constructor for a 'ReplyTarget'.
--
-- To build a @ReplyTarget@ for an 'Embeds' instance use 'embeddedReplyTarget'.
--
-- @since 0.26.0
replyTarget :: Serializer (Reply p reply) -> RequestOrigin p reply -> ReplyTarget p reply
replyTarget ser orig = MkReplyTarget (Arg orig ser)

-- | A simple 'Lens' for the 'RequestOrigin' of a 'ReplyTarget'.
--
-- @since 0.26.0
replyTargetOrigin :: Lens' (ReplyTarget p reply) (RequestOrigin p reply)
replyTargetOrigin f (MkReplyTarget (Arg o x)) =
  (\o' -> MkReplyTarget (Arg o' x)) <$> f o

-- | A simple 'Lens' for the 'Reply' 'Serializer' of a 'ReplyTarget'.
--
-- @since 0.26.0
replyTargetSerializer :: Lens' (ReplyTarget p reply) (Serializer (Reply p reply))
replyTargetSerializer f (MkReplyTarget (Arg x o)) =
  (\o' -> MkReplyTarget (Arg x o')) <$> f o

-- | Smart constructor for an /embedded/ 'ReplyTarget'.
--
-- This combines 'replyTarget' and 'toEmbeddedReplyTarget'.
--
-- @since 0.26.0
embeddedReplyTarget ::
  forall outer inner reply.
  Serializer (Reply outer reply) ->
  RequestOrigin outer reply ->
  ReplyTarget inner reply
embeddedReplyTarget ser orig = toEmbeddedReplyTarget $ replyTarget ser orig

-- | Convert a 'ReplyTarget' to be usable for /embedded/ replies.
--
-- This combines a 'toEmbeddedOrigin' with 'embedReplySerializer' to produce a
-- 'ReplyTarget' that can be passed to functions defined soley on an embedded protocol.
--
-- @since 0.26.0
toEmbeddedReplyTarget ::
  forall outer inner reply.
  ReplyTarget outer reply ->
  ReplyTarget inner reply
toEmbeddedReplyTarget (MkReplyTarget (Arg orig ser)) =
  MkReplyTarget (Arg (toEmbeddedOrigin orig) (embedReplySerializer ser))
