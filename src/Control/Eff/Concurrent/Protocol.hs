-- | Types and functions for type-safe(er) interaction between processes.
--
-- All messages sent between processes are eventually converted to 'Dynamic' values
-- which carry little type information.
--
-- A step towards a more controlled and type safe process interaction model is
-- done with the facilities defined in this module.
--
-- The metaphor for communication is a /stateles protocol/ that describes the
-- messages handled by a process.
--
-- A /protocol/ is represented by a custom data type, often a /phantom/ type,
-- which is then used to form specific instances of type classes data/type families,
-- to determine the messages, the replies, the servers and clients, associated with
-- specific task, that needs to be executed concurrently.
--
-- This module contains a mechanism to specify what kind of messages (aka
-- /requests/) an 'Endpoint' can handle.
--
-- The Endpoint wraps a 'ProcessId' and carries the protocol phantom-type, to indicate the messages
-- that a process repsonds to.
--
-- The associated data type 'Pdu' defines the messages or /requests/ along
-- with the corresponding responses.
--
-- Request handling can be either blocking, if a response is required, or
-- non-blocking.
--
-- A process can /serve/ a specific protocol by using the functions provided by
-- the "Control.Eff.Concurrent.Protocol.EffectfulServer" and
-- "Control.Eff.Concurrent.Protocol.EffectfulServer" modules.
--
-- To enable a process to use such a /service/, the functions provided in
-- "Control.Eff.Concurrent.Protocol.Client" should be used.
--
module Control.Eff.Concurrent.Protocol
  ( HasPdu(..)
  , Pdu(..)
  , Synchronicity(..)
  , ProtocolReply
  , Tangible
  , TangiblePdu
  , Endpoint(..)
  , fromEndpoint
  , proxyAsEndpoint
  , asEndpoint
  , EmbedProtocol(..)
  , toEmbeddedEndpoint
  , fromEmbeddedEndpoint
  )
where

import           Control.Eff.Concurrent.Misc
import           Control.DeepSeq
import           Control.Eff.Concurrent.Process
import           Control.Lens
import           Data.Coerce
import           Data.Dynamic
import           Data.Kind
import           Data.Typeable ()
import           Data.Type.Pretty
import           Type.Reflection


-- | A server process for /protocol/.
--
-- Protocols are represented by phantom types, which are used in different places to
-- index type families and type class instances.
--
-- A 'Process' can send and receive any messages. An 'Endpoint'
-- wraps around a 'ProcessId' and carries a phantom type to indicate
-- the kinds of messages accepted by the process.
--
-- As a metaphor, communication between processes can be thought of waiting for
-- and sending __protocol data units__ belonging to some protocol.
newtype Endpoint protocol = Endpoint { _fromEndpoint :: ProcessId }
  deriving (Eq,Ord,Typeable, NFData)

instance Typeable protocol => Show (Endpoint protocol) where
  showsPrec d (Endpoint c) =
    showParen (d>=10)
    (showSTypeRep (SomeTypeRep (typeRep @protocol)) . showsPrec 10 c)

-- | This type class and the associated data family defines the
-- __protocol data units__ (PDU) of a /protocol/.
--
-- A Protocol in the sense of a communication interface description
-- between processes.
--
-- The first parameter is usually a user defined type that identifies the
-- protocol that uses the 'Pdu's are. It maybe a /phantom/ type.
--
-- The second parameter specifies if a specific constructor of an (GADT-like)
-- @Pdu@ instance is 'Synchronous', i.e. returns a result and blocks the caller
-- or if it is 'Asynchronous'
--
-- Example:
--
-- >
-- > data BookShop deriving Typeable
-- >
-- > instance Typeable r => HasPdu BookShop r where
-- >   data instance Pdu BookShop r where
-- >     RentBook  :: BookId   -> Pdu BookShop ('Synchronous (Either RentalError RentalId))
-- >     BringBack :: RentalId -> Pdu BookShop 'Asynchronous
-- >     deriving Typeable
-- >
-- > type BookId = Int
-- > type RentalId = Int
-- > type RentalError = String
-- >
--
-- @since 0.25.1
class (Tangible (Pdu protocol reply), Typeable protocol, Typeable reply) => HasPdu (protocol :: Type) (reply :: Synchronicity) where

  -- | The __protocol data unit__ type for the given protocol.
  data family Pdu protocol reply

  -- | Deserialize a 'Pdu' from a 'Dynamic' i.e. from a message received by a process.
  --
  -- @since 0.25.1
  deserializePdu :: Dynamic -> Maybe (Pdu protocol reply)

  default deserializePdu :: (Typeable (Pdu protocol reply)) => Dynamic -> Maybe (Pdu protocol reply)
  deserializePdu = fromDynamic

  --  type family PrettyPdu protocol reply :: PrettyType
  --  type instance PrettyPdu protocol reply =
  --      PrettySurrounded (PutStr "<") (PutStr ">") ("protocol" <:> ToPretty protocol <+> ToPretty reply)

type instance ToPretty (Pdu x y) =
  PrettySurrounded (PutStr "<") (PutStr ">") ("protocol" <:> ToPretty x <+> ToPretty y)

-- | A set of constraints for types that can evaluated via 'NFData', compared via 'Ord' and presented
-- dynamically via 'Typeable', and represented both as values
-- via 'Show'.
--
-- @since 0.23.0
type Tangible i =
  ( NFData i
  , Typeable i
  , Show i
  )

-- | A 'Constraint' that bundles the requirements for the
-- 'Pdu' values of a protocol.
--
-- This ensures that 'Pdu's can be strictly and deeply evaluated and shown
-- such that for example logging is possible.
--
-- @since 0.24.0
type TangiblePdu p r =
  ( Typeable p
  , Typeable r
  , Tangible (Pdu p r)
  , HasPdu p r
  )

-- | The (promoted) constructors of this type specify (at the type level) the
-- reply behavior of a specific constructor of an @Pdu@ instance.
data Synchronicity =
  Synchronous Type -- ^ Specify that handling a request is a blocking operation
                   -- with a specific return type, e.g. @('Synchronous (Either
                   -- RentalError RentalId))@
  | Asynchronous -- ^ Non-blocking, asynchronous, request handling
    deriving Typeable

-- | This type function takes an 'Pdu' and analysis the reply type, i.e. the 'Synchronicity'
-- and evaluates to either @t@ for an
-- @Pdu x ('Synchronous' t)@ or to '()' for an @Pdu x 'Asynchronous'@.
--
-- @since 0.24.0
type family ProtocolReply (s :: Synchronicity) where
  ProtocolReply ('Synchronous t) = t
  ProtocolReply 'Asynchronous = ()

type instance ToPretty (Endpoint a) = ToPretty a <+> PutStr "endpoint"

makeLenses ''Endpoint

instance (Typeable r, HasPdu a1 r, HasPdu a2 r) => HasPdu (a1, a2) r where
  data instance Pdu (a1, a2) r where
          ToPduLeft :: Pdu a1 r -> Pdu (a1, a2) r
          ToPduRight :: Pdu a2 r -> Pdu (a1, a2) r

  deserializePdu d =
    case deserializePdu d of
      Just (x :: Pdu a1 r) ->
        Just (embedPdu x)
      Nothing ->
        case deserializePdu d of
          Just (x :: Pdu a2 r) ->
            Just (embedPdu x)
          Nothing ->
            Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r) => HasPdu (a1, a2, a3) r where
  data instance Pdu (a1, a2, a3) r where
    ToPdu1 :: Pdu a1 r -> Pdu (a1, a2, a3) r
    ToPdu2 :: Pdu a2 r -> Pdu (a1, a2, a3) r
    ToPdu3 :: Pdu a3 r -> Pdu (a1, a2, a3) r

  deserializePdu d =
    case deserializePdu d of
      Just (x :: Pdu a1 r) ->
        Just (embedPdu x)
      Nothing ->
        case deserializePdu d of
          Just (x :: Pdu a2 r) ->
            Just (embedPdu x)
          Nothing ->
            case deserializePdu d of
              Just (x :: Pdu a3 r) ->
                Just (embedPdu x)
              Nothing ->
                Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r) => HasPdu (a1, a2, a3, a4) r where
  data instance Pdu (a1, a2, a3, a4) r where
    ToPdu1Of4 :: Pdu a1 r -> Pdu (a1, a2, a3, a4) r
    ToPdu2Of4 :: Pdu a2 r -> Pdu (a1, a2, a3, a4) r
    ToPdu3Of4 :: Pdu a3 r -> Pdu (a1, a2, a3, a4) r
    ToPdu4Of4 :: Pdu a4 r -> Pdu (a1, a2, a3, a4) r

  deserializePdu d =
    case deserializePdu d of
      Just (x :: Pdu a1 r) ->
        Just (embedPdu x)
      Nothing ->
        case deserializePdu d of
          Just (x :: Pdu a2 r) ->
            Just (embedPdu x)
          Nothing ->
            case deserializePdu d of
              Just (x :: Pdu a3 r) ->
                Just (embedPdu x)
              Nothing ->
                case deserializePdu d of
                  Just (x :: Pdu a4 r) ->
                    Just (embedPdu x)
                  Nothing ->
                    Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r, HasPdu a5 r) => HasPdu (a1, a2, a3, a4, a5) r where
  data instance Pdu (a1, a2, a3, a4, a5) r where
    ToPdu1Of5 :: Pdu a1 r -> Pdu (a1, a2, a3, a4, a5) r
    ToPdu2Of5 :: Pdu a2 r -> Pdu (a1, a2, a3, a4, a5) r
    ToPdu3Of5 :: Pdu a3 r -> Pdu (a1, a2, a3, a4, a5) r
    ToPdu4Of5 :: Pdu a4 r -> Pdu (a1, a2, a3, a4, a5) r
    ToPdu5Of5 :: Pdu a5 r -> Pdu (a1, a2, a3, a4, a5) r

  deserializePdu d =
    case deserializePdu d of
      Just (x :: Pdu a1 r) ->
        Just (embedPdu x)
      Nothing ->
        case deserializePdu d of
          Just (x :: Pdu a2 r) ->
            Just (embedPdu x)
          Nothing ->
            case deserializePdu d of
              Just (x :: Pdu a3 r) ->
                Just (embedPdu x)
              Nothing ->
                case deserializePdu d of
                  Just (x :: Pdu a4 r) ->
                    Just (embedPdu x)
                  Nothing ->
                    case deserializePdu d of
                      Just (x :: Pdu a5 r) ->
                        Just (embedPdu x)
                      Nothing ->
                        Nothing

-- | Tag a 'ProcessId' with an 'Pdu' type index to mark it a 'Endpoint' process
-- handling that API
proxyAsEndpoint :: proxy protocol -> ProcessId -> Endpoint protocol
proxyAsEndpoint = const Endpoint

-- | Tag a 'ProcessId' with an 'Pdu' type index to mark it a 'Endpoint' process
-- handling that API
asEndpoint :: forall protocol . ProcessId -> Endpoint protocol
asEndpoint = Endpoint

-- | A class for 'Pdu' instances that embed other 'Pdu'.
-- A 'Prism' for the embedded 'Pdu' is the center of this class
--
-- Laws: @embeddedPdu = prism' embedPdu fromPdu@
--
-- @since 0.24.0
class
  ( HasPdu protocol         result
  , HasPdu embeddedProtocol result
  )
  => EmbedProtocol protocol embeddedProtocol (result :: Synchronicity) where

  -- | A 'Prism' for the embedded 'Pdu's.
  embeddedPdu :: Prism' (Pdu protocol result) (Pdu embeddedProtocol result)
  embeddedPdu = prism' embedPdu fromPdu

  -- | Embed the 'Pdu' value of an embedded protocol into the corresponding
  --  'Pdu' value.
  embedPdu :: Pdu embeddedProtocol result -> Pdu protocol result
  embedPdu = review embeddedPdu
  -- | Examine a 'Pdu' value from the outer protocol, and return it, if it embeds a 'Pdu' of
  -- embedded protocol, otherwise return 'Nothing'/
  fromPdu :: Pdu protocol result -> Maybe (Pdu embeddedProtocol result)
  fromPdu = preview embeddedPdu


-- | Convert an 'Endpoint' to an endpoint for an embedded protocol.
--
-- See 'EmbedProtocol', 'fromEmbeddedEndpoint'.
--
-- @since 0.25.1
toEmbeddedEndpoint :: forall inner outer r . EmbedProtocol outer inner r => Endpoint outer -> Endpoint inner
toEmbeddedEndpoint = coerce

-- | Convert an 'Endpoint' to an endpoint for a server, that embeds the protocol.
--
-- See 'EmbedProtocol', 'toEmbeddedEndpoint'.
--
-- @since 0.25.1
fromEmbeddedEndpoint ::  forall outer inner r . EmbedProtocol outer inner r => Endpoint inner -> Endpoint outer
fromEmbeddedEndpoint = coerce

instance HasPdu a r => EmbedProtocol a a r where
  embeddedPdu = prism' id Just
  embedPdu = id
  fromPdu = Just

instance (NFData (Pdu a1 r), NFData (Pdu a2 r)) => NFData (Pdu (a1, a2) r) where
  rnf (ToPduLeft x) = rnf x
  rnf (ToPduRight y) = rnf y

instance (Show (Pdu a1 r), Show (Pdu a2 r)) => Show (Pdu (a1, a2) r) where
  showsPrec d (ToPduLeft x) = showsPrec d x
  showsPrec d (ToPduRight y) = showsPrec d y

instance (Typeable r, HasPdu a1 r, HasPdu a2 r) => EmbedProtocol (a1, a2) a1 r where
  embedPdu = ToPduLeft
  fromPdu (ToPduLeft l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r) => EmbedProtocol (a1, a2) a2 r where
  embeddedPdu =
    prism' ToPduRight $ \case
      ToPduRight r -> Just r
      ToPduLeft _ -> Nothing

instance (NFData (Pdu a1 r), NFData (Pdu a2 r), NFData (Pdu a3 r)) => NFData (Pdu (a1, a2, a3) r) where
  rnf (ToPdu1 x) = rnf x
  rnf (ToPdu2 y) = rnf y
  rnf (ToPdu3 z) = rnf z

instance (Show (Pdu a1 r), Show (Pdu a2 r), Show (Pdu a3 r)) => Show (Pdu (a1, a2, a3) r) where
  showsPrec d (ToPdu1 x) = showsPrec d x
  showsPrec d (ToPdu2 y) = showsPrec d y
  showsPrec d (ToPdu3 z) = showsPrec d z

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r) => EmbedProtocol (a1, a2, a3) a1 r where
  embedPdu = ToPdu1
  fromPdu (ToPdu1 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r) => EmbedProtocol (a1, a2, a3) a2 r where
  embedPdu = ToPdu2
  fromPdu (ToPdu2 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r) => EmbedProtocol (a1, a2, a3) a3 r where
  embedPdu = ToPdu3
  fromPdu (ToPdu3 l) = Just l
  fromPdu _ = Nothing

instance (NFData (Pdu a1 r), NFData (Pdu a2 r), NFData (Pdu a3 r), NFData (Pdu a4 r)) => NFData (Pdu (a1, a2, a3, a4) r) where
  rnf (ToPdu1Of4 x) = rnf x
  rnf (ToPdu2Of4 y) = rnf y
  rnf (ToPdu3Of4 z) = rnf z
  rnf (ToPdu4Of4 w) = rnf w

instance (Show (Pdu a1 r), Show (Pdu a2 r), Show (Pdu a3 r), Show (Pdu a4 r)) => Show (Pdu (a1, a2, a3, a4) r) where
  showsPrec d (ToPdu1Of4 x) = showsPrec d x
  showsPrec d (ToPdu2Of4 y) = showsPrec d y
  showsPrec d (ToPdu3Of4 z) = showsPrec d z
  showsPrec d (ToPdu4Of4 w) = showsPrec d w

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r) => EmbedProtocol (a1, a2, a3, a4) a1 r where
  embedPdu = ToPdu1Of4
  fromPdu (ToPdu1Of4 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r) => EmbedProtocol (a1, a2, a3, a4) a2 r where
  embedPdu = ToPdu2Of4
  fromPdu (ToPdu2Of4 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r) => EmbedProtocol (a1, a2, a3, a4) a3 r where
  embedPdu = ToPdu3Of4
  fromPdu (ToPdu3Of4 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r) => EmbedProtocol (a1, a2, a3, a4) a4 r where
  embedPdu = ToPdu4Of4
  fromPdu (ToPdu4Of4 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, NFData (Pdu a1 r), NFData (Pdu a2 r), NFData (Pdu a3 r), NFData (Pdu a4 r), NFData (Pdu a5 r)) => NFData (Pdu (a1, a2, a3, a4, a5) r) where
  rnf (ToPdu1Of5 x) = rnf x
  rnf (ToPdu2Of5 y) = rnf y
  rnf (ToPdu3Of5 z) = rnf z
  rnf (ToPdu4Of5 w) = rnf w
  rnf (ToPdu5Of5 w) = rnf w

instance (Show (Pdu a1 r), Show (Pdu a2 r), Show (Pdu a3 r), Show (Pdu a4 r), Show (Pdu a5 r)) => Show (Pdu (a1, a2, a3, a4, a5) r) where
  showsPrec d (ToPdu1Of5 x) = showsPrec d x
  showsPrec d (ToPdu2Of5 y) = showsPrec d y
  showsPrec d (ToPdu3Of5 z) = showsPrec d z
  showsPrec d (ToPdu4Of5 w) = showsPrec d w
  showsPrec d (ToPdu5Of5 v) = showsPrec d v

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r, HasPdu a5 r) => EmbedProtocol (a1, a2, a3, a4, a5) a1 r where
  embedPdu = ToPdu1Of5
  fromPdu (ToPdu1Of5 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r, HasPdu a5 r) => EmbedProtocol (a1, a2, a3, a4, a5) a2 r where
  embedPdu = ToPdu2Of5
  fromPdu (ToPdu2Of5 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r, HasPdu a5 r) => EmbedProtocol (a1, a2, a3, a4, a5) a3 r where
  embedPdu = ToPdu3Of5
  fromPdu (ToPdu3Of5 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r, HasPdu a5 r) => EmbedProtocol (a1, a2, a3, a4, a5) a4 r where
  embedPdu = ToPdu4Of5
  fromPdu (ToPdu4Of5 l) = Just l
  fromPdu _ = Nothing

instance (Typeable r, HasPdu a1 r, HasPdu a2 r, HasPdu a3 r, HasPdu a4 r, HasPdu a5 r) => EmbedProtocol (a1, a2, a3, a4, a5) a5 r where
  embedPdu = ToPdu5Of5
  fromPdu (ToPdu5Of5 l) = Just l
  fromPdu _ = Nothing
