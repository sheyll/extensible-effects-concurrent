-- | This module contains a mechanism to specify what kind of messages (aka
-- /requests/) a 'Endpoint' ('Process') can handle, and if the caller blocks and
-- waits for an answer, which the server process provides.
--
-- The type magic in the 'Pdu' type family allows to define a related set of /requests/ along
-- with the corresponding responses.
--
-- Request handling can be either blocking, if a response is required, or
-- non-blocking.
--
-- A process can /serve/ a specific 'Pdu' instance by using the functions provided by
-- the "Control.Eff.Concurrent.Pdu.Server" module.
--
-- To enable a process to use such a /service/, the functions provided by
-- the "Control.Eff.Concurrent.Pdu.Client" should be used.
--
module Control.Eff.Concurrent.Protocol
  ( IsPdu(..)
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
  , prettyTypeableShows
  , prettyTypeableShowsPrec
  )
where

import           Control.DeepSeq
import           Control.Eff.Concurrent.Process
import           Control.Lens
import           Data.Coerce
import           Data.Dynamic
import           Data.Kind
import           Data.Typeable ()
import           Data.Type.Pretty
import           Type.Reflection


-- | This data family defines the __protocol data units__ (PDU) of a /protocol/.
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
-- > instance IsPdu BookShop r where
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
class (NFData (Pdu protocol reply), Show (Pdu protocol reply), Typeable protocol, Typeable reply) => IsPdu (protocol :: Type) (reply :: Synchronicity) where
  -- | Deserialize a 'Pdu' from a 'Dynamic' i.e. from a message received by a process.
  --
  -- @since 0.25.1
  deserializePdu :: Dynamic -> Maybe (Pdu protocol reply)

  default deserializePdu :: (Typeable (Pdu protocol reply)) => Dynamic -> Maybe (Pdu protocol reply)
  deserializePdu = fromDynamic

  -- | The __protocol data unit__ type for the given protocol.
  data family Pdu protocol reply

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

-- | This is a tag-type that wraps around a 'ProcessId' and holds an 'Pdu' index
-- type.
newtype Endpoint protocol = Endpoint { _fromEndpoint :: ProcessId }
  deriving (Eq,Ord,Typeable, NFData)

instance Typeable protocol => Show (Endpoint protocol) where
  showsPrec d (Endpoint c) =
    showParen (d>=10)
    (prettyTypeableShows (SomeTypeRep (typeRep @protocol)) . showsPrec 10 c)

-- | This is equivalent to @'prettyTypeableShowsPrec' 0@
--
-- @since 0.24.0
prettyTypeableShows :: SomeTypeRep -> ShowS
prettyTypeableShows = prettyTypeableShowsPrec 0

-- | An internal utility to print 'Typeable' without the kinds.
-- This is like 'showsPrec' in that it accepts a /precedence/ parameter,
-- and the result is in parentheses when the precedence is higher than 9.
--
-- @since 0.24.0
prettyTypeableShowsPrec :: Int -> SomeTypeRep -> ShowS
prettyTypeableShowsPrec d (SomeTypeRep tr) sIn =
  let (con, conArgs) = splitApps tr
   in case conArgs of
        [] -> showString (tyConName con) sIn
        _ ->
          showParen
            (d >= 10)
            (showString (tyConName con) . showChar ':' .
              foldr1 (\f acc -> showChar '-' . f . acc)
                     (prettyTypeableShowsPrec 10 <$> conArgs))
            sIn

type instance ToPretty (Endpoint a) = ToPretty a <+> PutStr "endpoint"

makeLenses ''Endpoint


instance (IsPdu a1 r, IsPdu a2 r) => IsPdu (a1, a2) r where
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

instance (IsPdu a1 r, IsPdu a2 r, IsPdu a3 r) => IsPdu (a1, a2, a3) r where
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

instance (IsPdu a1 r, IsPdu a2 r, IsPdu a3 r, IsPdu a4 r) => IsPdu (a1, a2, a3, a4) r where
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

instance (IsPdu a1 r, IsPdu a2 r, IsPdu a3 r, IsPdu a4 r, IsPdu a5 r) => IsPdu (a1, a2, a3, a4, a5) r where
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
class EmbedProtocol protocol embeddedProtocol (result :: Synchronicity) where
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

instance EmbedProtocol a a r where
  embeddedPdu = prism' id Just
  embedPdu = id
  fromPdu = Just

instance (NFData (Pdu a1 r), NFData (Pdu a2 r)) => NFData (Pdu (a1, a2) r) where
  rnf (ToPduLeft x) = rnf x
  rnf (ToPduRight y) = rnf y

instance (Show (Pdu a1 r), Show (Pdu a2 r)) => Show (Pdu (a1, a2) r) where
  showsPrec d (ToPduLeft x) = showsPrec d x
  showsPrec d (ToPduRight y) = showsPrec d y

instance EmbedProtocol (a1, a2) a1 r where
  embedPdu = ToPduLeft
  fromPdu (ToPduLeft l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2) a2 r where
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

instance EmbedProtocol (a1, a2, a3) a1 r where
  embedPdu = ToPdu1
  fromPdu (ToPdu1 l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2, a3) a2 r where
  embedPdu = ToPdu2
  fromPdu (ToPdu2 l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2, a3) a3 r where
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

instance EmbedProtocol (a1, a2, a3, a4) a1 r where
  embedPdu = ToPdu1Of4
  fromPdu (ToPdu1Of4 l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2, a3, a4) a2 r where
  embedPdu = ToPdu2Of4
  fromPdu (ToPdu2Of4 l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2, a3, a4) a3 r where
  embedPdu = ToPdu3Of4
  fromPdu (ToPdu3Of4 l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2, a3, a4) a4 r where
  embedPdu = ToPdu4Of4
  fromPdu (ToPdu4Of4 l) = Just l
  fromPdu _ = Nothing

instance (NFData (Pdu a1 r), NFData (Pdu a2 r), NFData (Pdu a3 r), NFData (Pdu a4 r), NFData (Pdu a5 r)) => NFData (Pdu (a1, a2, a3, a4, a5) r) where
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

instance EmbedProtocol (a1, a2, a3, a4, a5) a1 r where
  embedPdu = ToPdu1Of5
  fromPdu (ToPdu1Of5 l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2, a3, a4, a5) a2 r where
  embedPdu = ToPdu2Of5
  fromPdu (ToPdu2Of5 l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2, a3, a4, a5) a3 r where
  embedPdu = ToPdu3Of5
  fromPdu (ToPdu3Of5 l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2, a3, a4, a5) a4 r where
  embedPdu = ToPdu4Of5
  fromPdu (ToPdu4Of5 l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (a1, a2, a3, a4, a5) a5 r where
  embedPdu = ToPdu5Of5
  fromPdu (ToPdu5Of5 l) = Just l
  fromPdu _ = Nothing
