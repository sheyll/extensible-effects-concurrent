{-# LANGUAGE UndecidableInstances #-}
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
  ( Pdu
  , Synchronicity(..)
  , ProtocolReply
  , Tangible
  , TangiblePdu
  , Endpoint(..)
  , fromEndpoint
  , proxyAsEndpoint
  , asEndpoint
  , EmbedProtocol(..)
  , embeddedEndpoint
  )
where

import           Control.DeepSeq                ( NFData )
import           Control.Eff.Concurrent.Process
import           Control.Lens
import           Data.Kind
import           Data.Typeable                  ( Typeable )
import           Data.Type.Pretty

-- | This data family defines the **protocol data units**(PDU) of a /protocol/.
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
-- Also, for better logging, the an instance of 'ToPretty' for the 'Pdu' index
-- type must be given.
--
-- Example:
--
-- >
-- > data BookShop deriving Typeable
-- >
-- > data instance Pdu BookShop r where
-- >   RentBook  :: BookId   -> Pdu BookShop ('Synchronous (Either RentalError RentalId))
-- >   BringBack :: RentalId -> Pdu BookShop 'Asynchronous
-- >
-- > type instance ToPretty BookShop = PutStr "book shop"
-- >
-- > type BookId = Int
-- > type RentalId = Int
-- > type RentalError = String
-- >
data family Pdu (protocol :: Type) (reply :: Synchronicity)

type instance ToPretty (Pdu x y) =
  PrettySurrounded (PutStr "<") (PutStr ">") ("protocol" <:> ToPretty x <+> ToPretty y)

-- | A set of constraints for types that can evaluated via 'NFData', compared via 'Ord' and presented
-- dynamically via 'Typeable', and represented both as values
-- via 'Show', as well as on the type level via 'ToPretty'.
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
  , PrettyTypeShow (ToPretty p)
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

instance (PrettyTypeShow (ToPretty protocol)) => Show (Endpoint protocol) where
  showsPrec _ s@(Endpoint c) = showString (showPretty s) . showsPrec 10 c

type instance ToPretty (Endpoint a) = ToPretty a <+> PutStr "endpoint"

makeLenses ''Endpoint

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
class EmbedProtocol protocol embeddedProtocol where
  -- | A 'Prism' for the embedded 'Pdu's.
  embeddedPdu :: Prism' (Pdu protocol result) (Pdu embeddedProtocol result)
  embeddedPdu = prism' embedPdu fromPdu
  -- | Embed the 'Pdu' value of an embedded protocol into the corresponding
  --  'Pdu' value.
  embedPdu :: Pdu embeddedProtocol r -> Pdu protocol r
  embedPdu = review embeddedPdu
  -- | Examine a 'Pdu' value from the outer protocol, and return it, if it embeds a 'Pdu' of
  -- embedded protocol, otherwise return 'Nothing'/
  fromPdu :: Pdu protocol r -> Maybe (Pdu embeddedProtocol r)
  fromPdu = preview embeddedPdu

-- | Re-tag an 'Endpoint' of some protocol to the endpoint of an embedded
-- protocol, if an 'EmbedProtocol' instance exists.
--
-- @since 0.24.0
embeddedEndpoint :: EmbedProtocol outer inner => Endpoint outer -> Endpoint inner
embeddedEndpoint = Endpoint . _fromEndpoint

instance EmbedProtocol a a where
  embeddedPdu = prism' id Just
  embedPdu = id
  fromPdu = Just

data instance  Pdu (Either a1 a2) x where
        LeftProtocol :: Pdu a1 r -> Pdu (Either a1 a2) r
        RightProtocol :: Pdu a2 r -> Pdu (Either a1 a2) r

instance EmbedProtocol (Either a1 a2) a1 where
  embedPdu = LeftProtocol
  fromPdu (LeftProtocol l) = Just l
  fromPdu _ = Nothing

instance EmbedProtocol (Either a1 a2) a2 where
  embeddedPdu =
    prism' RightProtocol $ \case
      RightProtocol r -> Just r
      LeftProtocol _ -> Nothing
