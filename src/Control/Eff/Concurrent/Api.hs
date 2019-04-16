-- | This module contains a mechanism to specify what kind of messages (aka
-- /requests/) a 'Server' ('Process') can handle, and if the caller blocks and
-- waits for an answer, which the server process provides.
--
-- The type magic in the 'Api' type family allows to define a related set of /requests/ along
-- with the corresponding responses.
--
-- Request handling can be either blocking, if a response is required, or
-- non-blocking.
--
-- A process can /serve/ a specific 'Api' instance by using the functions provided by
-- the "Control.Eff.Concurrent.Api.Server" module.
--
-- To enable a process to use such a /service/, the functions provided by
-- the "Control.Eff.Concurrent.Api.Client" should be used.
--
module Control.Eff.Concurrent.Api
  ( Api
  , Synchronicity(..)
  , Server(..)
  , fromServer
  , proxyAsServer
  , asServer
  )
where

import           Control.Eff.Concurrent.Process
import           Control.Lens
import           Data.Kind
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                )
import Control.DeepSeq (NFData)

-- | This data family defines an API, a communication interface description
-- between at least two processes. The processes act as __servers__ or
-- __client(s)__ regarding a specific instance of this type.
--
-- The first parameter is usually a user defined phantom type that identifies
-- the 'Api' instance.
--
-- The second parameter specifies if a specific constructor of an (GADT-like)
-- @Api@ instance is 'Synchronous', i.e. returns a result and blocks the caller
-- or if it is 'Asynchronous'
--
-- Example:
--
-- >
-- > data BookShop deriving Typeable
-- >
-- > data instance Api BookShop r where
-- >   RentBook  :: BookId   -> Api BookShop ('Synchronous (Either RentalError RentalId))
-- >   BringBack :: RentalId -> Api BookShop 'Asynchronous
-- >
-- > type BookId = Int
-- > type RentalId = Int
-- > type RentalError = String
-- >
data family Api (api :: Type) (reply :: Synchronicity)

-- | The (promoted) constructors of this type specify (at the type level) the
-- reply behavior of a specific constructor of an @Api@ instance.
data Synchronicity =
  Synchronous Type -- ^ Specify that handling a request is a blocking operation
                   -- with a specific return type, e.g. @('Synchronous (Either
                   -- RentalError RentalId))@
  | Asynchronous -- ^ Non-blocking, asynchronous, request handling
    deriving (Typeable)

-- | This is a tag-type that wraps around a 'ProcessId' and holds an 'Api' index
-- type.
newtype Server api = Server { _fromServer :: ProcessId }
  deriving (Eq,Ord,Typeable, NFData)

instance Typeable api => Show (Server api) where
  showsPrec d s@(Server c) =
    showParen (d >= 10) (showsPrec 11 (typeRep s) . showsPrec 11 c)

makeLenses ''Server

-- | Tag a 'ProcessId' with an 'Api' type index to mark it a 'Server' process
-- handling that API
proxyAsServer :: proxy api -> ProcessId -> Server api
proxyAsServer = const Server

-- | Tag a 'ProcessId' with an 'Api' type index to mark it a 'Server' process
-- handling that API
asServer :: forall api . ProcessId -> Server api
asServer = Server
