-- | Build a "Control.Eff.Concurrent.EffectfulServer" from callbacks.
--
-- This module contains in instance of 'E.Server' that delegates to
-- callback functions.
--
-- @since 0.27.0
module Control.Eff.Concurrent.Protocol.CallbackServer
  ( start
  , startLink
  , Server
  , ServerId(..)
  , Event(..)
  , TangibleCallbacks
  )
  where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Extend ()
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as E
import Control.Eff.Concurrent.Protocol.EffectfulServer (Event(..))
import Control.Eff.Log
import Data.Kind
import Data.String
import Data.Typeable
import qualified Data.Text as T
import GHC.Stack (HasCallStack)


-- | Execute the server loop.
--
-- @since 0.27.0
start
  :: forall tag eLoop q h.
     ( HasCallStack
     , TangibleCallbacks tag eLoop q
     , E.Server (Server tag eLoop) (Processes q)
     , LogsTo h (Processes q)
     )
  => (forall x . Endpoint tag -> Eff eLoop x -> Eff (Processes q) x)
  -> (Endpoint tag -> Event tag -> Eff eLoop ())
  -> ServerId tag
  -> Eff (Processes q) (Endpoint tag)
start initCb eventCb serverId = E.start (MkServer serverId initCb eventCb)

-- | Execute the server loop.
--
-- @since 0.27.0
startLink
  :: forall tag eLoop q h.
     ( HasCallStack
     , TangibleCallbacks tag eLoop q
     , E.Server (Server tag eLoop) (Processes q)
     , LogsTo h (Processes q)
     )
  => (forall x . Endpoint tag -> Eff eLoop x -> Eff (Processes q) x)
  -> (Endpoint tag -> Event tag -> Eff eLoop ())
  -> ServerId tag
  -> Eff (Processes q) (Endpoint tag)
startLink initCb eventCb serverId = E.startLink (MkServer serverId initCb eventCb)

-- | Phantom type to indicate a callback based 'E.Server' instance.
--
-- @since 0.27.0
data Server tag eLoop deriving Typeable

-- | The constraints for a /tangible/ 'Server' instance.
--
-- @since 0.27.0
type TangibleCallbacks tag eLoop e =
       ( LogIo e
       , HasProcesses eLoop e
       , Typeable e
       , Typeable eLoop
       , Typeable tag
       )

-- | The name/id of a 'Server' for logging purposes.
--
-- @since 0.24.0
newtype ServerId (tag :: Type) =
  MkServerId { _fromServerId :: T.Text }
  deriving (Typeable, NFData, Ord, Eq, IsString)

instance (Typeable tag) => Show (ServerId tag) where
  showsPrec d px@(MkServerId x) =
    showParen
      (d >= 10)
      (showString (T.unpack x)
      . showString " :: "
      . prettyTypeableShows (typeOf px)
      )

instance (TangibleCallbacks tag eLoop e) => E.Server (Server (tag :: Type) eLoop) (Processes e) where
  type ServerPdu (Server tag eLoop) = tag
  type ServerEffects (Server tag eLoop) (Processes e) = eLoop
  data instance Init (Server tag eLoop) (Processes e) =
        MkServer
         { genServerId :: ServerId tag
         , genServerRunEffects :: forall x . (Endpoint tag -> Eff eLoop x -> Eff (Processes e) x)
         , genServerOnEvent :: Endpoint tag -> Event tag -> Eff eLoop ()
         } deriving Typeable
  runEffects myEp svr = genServerRunEffects svr myEp
  onEvent myEp svr = genServerOnEvent svr myEp

instance (TangibleCallbacks tag eLoop e) => NFData (E.Init (Server (tag :: Type) eLoop) (Processes e)) where
  rnf (MkServer x y z) = rnf x `seq` y `seq` z `seq` ()

instance (TangibleCallbacks tag eLoop e) => Show (E.Init (Server (tag :: Type) eLoop) (Processes e)) where
  showsPrec d svr =
    showParen (d>=10)
      ( showsPrec 11 (genServerId svr)
      . showChar ' ' . prettyTypeableShows (typeRep (Proxy @tag))
      . showString " callback-server"
      )

