-- | Utilities to implement /server-loops/ with builtin state and /TEA/-like naming.
--
-- @since 0.24.0
module Control.Eff.Concurrent.Protocol.StatefulServer
  ( Server(..)
  , start
  , startLink
  , ModelState
  , modifyModel
  , getAndModifyModel
  , modifyAndGetModel
  , getModel
  , putModel
  , getAndPutModel
  , useModel
  , zoomModel
  , SettingsReader
  , askSettings
  , viewSettings
  -- * Re-exports
  , Effectful.Event(..)
  , RequestOrigin(..)
  , Reply(..)
  , sendReply
  , toEmbeddedOrigin
  , embedReplySerializer
  )
  where

import Control.Eff
import Control.Eff.Extend ()
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import Control.Eff.Concurrent.Protocol.Request
import Control.Eff.Log
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Lens
import Data.Default
import Data.Kind
import Data.Typeable
import GHC.Stack (HasCallStack)

-- | A type class for server loops.
--
-- This type class serves as interface for other abstractions, for example /process supervision/
--
-- The methods of this class handle 'Event's 'Request's for 'Pdu' instance.
--
-- Instances can by /index types/ for 'Pdu' family directly, or indirectly via the 'ServerPdu' type family.
--
-- To builder servers serving multiple protocols, use the generic 'Pdu' instances, for which 'EmbedProtocol'
-- instances exist, like 2-,3-,4-, or 5-tuple.
--
-- The naming is inspired by The Elm Architecture, without the @view@ callback.
--
-- This class is based on "Control.Eff.Concurrent.Protocol.EffectfulServer" and adds a default
-- 'State' and 'Reader' effect.
--
-- @since 0.24.0
class (Typeable (Protocol a)) => Server (a :: Type) q where
  -- | The value that defines what is required to initiate a 'Server'
  -- loop.
  data StartArgument a q
  -- | The index type of the 'Event's that this server processes.
  -- This is the first parameter to the 'Request' and therefore of
  -- the 'Pdu' family.
  type Protocol a :: Type
  type Protocol a = a
  -- | Type of the /model/ data, given to every invocation of 'update'
  -- via the 'ModelState' effect.
  -- The /model/ of a server loop is changed through incoming 'Event's.
  -- It is initially calculated by 'setup'.
  type Model a :: Type
  type Model a = ()
  -- | Type of read-only state.
  type Settings a :: Type
  type Settings a = ()

  -- | Return an initial 'Model' and 'Settings'
  setup ::
       StartArgument a q
    -> Eff (Processes q) (Model a, Settings a)

  default setup ::
       (Default (Model a), Default (Settings a))
    => StartArgument a q
    -> Eff (Processes q) (Model a, Settings a)
  setup _ = pure (def, def)

  -- | Update the 'Model' based on the 'Event'.
  update ::
       StartArgument a q
    -> Effectful.Event (Protocol a)
    -> Eff (ModelState a ': SettingsReader a ': Processes q) ()

-- | This type is used to build stateful 'EffectfulServer' instances.
--
-- The types that are suitable to be have to be instances of 'Server'
--
-- @since 0.24.0
data Stateful a deriving Typeable

instance Server a q => Effectful.Server (Stateful a) (Processes q) where
  data Init (Stateful a) (Processes q) = Init (StartArgument a q)
  type ServerPdu (Stateful a) = Protocol a
  type Effects (Stateful a) (Processes q) = ModelState a ': SettingsReader a ': Processes q

  runEffects (Init sa) m = do
    (st, env) <- setup sa
    runReader env (evalState st m)

  onEvent (Init sa) = update sa


-- | Execute the server loop.
--
-- @since 0.24.0
start
  :: forall a q h
  . ( HasCallStack
    , Typeable a
    , LogsTo h (Processes q)
    , Effectful.Server (Stateful a) (Processes q)
    , Server a q
    )
  => StartArgument a q -> Eff (Processes q) (Endpoint (Protocol a))
start = Effectful.start . Init

-- | Execute the server loop.
--
-- @since 0.24.0
startLink
  :: forall a q h
  . ( HasCallStack
    , Typeable a
    , LogsTo h (Processes q)
    , Effectful.Server (Stateful a) (Processes q)
    , Server a q
    )
  => StartArgument a q -> Eff (Processes q) (Endpoint (Protocol a))
startLink = Effectful.startLink . Init


-- | The 'Eff'ect type of mutable 'Model' in a 'Server' instance.
--
-- @since 0.24.0
type ModelState a = State (Model a)

-- | Modify the 'Model' of a 'Server'.
--
-- @since 0.24.0
modifyModel :: forall a e . Member (ModelState a) e => (Model a -> Model a) -> Eff e ()
modifyModel f = getModel @a >>= putModel @a . f

-- | Modify the 'Model' of a 'Server' and return the old value.
--
-- @since 0.24.0
getAndModifyModel :: forall a e . Member (ModelState a) e => (Model a -> Model a) -> Eff e (Model a)
getAndModifyModel f = getModel @a <* modify f

-- | Modify the 'Model' of a 'Server' and return the new value.
--
-- @since 0.24.0
modifyAndGetModel :: forall a e . Member (ModelState a) e => (Model a -> Model a) -> Eff e (Model a)
modifyAndGetModel f = modifyModel @a f *> getModel @a

-- | Return the 'Model' of a 'Server'.
--
-- @since 0.24.0
getModel :: forall a e . Member (ModelState a) e => Eff e (Model a)
getModel = get

-- | Return a element selected by a 'Lens' of the 'Model' of a 'Server'.
--
-- @since 0.24.0
useModel :: forall a b e . Member (ModelState a) e => Getting b (Model a) b -> Eff e b
useModel l = view l <$> getModel @a

-- | Overwrite the 'Model' of a 'Server'.
--
-- @since 0.24.0
putModel :: forall a e . Member (ModelState a) e => Model a -> Eff e ()
putModel = put

-- | Overwrite the 'Model' of a 'Server', return the old value.
--
-- @since 0.24.0
getAndPutModel :: forall a e . Member (ModelState a) e => Model a -> Eff e (Model a)
getAndPutModel m = getModel @a <* putModel @a m

-- | Run an action that modifies portions of the 'Model' of a 'Server' defined by the given 'Lens'.
--
-- @since 0.24.0
zoomModel :: forall a b c e. Member (ModelState a) e => Lens' (Model a) b -> Eff (State b ': e) c -> Eff e c
zoomModel l a = do
  m0 <- getModel @a
  (c, m1) <- runState (view l m0) a
  modifyModel @a (l .~ m1)
  return c

-- | The 'Eff'ect type of readonly 'Settings' in a 'Server' instance.
--
-- @since 0.24.0
type SettingsReader a = Reader (Settings a)

-- | Return the read-only 'Settings' of a 'Server'
--
-- @since 0.24.0
askSettings :: forall a e . Member (SettingsReader a) e => Eff e (Settings a)
askSettings = ask

-- | Return the read-only 'Settings' of a 'Server' as viewed through a 'Lens'
--
-- @since 0.24.0
viewSettings :: forall a b e . Member (SettingsReader a) e =>  Getting b (Settings a) b -> Eff e b
viewSettings l = view l <$> askSettings @a
