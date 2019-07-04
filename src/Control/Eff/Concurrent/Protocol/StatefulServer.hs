-- | Utilities to implement /server-loops/ with builtin state and /TEA/-like naming.
--
-- @since 0.24.0
module Control.Eff.Concurrent.Protocol.StatefulServer
  ( Server(..)
  , Stateful
  , Effectful.Init(..)
  , startLink
  , start
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
  , mapEffects
  , coerceEffects
  -- * Re-exports
  , Effectful.Event(..)
  )
  where

import Control.Eff
import Control.Eff.Concurrent.Misc
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import Control.Eff.Extend ()
import Control.Eff.Log
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Lens
import Data.Coerce
import Data.Default
import Data.Kind
import Data.String (fromString)
import Data.Typeable
import GHC.Stack (HasCallStack)
import Control.Eff.Extend (raise)
-- | A type class for server loops.
--
-- This class serves as interface for other mechanisms, for example /process supervision/
--
-- The methods of this class handle 'Event's and 'Request's for 'Pdu' instances.
--
-- Instances can by /index types/ for 'Pdu' family directly, or indirectly via the 'ServerPdu' type family.
--
-- To builder servers serving multiple protocols, use the generic 'Pdu' instances, for which 'Embeds'
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
  data family Model a :: Type

  -- | Type of read-only state.
  type Settings a :: Type
  type Settings a = ()

  -- | Return a new 'ProcessTitle' for the stateful process,
  -- while it is running.
  --
  -- @since 0.29.3
  title :: StartArgument a q -> ProcessTitle

  default title :: Typeable a => StartArgument a q -> ProcessTitle
  title _ = fromString $ showSTypeable @a ""

  -- | Return an initial 'Model' and 'Settings'
  setup ::
       Endpoint (Protocol a)
    -> StartArgument a q
    -> Eff q (Model a, Settings a)

  default setup ::
       (Default (Model a), Default (Settings a))
    => Endpoint (Protocol a)
    -> StartArgument a q
    -> Eff q (Model a, Settings a)
  setup _ _ = pure (def, def)

  -- | Update the 'Model' based on the 'Event'.
  update ::
       Endpoint (Protocol a)
    -> StartArgument a q
    -> Effectful.Event (Protocol a)
    -> Eff (ModelState a ': SettingsReader a ': q) ()

-- | This type is used to build stateful 'EffectfulServer' instances.
--
-- It is a variant of 'EffectfulServer', that comes pre-installed
-- with 'State' and 'Reader' effects.
--
-- @since 0.24.0
data Stateful a deriving Typeable

instance Server a q => Effectful.Server (Stateful a) q where
  data Init (Stateful a) q = Init (StartArgument a q)
  type ServerPdu (Stateful a) = Protocol a
  type ServerEffects (Stateful a) q = ModelState a ': SettingsReader a ': q

  runEffects selfEndpoint (Init sa) m = do
    (st, env) <- setup selfEndpoint sa
    runReader env (evalState st m)

  onEvent selfEndpoint (Init sa) = update selfEndpoint sa

  serverTitle (Init startArg) = title startArg

-- | Execute the server loop.
--
-- @since 0.24.0
startLink
  :: forall a r q h
  . ( HasCallStack
    , Typeable a
    , LogsTo h (Processes q)
    , Effectful.Server (Stateful a) (Processes q)
    , Server a (Processes q)
    , HasProcesses r q
    )
  => StartArgument a (Processes q) -> Eff r (Endpoint (Protocol a))
startLink = Effectful.startLink . Init

-- | Execute the server loop. Please use 'startLink' if you can.
--
-- @since 0.24.0
start
  :: forall a r q h
  . ( HasCallStack
    , Typeable a
    , Effectful.Server (Stateful a) (Processes q)
    , Server a (Processes q)
    , LogsTo h (Processes q)
    , HasProcesses r q
    )
  => StartArgument a (Processes q) -> Eff r (Endpoint (Protocol a))
start = Effectful.start . Init

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

-- | Map 'ModelState' and 'SettingsReader' effects.
-- Use this to embed 'update' from another 'Server' instance.
--
-- @since 0.29.3
mapEffects
  :: forall inner outer a e.
     (Settings outer -> Settings inner) -- ^ A function to get the /inner/ settings out of the /outer/ settings
  -> Lens' (Model outer) (Model inner)  -- ^ A 'Lens' to get and set the /inner/ model inside the /outer/ model
  -> Eff (ModelState inner : SettingsReader inner : e) a
  -> Eff (ModelState outer : SettingsReader outer : e) a
mapEffects innerSettings innerStateLens innerEff =
  do st0 <- getModel @outer
     s0 <- askSettings @outer
     (res, st1) <-
      raise
        (raise
          (runReader
            @(Settings inner)
            (innerSettings s0)
            (runState
              @(Model inner)
              (st0 ^. innerStateLens)
              innerEff)))
     modifyModel @outer (innerStateLens .~ st1)
     return res


-- | Coerce 'Coercible' 'ModelState' and 'SettingsReader' effects.
-- Use this to embed 'update' from a /similar/ 'Server' instance.
--
-- @since 0.29.3
coerceEffects
  :: forall inner outer a e.
     ( Coercible (Model inner) (Model outer)
     , Coercible (Model outer) (Model inner)
     , Coercible (Settings outer) (Settings inner)
     )
  => Eff (ModelState inner : SettingsReader inner : e) a
  -> Eff (ModelState outer : SettingsReader outer : e) a
coerceEffects innerEff =
  do st0 <- getModel @outer
     s0 <- askSettings @outer
     (res, st1) <-
      raise
        (raise
          (runReader
            @(Settings inner)
            (coerce s0)
            (runState
              (coerce @(Model outer) st0)
              innerEff)))
     putModel @outer (coerce st1)
     return res
