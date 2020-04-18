{-# LANGUAGE UndecidableInstances #-}
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
  , Callbacks
  , callbacks
  , onEvent
  , CallbacksEff
  , callbacksEff
  , onEventEff
  )
  where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent.Misc
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as E
import Control.Eff.Concurrent.Protocol.EffectfulServer (Event(..))
import Control.Eff.Extend ()
import Control.Eff.Log
import Data.Kind
import Data.Proxy
import Data.String
import Data.Typeable
import qualified Data.Text as T
import GHC.Stack (HasCallStack)

-- | Execute the server loop, that dispatches incoming events
-- to either a set of 'Callbacks' or 'CallbacksEff'.
--
-- @since 0.29.1
start
  :: forall (tag :: Type) eLoop q e.
     ( HasCallStack
     , TangibleCallbacks tag eLoop q
     , E.Server (Server tag eLoop q) (Processes q)
     , FilteredLogging (Processes q)
     , HasProcesses e q
     )
  => CallbacksEff tag eLoop q
  -> Eff e (Endpoint tag)
start = E.start

-- | Execute the server loop, that dispatches incoming events
-- to either a set of 'Callbacks' or 'CallbacksEff'.
--
-- @since 0.29.1
startLink
  :: forall (tag :: Type) eLoop q e.
     ( HasCallStack
     , TangibleCallbacks tag eLoop q
     , E.Server (Server tag eLoop q) (Processes q)
     , FilteredLogging (Processes q)
     , HasProcesses e q
     )
  => CallbacksEff tag eLoop q
  -> Eff e (Endpoint tag)
startLink = E.startLink

-- | Phantom type to indicate a callback based 'E.Server' instance.
--
-- @since 0.27.0
data Server tag eLoop e deriving Typeable

instance ToTypeLogMsg tag => ToTypeLogMsg (Server tag eLoop e) where
  toTypeLogMsg _ = toTypeLogMsg (Proxy @tag)

-- | The constraints for a /tangible/ 'Server' instance.
--
-- @since 0.27.0
type TangibleCallbacks tag eLoop e =
       ( HasProcesses eLoop e
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
      . showSTypeRep (typeOf px)
      )

instance (ToTypeLogMsg tag, TangibleCallbacks tag eLoop e) => E.Server (Server (tag :: Type) eLoop e) (Processes e) where
  type ServerPdu (Server tag eLoop e) = tag
  type ServerEffects (Server tag eLoop e) (Processes e) = eLoop
  data instance Init (Server tag eLoop e) =
        MkServer
         { genServerId :: ServerId tag
         , genServerRunEffects :: forall x . (Endpoint tag -> Eff eLoop x -> Eff (Processes e) x)
         , genServerOnEvent :: Endpoint tag -> Event tag -> Eff eLoop ()
         } deriving Typeable
  runEffects myEp svr = genServerRunEffects svr myEp
  onEvent myEp svr = genServerOnEvent svr myEp

instance (TangibleCallbacks tag eLoop e) => NFData (E.Init (Server (tag :: Type) eLoop e)) where
  rnf (MkServer x y z) = rnf x `seq` y `seq` z `seq` ()

instance (TangibleCallbacks tag eLoop e) => Show (E.Init (Server (tag :: Type) eLoop e)) where
  showsPrec d svr =
    showParen (d>=10)
      ( showsPrec 11 (genServerId svr)
      . showChar ' ' . showSTypeRep (typeRep (Proxy @tag))
      . showString " callback-server"
      )


-- ** Smart Constructors for 'Callbacks'

-- | A convenience type alias for callbacks that do not
-- need a custom effect.
--
-- @since 0.29.1
type Callbacks tag e = CallbacksEff tag (Processes e) e


-- | A smart constructor for 'Callbacks'.
--
-- @since 0.29.1
callbacks
  :: forall tag q.
     ( HasCallStack
     , TangibleCallbacks tag (Processes q) q
     , E.Server (Server tag (Processes q) q) (Processes q)
     , FilteredLogging q
     )
  => (Endpoint tag -> Event tag -> Eff (Processes q) ())
  -> ServerId tag
  -> Callbacks tag q
callbacks evtCb i = callbacksEff (const id) evtCb i

-- | A simple smart constructor for 'Callbacks'.
--
-- @since 0.29.1
onEvent
  :: forall tag q .
     ( HasCallStack
     , TangibleCallbacks tag (Processes q) q
     , E.Server (Server tag (Processes q) q) (Processes q)
     , FilteredLogging q
     )
  => (Event tag -> Eff (Processes q) ())
  -> ServerId (tag :: Type)
  -> Callbacks tag q
onEvent = onEventEff id

-- ** Smart Constructors for 'CallbacksEff'

-- | A convenience type alias for __effectful__ callback based 'E.Server' instances.
--
-- See 'Callbacks'.
--
-- @since 0.29.1
type CallbacksEff tag eLoop e = E.Init (Server tag eLoop e)

-- | A smart constructor for 'CallbacksEff'.
--
-- @since 0.29.1
callbacksEff
  :: forall tag eLoop q.
     ( HasCallStack
     , TangibleCallbacks tag eLoop q
     , E.Server (Server tag eLoop q) (Processes q)
     , FilteredLogging q
     )
  => (forall x . Endpoint tag -> Eff eLoop x -> Eff (Processes q) x)
  -> (Endpoint tag -> Event tag -> Eff eLoop ())
  -> ServerId tag
  -> CallbacksEff tag eLoop q
callbacksEff a b c = MkServer c a b

-- | A simple smart constructor for 'CallbacksEff'.
--
-- @since 0.29.1
onEventEff
  ::
    ( HasCallStack
    , TangibleCallbacks tag eLoop q
    , E.Server (Server tag eLoop q) (Processes q)
    , FilteredLogging q
    )
  => (forall a. Eff eLoop a -> Eff (Processes q) a)
  -> (Event tag -> Eff eLoop ())
  -> ServerId (tag :: Type)
  -> CallbacksEff tag eLoop q
onEventEff h f i = callbacksEff (const h) (const f) i
