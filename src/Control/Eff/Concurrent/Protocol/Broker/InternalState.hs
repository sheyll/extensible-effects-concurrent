module Control.Eff.Concurrent.Protocol.Broker.InternalState where

import Control.DeepSeq
import Control.Eff as Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.EffectfulServer
import Control.Eff.State.Strict as Eff
import Control.Lens hiding ((.=), use)
import Data.Default
import Data.Dynamic
import Data.Map (Map)
import GHC.Generics (Generic)


data Child p = MkChild
  { _childEndpoint :: Endpoint (ServerPdu p)
  , _childMonitoring :: MonitorReference
  }
  deriving (Generic, Typeable, Eq, Ord)

instance NFData (Child o)

instance Typeable (ServerPdu p) => Show (Child p) where
  showsPrec d c = showParen (d>=10)
    (showString "process broker entry: " . shows (_childEndpoint c)  . showChar ' ' . shows (_childMonitoring c) )

makeLenses ''Child

-- | Internal state.
data Children i p = MkChildren
  { _childrenById :: Map i (Child p)
  , _childrenByMonitor :: Map MonitorReference (i, Child p)
  } deriving (Show, Generic, Typeable)

instance Default (Children i p) where
  def = MkChildren def def

instance (NFData i) => NFData (Children i p)

makeLenses ''Children

-- | State accessor
getChildren
  ::  (Ord i, Member (State (Children i o)) e)
  => Eff e (Children i o)
getChildren = Eff.get

putChild
  :: (Ord i, Member (State (Children i o)) e)
  => i
  -> Child o
  -> Eff e ()
putChild cId c = modify ( (childrenById . at cId .~ Just c)
                        . (childrenByMonitor . at (_childMonitoring c) .~ Just (cId, c))
                        )

lookupChildById
  :: (Ord i, Member (State (Children i o)) e)
  => i
  -> Eff e (Maybe (Child o))
lookupChildById i = view (childrenById . at i) <$> get

lookupChildByMonitor
  :: (Ord i, Member (State (Children i o)) e)
  => MonitorReference
  -> Eff e (Maybe (i, Child o))
lookupChildByMonitor m = view (childrenByMonitor . at m) <$> get

lookupAndRemoveChildById
  :: forall i o e. (Ord i, Member (State (Children i o)) e)
  => i
  -> Eff e (Maybe (Child o))
lookupAndRemoveChildById i =
  traverse go =<< lookupChildById i
  where
    go c = pure c <* removeChild i c

removeChild
  :: forall i o e. (Ord i, Member (State (Children i o)) e)
  => i
  -> Child o
  -> Eff e ()
removeChild i c = do
  modify @(Children i o) ( (childrenById . at i .~ Nothing)
                         . (childrenByMonitor . at (_childMonitoring c) .~ Nothing)
                         )

lookupAndRemoveChildByMonitor
  :: forall i o e. (Ord i, Member (State (Children i o)) e)
  => MonitorReference
  -> Eff e (Maybe (i, Child o))
lookupAndRemoveChildByMonitor r = do
  traverse go =<< lookupChildByMonitor r
  where
    go (i, c) = pure (i, c) <* removeChild i c

removeAllChildren
  :: forall i o e. (Ord i, Member (State (Children i o)) e)
  => Eff e (Map i (Child o))
removeAllChildren = do
  cm <- view childrenById <$> getChildren @i
  modify @(Children i o) (childrenById .~ mempty)
  modify @(Children i o) (childrenByMonitor .~ mempty)
  return cm
