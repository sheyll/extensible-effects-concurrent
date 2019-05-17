module Control.Eff.Concurrent.Api.Supervisor.InternalState where

import Control.DeepSeq
import Control.Eff as Eff
import Control.Eff.Concurrent.Process
import Control.Eff.State.Strict as Eff
import Control.Lens hiding ((.=), use)
import Data.Default
import Data.Dynamic
import Data.Map (Map)
import GHC.Generics (Generic)


data Child o = MkChild
  { _childOutput :: o
  , _childProcessId :: ProcessId
  , _childMonitoring :: MonitorReference
  }
  deriving (Show, Generic, Typeable, Eq, Ord)

instance (NFData o) => NFData (Child o)

makeLenses ''Child


-- | Internal state.
data Children i o = MkChildren
  { _childrenById :: Map i (Child o)
  , _childrenByMonitor :: Map MonitorReference (i, Child o)
  } deriving (Show, Generic, Typeable)

instance Default (Children i o) where
  def = MkChildren def def

instance (NFData i, NFData o) => NFData (Children i o)

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
