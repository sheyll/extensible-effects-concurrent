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
  -- , _childrenPidToId :: Map ProcessId i
  -- , _childrenMonitorRefs :: Map MonitorReference (i, (o, ProcessId))
  } deriving (Show, Generic, Typeable)

instance Default (Children i o) where
  def = MkChildren def -- def def

instance (NFData i, NFData o) => NFData (Children i o)

makeLenses ''Children

-- | State accessor
getChildren :: Eff (State (Children i o) ': e) (Children i o)
getChildren = Eff.get

putChild
  :: (Ord i, Member (State (Children i o)) e)
  => i
  -> Child o
  -> Eff e ()
putChild cId c = modify (childrenById . at cId .~ Just c)

lookupChildById
  :: (Ord i, Member (State (Children i o)) e)
  => i
  -> Eff e (Maybe (Child o))
lookupChildById i = view (childrenById . at i) <$> get

lookupAndRemove
  :: forall i o e. (Ord i, Member (State (Children i o)) e)
  => i
  -> Eff e (Maybe (Child o))
lookupAndRemove i =
  lookupChildById i <* modify @(Children i o) (childrenById . at i .~ Nothing)
