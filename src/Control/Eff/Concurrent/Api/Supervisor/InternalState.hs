module Control.Eff.Concurrent.Api.Supervisor.InternalState where

import Control.DeepSeq (NFData(rnf))
import Control.Eff as Eff
import Control.Eff.Concurrent.Api
import Control.Eff.Concurrent.Api.Client
import Control.Eff.Concurrent.Api.Server
import Control.Eff.Concurrent.Process
import Control.Eff.Log
import Control.Eff.Reader.Strict as Eff
import Control.Eff.State.Strict as Eff
import Control.Lens hiding ((.=), use)
import Control.Monad
import Data.Default
import Data.Dynamic
import Data.Foldable
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, pack)
import GHC.Generics (Generic)

-- | Internal state.
data Children i o = MkChildren
  { _childrenIdToOutput :: Map i o -- (o, ProcessId)
 -- , _childrenPidToId :: Map ProcessId i
 -- , _childrenMonitorRefs :: Map MonitorReference (i, (o, ProcessId))
  } deriving (Show, Generic)

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
  -> o
  -> Eff e ()
putChild cId cOut = modify go
  where
    go = childrenIdToOutput . at cId .~ Just cOut -- (cOut, cPid)
      --- . childrenPidToId    . at cPid

lookupChildById
  :: (Ord i, Member (State (Children i o)) e)
  => i
  -> Eff e (Maybe o)
lookupChildById i = view (childrenIdToOutput . at i) <$> get