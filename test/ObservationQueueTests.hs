module ObservationQueueTests where

import           Control.Eff.Concurrent
import           Data.Typeable
import           Control.Eff
import           GHC.Stack
import           Control.Lens

data FooApi deriving Typeable

data instance Api FooApi x where
  Foo :: String -> Api FooApi 'Asynchronous
  AddFooObserver :: SomeObserver FooApi -> Api FooApi 'Asynchronous
  RemoveFooObserver :: SomeObserver FooApi -> Api FooApi 'Asynchronous
  deriving (Typeable)

deriving instance Show (Api FooApi x)

spawnFoo
  :: forall q r
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Member (Logs LogMessage) r
     )
  => SchedulerProxy q
  -> Eff r (Server FooApi)
spawnFoo px = spawnServer px (castHandlerForever handleFooCast)
  where handleFooCast = error "TODE"
