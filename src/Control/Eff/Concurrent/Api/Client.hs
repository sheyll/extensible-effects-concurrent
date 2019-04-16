-- | Functions for 'Api' clients.
--
-- This modules is required to write clients that consume an 'Api'.
module Control.Eff.Concurrent.Api.Client
  ( -- * Calling APIs directly
    cast
  , call
  -- * Server Process Registration
  , castRegistered
  , callRegistered
  , ServesApi
  , ServerReader
  , whereIsServer
  , registerServer
  )
where

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Request
import           Control.Eff.Concurrent.Process
import           Data.Typeable                  ( Typeable )
import           Control.DeepSeq
import           GHC.Stack

-- | Send an 'Api' request that has no return value and return as fast as
-- possible. The type signature enforces that the corresponding 'Api' clause is
-- 'Asynchronous'. The operation never fails, if it is important to know if the
-- message was delivered, use 'call' instead.
--
-- The message will be reduced to normal form ('rnf') in the caller process.
cast
  :: forall r q o
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Member Interrupts r
     , Typeable o
     , Typeable (Api o 'Asynchronous)
     , NFData (Api o 'Asynchronous)
     )
  => Server o
  -> Api o 'Asynchronous
  -> Eff r ()
cast (Server pid) castMsg = sendMessage pid (Cast castMsg)

-- | Send an 'Api' request and wait for the server to return a result value.
--
-- The type signature enforces that the corresponding 'Api' clause is
-- 'Synchronous'.
call
  :: forall result api r q
   . ( SetMember Process (Process q) r
     , Member Interrupts r
     , Typeable api
     , Typeable (Api api ( 'Synchronous result))
     , NFData (Api api ( 'Synchronous result))
     , Typeable result
     , NFData result
     , Show result
     , HasCallStack
     )
  => Server api
  -> Api api ( 'Synchronous result)
  -> Eff r result
call (Server pidInternal) req = do
  fromPid <- self
  callRef <- makeReference
  let requestMessage = Call callRef fromPid $! req
  sendMessage pidInternal requestMessage
  let selectResult :: MessageSelector result
      selectResult =
        let extractResult
              :: Reply (Api api ( 'Synchronous result)) -> Maybe result
            extractResult (Reply _pxResult callRefMsg result) =
              if callRefMsg == callRef then Just result else Nothing
        in  selectMessageWith extractResult
  resultOrError <- receiveWithMonitor pidInternal selectResult
  either (interrupt . becauseProcessIsDown) return resultOrError

-- | Instead of passing around a 'Server' value and passing to functions like
-- 'cast' or 'call', a 'Server' can provided by a 'Reader' effect, if there is
-- only a __single server__ for a given 'Api' instance. This type alias is
-- convenience to express that an effect has 'Process' and a reader for a
-- 'Server'.
type ServesApi o r q =
  ( Typeable o
  , SetMember Process (Process q) r
  , Member (ServerReader o) r
  )

-- | The reader effect for 'ProcessId's for 'Api's, see 'registerServer'
type ServerReader o = Reader (Server o)

-- | Run a reader effect that contains __the one__ server handling a specific
-- 'Api' instance.
registerServer
  :: HasCallStack => Server o -> Eff (ServerReader o ': r) a -> Eff r a
registerServer = runReader

-- | Get the 'Server' registered with 'registerServer'.
whereIsServer :: Member (ServerReader o) e => Eff e (Server o)
whereIsServer = ask

-- | Like 'call' but take the 'Server' from the reader provided by
-- 'registerServer'.
callRegistered
  :: ( Typeable reply
     , ServesApi o r q
     , HasCallStack
     , NFData reply
     , Show reply
     , NFData (Api o ( 'Synchronous reply))
     , Member Interrupts r
     )
  => Api o ( 'Synchronous reply)
  -> Eff r reply
callRegistered method = do
  serverPid <- whereIsServer
  call serverPid method

-- | Like 'cast' but take the 'Server' from the reader provided by
-- 'registerServer'.
castRegistered
  :: (Typeable o, ServesApi o r q, HasCallStack, Member Interrupts r, NFData (Api o 'Asynchronous))
  => Api o 'Asynchronous
  -> Eff r ()
castRegistered method = do
  serverPid <- whereIsServer
  cast serverPid method
