-- | Functions for 'Api' clients.
--
-- This modules is required to write clients that consume an 'Api'.
module Control.Eff.Concurrent.Api.Client
  ( -- * Calling APIs directly
    cast
  , call
  , callWithTimeout
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
import           Control.Eff.Concurrent.Process.Timer
import           Control.Eff.Log
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
--
-- __Always prefer 'callWithTimeout' over 'call'__
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
  callRef <- makeReference
  fromPid <- self
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


-- | Send an 'Api' request and wait for the server to return a result value.
--
-- The type signature enforces that the corresponding 'Api' clause is
-- 'Synchronous'.
--
-- If the server that was called dies, this function interrupts the
-- process with 'ProcessDown'.
-- If the server takes longer to reply than the given timeout, this
-- function interrupts the process with 'TimeoutInterrupt'.
--
-- __Always prefer this function over 'call'__
--
-- @since 0.22.0
callWithTimeout
  :: forall result api r q
   . ( SetMember Process (Process q) r
     , Member Interrupts r
     , Typeable api
     , Typeable (Api api ( 'Synchronous result))
     , NFData (Api api ( 'Synchronous result))
     , Typeable result
     , NFData result
     , Show result
     , Member Logs r
     , Lifted IO q
     , Lifted IO r
     , HasCallStack
     )
  => Server api
  -> Api api ( 'Synchronous result)
  -> Timeout
  -> Eff r result
callWithTimeout serverP@(Server pidInternal) req timeOut = do
  fromPid <- self
  callRef <- makeReference
  let requestMessage = Call callRef fromPid $! req
  sendMessage pidInternal requestMessage
  let selectResult =
        let extractResult
              :: Reply (Api api ( 'Synchronous result)) -> Maybe result
            extractResult (Reply _pxResult callRefMsg result) =
              if callRefMsg == callRef then Just result else Nothing
        in selectMessageWith extractResult
  resultOrError <- receiveSelectedWithMonitorAfter pidInternal selectResult timeOut
  let onTimeout timerRef = do
        let msg = "call timed out after "
                  ++ show timeOut ++ " to server: "
                  ++ show serverP ++ " from "
                  ++ show fromPid ++ " "
                  ++ show timerRef
        logWarning' msg
        interrupt (TimeoutInterrupt msg)
      onProcDown p = do
        logWarning' ("call to dead server: "++ show serverP ++ " from " ++ show fromPid)
        interrupt (becauseProcessIsDown p)
  either (either onProcDown onTimeout) return resultOrError

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
