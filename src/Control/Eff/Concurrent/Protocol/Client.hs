-- | Functions for protocol clients.
--
-- This modules is required to write clients that send'Pdu's.
module Control.Eff.Concurrent.Protocol.Client
  ( -- * Calling APIs directly
    cast
  , call
  , callWithTimeout
  -- * Server Process Registration
  , castEndpointReader
  , callEndpointReader
  , ServesProtocol
  , EndpointReader
  , askEndpoint
  , runEndpointReader
  )
where

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Concurrent.Protocol
import           Control.Eff.Concurrent.Protocol.Request
import           Control.Eff.Concurrent.Process
import           Control.Eff.Concurrent.Process.Timer
import           Control.Eff.Log
import           Data.Typeable                  ( Typeable )
import           GHC.Stack

-- | Send a request 'Pdu' that has no reply and return immediately.
--
-- The type signature enforces that the corresponding 'Pdu' clause is
-- 'Asynchronous'. The operation never fails, if it is important to know if the
-- message was delivered, use 'call' instead.
--
-- The message will be reduced to normal form ('rnf') in the caller process.
cast
  :: forall o' o r q
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Member Interrupts r
     , TangiblePdu o' 'Asynchronous
     , TangiblePdu o 'Asynchronous
     , EmbedProtocol o' o
     )
  => Endpoint o'
  -> Pdu o 'Asynchronous
  -> Eff r ()
cast (Endpoint pid) castMsg = sendMessage pid (Cast (embedPdu @o' castMsg))

-- | Send a request 'Pdu' and wait for the server to return a result value.
--
-- The type signature enforces that the corresponding 'Pdu' clause is
-- 'Synchronous'.
--
-- __Always prefer 'callWithTimeout' over 'call'__
call
  :: forall result protocol' protocol r q
   . ( SetMember Process (Process q) r
     , Member Interrupts r
     , TangiblePdu protocol' ( 'Synchronous result)
     , TangiblePdu protocol ( 'Synchronous result)
     , EmbedProtocol protocol' protocol
     , Tangible result
     , HasCallStack
     )
  => Endpoint protocol'
  -> Pdu protocol ( 'Synchronous result)
  -> Eff r result
call (Endpoint pidInternal) req = do
  callRef <- makeReference
  fromPid <- self
  let requestMessage = Call origin $! (embedPdu @protocol' req)
      origin = RequestOrigin @protocol' @result fromPid callRef
  sendMessage pidInternal requestMessage
  let selectResult :: MessageSelector result
      selectResult =
        let extractResult
              :: Reply protocol' result -> Maybe result
            extractResult (Reply origin' result) =
              if origin == origin' then Just result else Nothing
        in  selectMessageWith extractResult
  resultOrError <- receiveWithMonitor pidInternal selectResult
  either (interrupt . becauseProcessIsDown) return resultOrError


-- | Send an request 'Pdu' and wait for the server to return a result value.
--
-- The type signature enforces that the corresponding 'Pdu' clause is
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
  :: forall result protocol' protocol r q
   . ( SetMember Process (Process q) r
     , Member Interrupts r
     , TangiblePdu protocol' ( 'Synchronous result)
     , TangiblePdu protocol ( 'Synchronous result)
     , EmbedProtocol protocol' protocol
     , Tangible result
     , Member Logs r
     , Lifted IO q
     , Lifted IO r
     , HasCallStack
     )
  => Endpoint protocol'
  -> Pdu protocol ( 'Synchronous result)
  -> Timeout
  -> Eff r result
callWithTimeout serverP@(Endpoint pidInternal) req timeOut = do
  fromPid <- self
  callRef <- makeReference
  let requestMessage = Call origin $! embedPdu @protocol' req
      origin = RequestOrigin @protocol' @result fromPid callRef
  sendMessage pidInternal requestMessage
  let selectResult =
        let extractResult
              :: Reply protocol' result -> Maybe result
            extractResult (Reply origin' result) =
              if origin == origin' then Just result else Nothing
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

-- | Instead of passing around a 'Endpoint' value and passing to functions like
-- 'cast' or 'call', a 'Endpoint' can provided by a 'Reader' effect, if there is
-- only a __single server__ for a given 'Pdu' instance. This type alias is
-- convenience to express that an effect has 'Process' and a reader for a
-- 'Endpoint'.
type ServesProtocol o r q =
  ( Typeable o
  , SetMember Process (Process q) r
  , Member (EndpointReader o) r
  )

-- | The reader effect for 'ProcessId's for 'Pdu's, see 'runEndpointReader'
type EndpointReader o = Reader (Endpoint o)

-- | Run a reader effect that contains __the one__ server handling a specific
-- 'Pdu' instance.
runEndpointReader
  :: HasCallStack => Endpoint o -> Eff (EndpointReader o ': r) a -> Eff r a
runEndpointReader = runReader

-- | Get the 'Endpoint' registered with 'runEndpointReader'.
askEndpoint :: Member (EndpointReader o) e => Eff e (Endpoint o)
askEndpoint = ask

-- | Like 'call' but take the 'Endpoint' from the reader provided by
-- 'runEndpointReader'.
callEndpointReader
  :: forall reply o r q .
     ( ServesProtocol o r q
     , HasCallStack
     , Tangible reply
     , TangiblePdu o ( 'Synchronous reply)
     , Member Interrupts r
     )
  => Pdu o ( 'Synchronous reply)
  -> Eff r reply
callEndpointReader method = do
  serverPid <- askEndpoint @o
  call @reply @o @o serverPid method

-- | Like 'cast' but take the 'Endpoint' from the reader provided by
-- 'runEndpointReader'.
castEndpointReader
  :: forall o r q .
     ( ServesProtocol o r q
     , HasCallStack
     , Member Interrupts r
     , TangiblePdu o 'Asynchronous
     )
  => Pdu o 'Asynchronous
  -> Eff r ()
castEndpointReader method = do
  serverPid <- askEndpoint @o
  cast @o @o serverPid method
