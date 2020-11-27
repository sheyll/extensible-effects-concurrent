-- | Functions for protocol clients.
--
-- This modules is required to write clients that send'Pdu's.
module Control.Eff.Concurrent.Protocol.Client
  ( -- * Calling APIs directly
    cast,
    call,
    callWithTimeout,

    -- * Server Process Registration
    castSingleton,
    castEndpointReader,
    callSingleton,
    callEndpointReader,
    HasEndpointReader,
    EndpointReader,
    askEndpoint,
    runEndpointReader,
  )
where

import Control.Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Wrapper
import Control.Eff.Log
import Control.Eff.Reader.Strict
import Data.Typeable (Typeable)
import GHC.Stack

-- instance ToProtocolName protocol => ToProtocolName (Endpoint protocol) where
--   toProtocolName = toProtocolName @protocol <> "_ep"
--
-- instance ToProtocolName protocol => ToTypeLogMsg (Endpoint protocol) where
--   toTypeLogMsg _ = packLogMsg (toProtocolName @(Endpoint protocol))
--

-- | Send a request 'Pdu' that has no reply and return immediately.
--
-- The type signature enforces that the corresponding 'Pdu' clause is
-- 'Asynchronous'. The operation never fails, if it is important to know if the
-- message was delivered, use 'call' instead.
--
-- The message will be reduced to normal form ('rnf') in the caller process.
cast ::
  forall destination protocol r q.
  ( HasProcesses r q,
    TangiblePdu destination 'Asynchronous,
    Embeds destination protocol
  ) =>
  Endpoint destination ->
  Pdu protocol 'Asynchronous ->
  Eff r ()
cast (Endpoint pid) castMsg = sendMessage pid (Cast (embedPdu @destination castMsg))

-- | Send a request 'Pdu' and wait for the server to return a result value.
--
-- The type signature enforces that the corresponding 'Pdu' clause is
-- 'Synchronous'.
--
-- __Always prefer 'callWithTimeout' over 'call'__
call ::
  forall result destination protocol r q.
  ( HasProcesses r q,
    TangiblePdu destination ('Synchronous result),
    Tangible result,
    Embeds destination protocol
  ) =>
  Endpoint destination ->
  Pdu protocol ('Synchronous result) ->
  Eff r result
call (Endpoint pidInternal) req = do
  callRef <- makeReference
  fromPid <- self
  let requestMessage = Call origin $! (embedPdu @destination req)
      origin = RequestOrigin @destination @result fromPid callRef
  sendMessage pidInternal requestMessage
  let selectResult :: MessageSelector result
      selectResult =
        let extractResult ::
              Reply destination result -> Maybe result
            extractResult (Reply origin' result) =
              if origin == origin' then Just result else Nothing
         in selectMessageWith extractResult
  resultOrError <- receiveWithMonitor pidInternal selectResult
  either (interrupt . becauseOtherProcessNotRunning) return resultOrError

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
callWithTimeout ::
  forall result destination protocol r q.
  ( HasProcesses r q,
    TangiblePdu destination ('Synchronous result),
    Tangible result,
    Member Logs q,
    Member Logs r,
    HasCallStack,
    Embeds destination protocol
  ) =>
  Endpoint destination ->
  Pdu protocol ('Synchronous result) ->
  Timeout ->
  Eff r result
callWithTimeout serverP@(Endpoint pidInternal) req timeOut = do
  fromPid <- self
  callRef <- makeReference
  let requestMessage = Call origin $! embedPdu @destination req
      origin = RequestOrigin @destination @result fromPid callRef
  sendMessage pidInternal requestMessage
  let selectResult =
        let extractResult ::
              Reply destination result -> Maybe result
            extractResult (Reply origin' result) =
              if origin == origin' then Just result else Nothing
         in selectMessageWith extractResult
  let timerTitle = MkProcessTitle (packLogMsg "call-timeout")
      initialLog =
        packLogMsg "call-timeout from: "
          <> toLogMsg fromPid
          <> packLogMsg " to: "
          <> toLogMsg serverP
  resultOrError <- receiveSelectedWithMonitorAfterWithTitle pidInternal selectResult timeOut timerTitle initialLog
  let onTimeout timerRef = do
        let msg =
              packLogMsg "call-timeout "
                <> toLogMsg timerRef
                <> packLogMsg " for call from "
                <> toLogMsg fromPid
                <> packLogMsg " to "
                <> toLogMsg serverP
                <> packLogMsg " timed out after "
                <> toLogMsg timeOut
        logWarning msg
        interrupt (TimeoutInterrupt msg)
      onProcDown p = do
        logWarning (LABEL "call to dead server" serverP) (LABEL "from" fromPid)
        interrupt (becauseOtherProcessNotRunning p)
  either (either onProcDown onTimeout) return resultOrError

-- | Instead of passing around a 'Endpoint' value and passing to functions like
-- 'cast' or 'call', a 'Endpoint' can provided by a 'Reader' effect, if there is
-- only a __single server__ for a given 'Pdu' instance. This type alias is
-- convenience to express that an effect has 'Process' and a reader for a
-- 'Endpoint'.
type HasEndpointReader o r =
  ( Typeable o,
    Member (EndpointReader o) r
  )

-- | The reader effect for 'ProcessId's for 'Pdu's, see 'runEndpointReader'
type EndpointReader o = Reader (Endpoint o)

-- | Run a reader effect that contains __the one__ server handling a specific
-- 'Pdu' instance.
runEndpointReader :: Endpoint o -> Eff (EndpointReader o ': r) a -> Eff r a
runEndpointReader = runReader

-- | Get the 'Endpoint' registered with 'runEndpointReader'.
askEndpoint :: Member (EndpointReader o) e => Eff e (Endpoint o)
askEndpoint = ask

-- | Like 'call' but take the 'Endpoint' from the reader provided by
-- 'runEndpointReader'.
--
-- When working with an embedded 'Pdu' use 'callSingleton'.
callEndpointReader ::
  forall reply o r q.
  ( HasEndpointReader o r,
    Tangible reply,
    TangiblePdu o ('Synchronous reply),
    HasProcesses r q,
    Embeds o o
  ) =>
  Pdu o ('Synchronous reply) ->
  Eff r reply
callEndpointReader method = do
  serverPid <- askEndpoint @o
  call @reply @o @o serverPid method

-- | Like 'cast' but take the 'Endpoint' from the reader provided by
-- 'runEndpointReader'.
--
-- When working with an embedded 'Pdu' use 'castSingleton'.
castEndpointReader ::
  forall o r q.
  ( HasEndpointReader o r,
    HasProcesses r q,
    TangiblePdu o 'Asynchronous,
    Embeds o o
  ) =>
  Pdu o 'Asynchronous ->
  Eff r ()
castEndpointReader method = do
  serverPid <- askEndpoint @o
  cast @o @o serverPid method

-- | Like 'callEndpointReader', uses 'embedPdu' to embed the value.
--
-- This function makes use of AmbigousTypes and TypeApplications.
--
-- When not working with an embedded 'Pdu' use 'callEndpointReader'.
--
-- @since 0.25.1
callSingleton ::
  forall outer inner reply q e.
  ( HasCallStack,
    Member (EndpointReader outer) e,
    Embeds outer inner,
    Embeds outer outer,
    HasProcesses e q,
    TangiblePdu outer ('Synchronous reply),
    Tangible reply
  ) =>
  Pdu inner ('Synchronous reply) ->
  Eff e reply
callSingleton = withFrozenCallStack $ \p -> callEndpointReader (embedPdu @outer @inner p)

-- | Like 'castEndpointReader', but uses 'embedPdu' to embed the value.
--
-- This function makes use of AmbigousTypes and TypeApplications.
--
-- When not working with an embedded 'Pdu' use 'castEndpointReader'.
--
-- @since 0.25.1
castSingleton ::
  forall outer inner q e.
  ( HasCallStack,
    Member (EndpointReader outer) e,
    HasProcesses e q,
    TangiblePdu outer 'Asynchronous,
    Embeds outer inner,
    Embeds outer outer
  ) =>
  Pdu inner 'Asynchronous ->
  Eff e ()
castSingleton = withFrozenCallStack $ \p -> castEndpointReader (embedPdu @outer @inner p)
