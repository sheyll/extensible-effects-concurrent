-- | Functions for 'Api' clients.
--
-- This modules is required to write clients that consume an 'Api'.
module Control.Eff.Concurrent.Api.Client
  ( -- * Calling APIs directly
    cast
  , castChecked
  , call
  -- * Server Process Registration
  , castRegistered
  , callRegistered
  , callRegisteredA
  , ServesApi
  , ServerReader
  , whereIsServer
  , registerServer
  )
where

import           Control.Applicative
import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Internal
import           Control.Eff.Concurrent.Process
import           Data.Dynamic
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                )
import           GHC.Stack

-- | Send an 'Api' request that has no return value and return as fast as
-- possible. The type signature enforces that the corresponding 'Api' clause is
-- 'Asynchronous'. Return @True@ if the message was sent to the process. Note
-- that this is totally not the same as that the request was successfully
-- handled. If that is important, use 'call' instead.
castChecked
  :: forall r q o
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Typeable o
     , Typeable (Api o 'Asynchronous)
     )
  => SchedulerProxy q
  -> Server o
  -> Api o 'Asynchronous
  -> Eff r Bool
castChecked px (Server pid) castMsg =
  sendMessageChecked px pid (toDyn $! (Cast $! castMsg))

-- | Send an 'Api' request that has no return value and return as fast as
-- possible. The type signature enforces that the corresponding 'Api' clause is
-- 'Asynchronous'.
cast
  :: forall r q o
   . ( HasCallStack
     , SetMember Process (Process q) r
     , Typeable o
     , Typeable (Api o 'Asynchronous)
     )
  => SchedulerProxy q
  -> Server o
  -> Api o 'Asynchronous
  -> Eff r ()
cast px toServer apiRequest = do
  _ <- castChecked px toServer apiRequest
  return ()

-- | Send an 'Api' request and wait for the server to return a result value.
--
-- The type signature enforces that the corresponding 'Api' clause is
-- 'Synchronous'.
call
  :: forall result api r q
   . ( SetMember Process (Process q) r
     , Typeable api
     , Typeable (Api api ( 'Synchronous result))
     , Typeable result
     , HasCallStack
     )
  => SchedulerProxy q
  -> Server api
  -> Api api ( 'Synchronous result)
  -> Eff r result
call px (Server pidInt) req = do
  fromPid <- self px
  let requestMessage = Call fromPid $! req
  wasSent <- sendMessageChecked px pidInt (toDyn $! requestMessage)
  if wasSent
    then
      let extractResult :: Response api result -> result
          extractResult (Response _pxResult result) = result
      in  extractResult <$> receiveMessageAs px
    else raiseError
      px
      ("Could not send request message " ++ show (typeRep requestMessage))


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
registerServer :: Server o -> Eff (ServerReader o ': r) a -> Eff r a
registerServer = runReader

-- | Get the 'Server' registered with 'registerServer'.
whereIsServer :: Member (ServerReader o) e => Eff e (Server o)
whereIsServer = ask

-- | Like 'call' but take the 'Server' from the reader provided by
-- 'registerServer'.
callRegistered
  :: (Typeable reply, ServesApi o r q)
  => SchedulerProxy q
  -> Api o ( 'Synchronous reply)
  -> Eff r reply
callRegistered px method = do
  serverPid <- whereIsServer
  call px serverPid method

-- | Like 'callRegistered' but also catch errors raised if e.g. the server
-- crashed. By allowing 'Alternative' instances to contain the reply,
-- application level errors can be combined with errors rising from inter
-- process communication.
callRegisteredA
  :: forall r q o f reply
   . (Alternative f, Typeable f, Typeable reply, ServesApi o r q)
  => SchedulerProxy q
  -> Api o ( 'Synchronous (f reply))
  -> Eff r (f reply)
callRegisteredA px method =
  catchRaisedError px (const (return (empty @f))) (callRegistered px method)

-- | Like 'cast' but take the 'Server' from the reader provided by
-- 'registerServer'.
castRegistered
  :: (Typeable o, ServesApi o r q)
  => SchedulerProxy q
  -> Api o 'Asynchronous
  -> Eff r ()
castRegistered px method = do
  serverPid <- whereIsServer
  cast px serverPid method
