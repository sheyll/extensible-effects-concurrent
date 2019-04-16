-- | A better, more safe implementation of the Erlang/OTP gen_server behaviour.
-- **PLANNED TODO**
-- @since 0.23.0
module Control.Eff.Concurrent.Api.GenServer
  (
  ) where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent.Api
import Control.Eff.Concurrent.Process
import Control.Eff.Log
import Control.Eff.State.Lazy
import Data.Dynamic
import Data.Kind
import Data.Text as T

-- | A type class for 'Api' values that have an implementation
-- which handles the 'Api'.
class (Typeable (GenServerState a), NFData (GenServerState a)) =>
      GenServer a eff
  where
  type GenServerState a :: Type
  genServerInit :: ('[Interrupts, Logs] <:: eff, SetMember Process (Process q) eff) => a -> Eff eff (GenServerState a)
  genServerHandle ::
       ('[Interrupts, Logs] <:: eff, SetMember Process (Process q) eff)
    => Api a v
    -> Eff (State (GenServerState a) ': eff) (ApiReply v)
  genServerInterrupt ::
       ('[Interrupts, Logs] <:: eff, SetMember Process (Process q) eff)
    => InterruptReason
    -> Eff (State (GenServerState a) ': eff) ()
  genServerInfoCommand :: Api a ('Synchronous Text)

type family ApiReply (s :: Synchronicity) where
  ApiReply ('Synchronous t) = Maybe t
  ApiReply 'Asynchronous = ()

data SomeMessage a =
  MkSomeMessage

data instance  Api (SomeMessage a) s where
        SomeMessage :: a -> Api (SomeMessage a) 'Asynchronous

runGenServer ::
     forall q e h a. (GenServer a e, SetMember Process (Process q) e, Member Interrupts e, LogsTo h e)
  => a
  -> Eff e (Server a)
runGenServer initArg = error "TODO"