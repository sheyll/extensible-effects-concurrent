-- | A better, more safe implementation of the Erlang/OTP gen_server behaviour.
-- **PLANNED TODO**
-- @since 0.24.0
module Control.Eff.Concurrent.Api.GenServer
  ( GenServer(..)
  , runGenServer
  )
where

import Control.Eff
import Control.Eff.Concurrent.Api
import Control.Eff.Concurrent.Process
import Control.Eff.Log
import Control.Eff.State.Lazy
import Control.Lens
import Data.Kind

-- | A type class for 'Api' values that have an implementation
-- which handles the 'Api'.
class (Tangible (GenServerState a)) => GenServer a eff where
  type GenServerState a :: Type
  genServerInit :: ('[Interrupts, Logs] <:: eff, SetMember Process (Process q) eff) => a -> Eff eff (GenServerState a)
  genServerHandle ::
       ('[Interrupts, Logs] <:: eff, SetMember Process (Process q) eff)
    => Api a v
    -> Eff (State (GenServerState a) ': eff) (ApiReply v)
  genServerInterrupt ::
       ('[Interrupts, Logs] <:: eff, SetMember Process (Process q) eff)
    => Interrupt 'Recoverable
    -> Eff (State (GenServerState a) ': eff) ()



runGenServer ::
     forall q e h a. (GenServer a e, SetMember Process (Process q) e, Member Interrupts e, LogsTo h e)
  => a
  -> Eff e (Server a)
runGenServer _initArg = error "TODO"