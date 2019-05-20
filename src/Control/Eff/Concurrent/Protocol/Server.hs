-- | A better, more safe implementation of the Erlang/OTP gen_server behaviour.
-- **PLANNED TODO**
-- @since 0.24.0
module Control.Eff.Concurrent.Protocol.Server
  ( Server(..)
  , ServerLoopEvent(..)
  , spawnProtocolServer
  , spawnLinkProtocolServer
  , protocolServerLoop
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Request
import Control.Eff.Log
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Data.Default
import Data.Kind
import Data.Typeable
import Data.Type.Pretty
import GHC.Stack (HasCallStack)
import GHC.Generics

-- | A type class for 'Pdu' values that have an implementation
-- which handles the corresponding protocol.
--
-- @since 0.24.0
class
  ( Typeable (Protocol a)
  , Typeable a
  , Tangible (ServerEnv a)
  , Tangible (ServerState a)
  ) =>
      Server (a :: Type) e
  where
  data ServerArgument a e
  type Protocol a :: Type
  type Protocol a = a
  type ServerState a :: Type
  type ServerState a = ()
  type ServerEnv a :: Type
  type ServerEnv a = ()

  serverInit ::
       ServerArgument a e
    -> Eff e (ServerState a, ServerEnv a)

  default serverInit ::
       (Default (ServerState a), Default (ServerEnv a))
    => ServerArgument a e
    -> Eff e (ServerState a, ServerEnv a)
  serverInit _ = pure (def, def)

  stepServerLoop ::
       ServerArgument a e
    -> ServerLoopEvent (Protocol a)
    -> Eff (State (ServerState a) ': Reader (ServerEnv a) ': e) ()

  recoverFromInterrupt ::
       ServerArgument a e
    -> Interrupt 'Recoverable
    -> Eff (State (ServerState a) ': Reader (ServerEnv a) ': e) ()

  default recoverFromInterrupt ::
       (SetMember Process (Process q) e)
    => ServerArgument a e
    -> Interrupt 'Recoverable
    -> Eff (State (ServerState a) ': Reader (ServerEnv a) ': e) ()
  recoverFromInterrupt _ = exitBecause . interruptToExit

-- | There is an internal protocol used by the protocol server loop
-- to communicate certain events, such that a

-- | Execute the server loop.
--
-- @since 0.24.0
spawnProtocolServer
  :: forall a q h
  . ( Server a (InterruptableProcess q)
    , LogsTo h (InterruptableProcess q)
    , HasCallStack)
  => ServerArgument a (InterruptableProcess q) -> Eff (InterruptableProcess q) (Endpoint (Protocol a))
spawnProtocolServer a = asEndpoint <$> spawn (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
spawnLinkProtocolServer
  :: forall a q h . (Server a (InterruptableProcess q), LogsTo h (InterruptableProcess q), HasCallStack)
  => ServerArgument a (InterruptableProcess q) -> Eff (InterruptableProcess q) (Endpoint (Protocol a))
spawnLinkProtocolServer a = asEndpoint <$> spawnLink (protocolServerLoop a)

-- | Execute the server loop.
--
-- @since 0.24.0
protocolServerLoop
     :: forall q e h a
     . ( Server a e
       , SetMember Process (Process q) e
       , Member Interrupts e
       , LogsTo h e
       )
  => ServerArgument a e -> Eff e ()
protocolServerLoop a = do
  (st, env) <- serverInit a
  _ <- runReader env (runState st (receiveSelectedLoop sel mainLoop))
  return ()
  where
    sel :: MessageSelector (ServerLoopEvent (Protocol a))
    sel =
          ServerLoopRequest      <$> selectMessage @(Request (Protocol a))
      <|> ServerLoopProcessDown  <$> selectMessage @ProcessDown
      <|> ServerLoopTimerElapsed <$> selectMessage @TimerElapsed
      <|> ServerLoopOtherMessage <$> selectAnyMessage
    handleInt i = do
      recoverFromInterrupt a i
      pure (Just ())
    mainLoop ::
         (Typeable a)
      => Either (Interrupt 'Recoverable) (ServerLoopEvent (Protocol a))
      -> Eff (State (ServerState a) ': Reader (ServerEnv a) ': e) (Maybe ())
    mainLoop (Left i) = handleInt i
    mainLoop (Right i) = do
      stepServerLoop a i
      pure (Just ())

-- | Internal protocol to communicate incoming messages and other events to the
-- instances of 'Server'.
--
-- Note that this is required to receive any kind of messages in 'protocolServerLoop'.
--
-- @since 0.24.0
data ServerLoopEvent a =
    ServerLoopRequest (Request a)
  | ServerLoopProcessDown ProcessDown
  | ServerLoopTimerElapsed TimerElapsed
  | ServerLoopOtherMessage StrictDynamic
  deriving (Show,Generic,Typeable)

instance NFData a => NFData (ServerLoopEvent a) where
   rnf = \case
       ServerLoopRequest r      -> rnf r
       ServerLoopProcessDown r  -> rnf r
       ServerLoopTimerElapsed r -> rnf r
       ServerLoopOtherMessage r -> r `seq` ()


type instance ToPretty (ServerLoopEvent a) = PutStr "sever-loop-event<" <++> ToPretty a <++> PutStr ">"
