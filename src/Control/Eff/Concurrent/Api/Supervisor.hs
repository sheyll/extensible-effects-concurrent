-- | A process supervisor spawns and monitors child processes.
--
-- The child processes are mapped to symbolic identifier values: Child-IDs.
--
-- Opposed to /'ProcessId's/ these child IDs remain consistent between child
-- restarts.
--
-- A supervisor only knows how to spawn a single kind of child process.
--
-- When a supervisor exists, all child processes are exited, too.
--
-- This module is far simpler than the Erlang/OTP counter part, of a @simple_one_for_one@
-- supervisor.
--
-- @since 0.23.0
module Control.Eff.Concurrent.Api.Supervisor
  ( Sup()
  , SpawnFun
  , SpawnErr
  , startLink
  , getDiagnosticInfo
  , spawnChild
  ) where

import Control.DeepSeq (NFData(rnf))
import Control.Eff as Eff
import Control.Eff.Concurrent.Api
import Control.Eff.Concurrent.Api.Client
import Control.Eff.Concurrent.Api.Server
import Control.Eff.Concurrent.Api.Supervisor.InternalState
import Control.Eff.Concurrent.Process
import Control.Eff.Extend (raise)
import Control.Eff.Log
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
import GHC.Stack

-- * Supervisor Server API
-- ** Types
-- | 'Api' type for supervisor processes.
--
-- The /supervisor/ process contains a 'SpawnFun' from which it can spawn new child processes.
--
-- The supervisor maps an identifier value of type @childId@ to a 'ProcessId' and a
-- 'spawResult' type.
--
-- This 'o' is likely a tuple of 'Server' process ids that allow type-safe interaction with
-- the process.
--
-- @since 0.23.0
data Sup childId spawnResult =
  MkSup
  deriving (Show, Typeable)

-- | Spawn and monitor a new child process with the given id.
--
-- If another process with this ID already exists
--
-- The process-id returned from this function is kept private, but
-- it is possible to return '(ProcessId, ProcessId)' for example.
--
-- The function is expected to return a value - usually a tuple of 'Server' values for
-- the different aspects of a process, such as sending API requests, managing
-- observers and receiving other auxiliary API messages.
--
-- @since 0.23.0
type SpawnFun i e o = i -> Eff e (o, ProcessId)

-- | The 'Api' instance contains methods to start, stop and lookup a child
-- process, as well as a diagnostic callback.
--
-- @since 0.23.0
data instance  Api (Sup i o) r where
        StartC ::
          i -> Api (Sup i o) ('Synchronous (Either (SpawnErr i o) o))
        StopC :: i -> Api (Sup i o) ('Synchronous (Maybe ()))
        LookupC :: i -> Api (Sup i o) ('Synchronous (Maybe o))
        GetDiagnosticInfo :: Api (Sup i o) ('Synchronous Text)
    deriving Typeable

instance (Show i) => Show (Api (Sup i o) ('Synchronous r)) where
  showsPrec d (StartC c) = showParen (d >= 10) (showString "StartC " . showsPrec 10 c)
  showsPrec d (StopC c) = showParen (d >= 10) (showString "StopC " . showsPrec 10 c)
  showsPrec d (LookupC c) = showParen (d >= 10) (showString "LookupC " . showsPrec 10 c)
  showsPrec _ GetDiagnosticInfo = showString "GetDiagnosticInfo"

instance (NFData i) => NFData (Api (Sup i o) ('Synchronous r)) where
  rnf (StartC ci) = rnf ci
  rnf (StopC ci) = rnf ci
  rnf (LookupC ci) = rnf ci
  rnf GetDiagnosticInfo = ()

-- | Runtime-Errors occurring when spawning child-processes.
--
-- @since 0.23.0
data SpawnErr i o
  = AlreadyStarted i
                   o
  | StartFailed i
  deriving (Eq, Ord, Show, Typeable, Generic)

instance (NFData i, NFData o) => NFData (SpawnErr i o)

-- ** Functions
-- | Start and link a new supervisor process with the given 'SpawnFun'unction.
--
-- To spawn new child processes use 'spawnChild'.
--
-- @since 0.23.0
startLink ::
     forall i e o q0. (HasCallStack, Member Logs e, Ord i, NFData i, NFData o, Typeable i, Typeable o, Show i, Show o)
  => SpawnFun i (InterruptableProcess e) o
  -> Eff (InterruptableProcess e) (Server (Sup i o))
startLink childSpawner = do
  (sup, _supPid) <- spawnApiServerStateful initChildren (onRequest ^: onMessage) onInterrupt
  return sup
  where
    initChildren :: Eff (InterruptableProcess e) (Children i o)
    initChildren = return def
    onRequest :: MessageCallback (Sup i o) (State (Children i o) ': InterruptableProcess e)
    onRequest = handleCalls onCall
      where
        onCall ::
             Api (Sup i o) ('Synchronous reply)
          -> (Eff (State (Children i o) ': InterruptableProcess e) (Maybe reply, CallbackResult 'Recoverable) -> st)
          -> st
        onCall GetDiagnosticInfo k = k ((, AwaitNext) . Just . pack . show <$> getChildren)
        onCall (LookupC i) k = k ((, AwaitNext) . Just <$> lookupChildById i)
        onCall (StartC i) k =
          k $ do
            (o, _cPid) <- raise (childSpawner i)
            putChild i o
            return (Just (Right o), AwaitNext)
    onMessage :: MessageCallback '[] (State (Children i o) ': InterruptableProcess e)
    onMessage = handleAnyMessages onInfo
      where
        onInfo d = do
          logInfo (pack (show d))
          return AwaitNext
    onInterrupt :: InterruptCallback (State (Children i o) ': ConsProcess e)
    onInterrupt =
      InterruptCallback $ \e -> do
        let (logSev, exitReason) =
              case e of
                NormalExitRequested ->
                  (debugSeverity, ExitNormally)
                _ ->
                  (warningSeverity, NotRecovered (ErrorInterrupt ("supervisor interrupted: " <> show e)))
        logWithSeverity logSev ("supervisor stopping: " <> pack (show e))
        pure (StopServer exitReason)

-- | Start, link and monitor a new child process using the 'SpawnFun' passed
-- to 'startLink'.
--
-- @since 0.23.0
spawnChild ::
     ( HasCallStack
     , Member Interrupts e
     , Member Logs e
     , Typeable i
     , Typeable o
     , NFData i
     , NFData o
     , Show i
     , Show o
     , SetMember Process (Process q0) e
     )
  => Server (Sup i o)
  -> i
  -> Eff e (Either (SpawnErr i o) o)
spawnChild svr cId = call svr (StartC cId)

-- | Lookup the given child-id and return the output value of the 'SpawnFun'
--   if the client process exists.
--
-- @since 0.23.0
lookupChild ::
     ( HasCallStack
     , Member Interrupts e
     , Member Logs e
     , Typeable i
     , Typeable o
     , NFData i
     , NFData o
     , Show i
     , Show o
     , SetMember Process (Process q0) e
     )
  => Server (Sup i o)
  -> i
  -> Eff e (Maybe o)
lookupChild svr cId = call svr (LookupC cId)

-- | Apply the given function to child process.
--
-- Like 'lookupChild' this will lookup the child and if it does not exist
-- 'Nothing' is returned, and the given function is not applied to anything.
--
-- @since 0.23.0
withChild ::
     forall i e o q0 a.
     ( HasCallStack
     , Member Interrupts e
     , Member Logs e
     , SetMember Process (Process q0) e
     , Typeable o
     , NFData o
     , Show o
     , Typeable e
     , Typeable i
     , NFData i
     , Show i
     )
  => Server (Sup i o)
  -> i
  -> (o -> Eff e a)
  -> Eff e (Maybe a)
withChild svr i f =
  call svr (LookupC i) >>=
  maybe
    (do logError ("no process found for: " <> pack (show i))
        pure Nothing)
    (fmap Just . f)

-- | Return a 'Text' describing the current state of the supervisor.
--
-- @since 0.23.0
getDiagnosticInfo ::
     ( Typeable i
     , Typeable o
     , Show i
     , Show o
     , NFData i
     , NFData o
     , Typeable e
     , HasCallStack
     , Member Interrupts e
     , SetMember Process (Process q0) e
     )
  => Server (Sup i o)
  -> Eff e Text
getDiagnosticInfo supervisor = call supervisor GetDiagnosticInfo