-- | A process supervisor spawns and monitors child processes.
--
-- The child processes are mapped to symbolic identifier values: Child-IDs.
--
-- Opposed to /'ProcessId's/ these child IDs remain consistent between child
-- restarts.
--
-- A supervisor only knows how to spawn a single kind of child process.
--
-- A supervisor is stopped and will exit through 'stopSupervisor'.
-- When a supervisor exists, all child processes are exited, too.
--
-- When a supervisor spawns a new child process, it expects the child process
-- to return a 'ProcessId'.
-- The supervisor will 'monitor' the child process, and react when the child exits.
--
-- This is in stark contrast to how Erlang/OTP handles this; In the OTP Supervisor, the child
-- has to link to the parent. This allows the child spec to be more flexible in that no @pid@ has
-- to be passed from the child start function to the supervisor process, and also, a child may
-- break free from the supervisor by unlinking.
--
-- Now while this seems nice at first, this might actually cause surprising results, since
-- it is usually expected that stopping a supervisor also stops the children, or that a child exit
-- shows up in the logging originating from the former supervisor.
--
-- The approach here is to allow any child to link to the supervisor to realize when the supervisor was
-- violently killed, and otherwise give the child no chance to unlink itself from its supervisor.
--
-- This module is far simpler than the Erlang/OTP counter part, of a @simple_one_for_one@
-- supervisor.
--
-- @since 0.23.0
module Control.Eff.Concurrent.Api.Supervisor
  ( Sup()
  , SpawnFun
  , SpawnErr
  , startSupervisor
  , stopSupervisor
  , isSupervisorAlive
  , monitorSupervisor
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
import Control.Eff.Concurrent.Process.Timer
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
import Control.Applicative ((<|>))

-- * Supervisor Server API
-- ** Types

-- | 'Api' type for supervisor processes.
--
-- The /supervisor/ process contains a 'SpawnFun' from which it can spawn new child processes.
--
-- The supervisor maps an identifier value of type @childId@ to a 'ProcessId' and a
-- 'spawnResult' type.
--
-- This 'spawnResult' is likely a tuple of 'Server' process ids that allow type-safe interaction with
-- the process.
--
-- Also, this serves as handle or reference for interacting with a supervisor process.
--
-- A value of this type is returned by 'startSupervisor'
--
-- @since 0.23.0
newtype Sup childId spawnResult =
  MkSup (Server (Sup childId spawnResult))
  deriving (Show, Typeable, NFData)

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
        StartC :: i -> Api (Sup i o) ('Synchronous (Either (SpawnErr i o) o))
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
startSupervisor ::
     forall i e o . (HasCallStack, Member Logs e, Ord i, NFData i, NFData o, Typeable i, Typeable o, Show i, Show o)
  => SpawnFun i (InterruptableProcess e) o
  -> Eff (InterruptableProcess e) (Sup i o)
startSupervisor childSpawner = do
  (sup, _supPid) <- spawnApiServerStateful initChildren (onRequest ^: onMessage) onInterrupt
  return (MkSup sup)
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
        onCall (LookupC i) k = k ((, AwaitNext) . Just . fmap _childOutput <$> lookupChildById @i @o i)
        onCall (StartC i) k =
          k $ do
            (o, cPid) <- raise (childSpawner i)
            cMon <- monitor cPid
            putChild i (MkChild o cPid cMon)
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
                  (warningSeverity, ExitUnhandledInterrupt (ErrorInterrupt ("supervisor interrupted: " <> show e)))
        logWithSeverity logSev ("supervisor stopping: " <> pack (show e))

        pure (StopServer exitReason)

-- | Stop the supervisor and shutdown all processes.
--
-- Block until the supervisor has finished.
--
-- @since 0.23.0
stopSupervisor
  :: ( HasCallStack
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
  => Sup i o
  -> Eff e ()
stopSupervisor (MkSup svr) = do
  logInfo ("stopping supervisor: " <> pack (show svr))
  mr <- monitor (_fromServer svr)
  sendInterrupt (_fromServer svr) NormalExitRequested
  r <- receiveSelectedMessage (selectProcessDown mr)
  logInfo ("supervisor stopped: " <> pack (show svr) <> " " <> pack (show r))

-- | Check if a supervisor process is still alive.
--
-- @since 0.23.0
isSupervisorAlive
  :: ( HasCallStack
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
  => Sup i o
  -> Eff e Bool
isSupervisorAlive (MkSup x) = isProcessAlive (_fromServer x)

-- | Monitor a supervisor process.
--
-- @since 0.23.0
monitorSupervisor
  :: ( HasCallStack
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
  => Sup i o
  -> Eff e MonitorReference
monitorSupervisor (MkSup x) = monitor (_fromServer x)

-- | Start, link and monitor a new child process using the 'SpawnFun' passed
-- to 'startSupervisor'.
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
  => Sup i o
  -> i
  -> Eff e (Either (SpawnErr i o) o)
spawnChild (MkSup svr) cId = call svr (StartC cId)

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
  => Sup i o
  -> i
  -> Eff e (Maybe o)
lookupChild (MkSup svr) cId = call svr (LookupC cId)

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
  => Sup i o
  -> i
  -> (o -> Eff e a)
  -> Eff e (Maybe a)
withChild (MkSup svr) i f =
  call svr (LookupC i) >>=
  maybe
    (do logError ("no process found for: " <> pack (show i))
        pure Nothing)
    (fmap Just . f)

-- | Return a 'Text' describing the current state of the supervisor.
--
-- @since 0.23.0
getDiagnosticInfo
  :: ( Typeable i
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
  => Sup i o
  -> Eff e Text
getDiagnosticInfo (MkSup s) = call s GetDiagnosticInfo

-- Internal Functions

stopChild
  :: forall i o e q0 .
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
  => i
  -> Timeout
  -> Eff (State (Children i o) ': e) ()
stopChild cId timeout = traverse_ go . lookupAndRemove @i @o cId
  where
    go :: Child o -> Eff (State (Children i o) ': e) ()
    go c = do
              t <- startTimer timeout
              sendInterrupt (c^.childProcessId) NormalExitRequested
              r1 <- receiveSelectedMessage (   Right <$> selectProcessDown (c^.childMonitoring)
                                           <|> Left  <$> selectTimerElapsed t )
              case r1 of
                Left timerElapsed ->
