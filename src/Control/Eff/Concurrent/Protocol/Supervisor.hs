{-# LANGUAGE UndecidableInstances #-}
-- | A process supervisor spawns and monitors child processes.
--
-- The child processes are mapped to symbolic identifier values: Child-IDs.
--
-- This is the barest, most minimal version of a supervisor. Children
-- can be started, but not restarted.
--
-- Children can efficiently be looked-up by an id-value, and when
-- the supervisor is shutdown, all children will be shutdown these
-- are actually all the features of this supervisor implementation.
--
-- Also, this minimalist supervisor only knows how to spawn a
-- single kind of child process.
--
-- When a supervisor spawns a new child process, it expects the
-- child process to return a 'ProcessId'. The supervisor will
-- 'monitor' the child process.
--
-- This is in stark contrast to how Erlang/OTP handles things;
-- In the OTP Supervisor, the child has to link to the parent.
-- This allows the child spec to be more flexible in that no
-- @pid@ has to be passed from the child start function to the
-- supervisor process, and also, a child may break free from
-- the supervisor by unlinking.
--
-- Now while this seems nice at first, this might actually
-- cause surprising results, since it is usually expected that
-- stopping a supervisor also stops the children, or that a child
-- exit shows up in the logging originating from the former
-- supervisor.
--
-- The approach here is to allow any child to link to the
-- supervisor to realize when the supervisor was violently killed,
-- and otherwise give the child no chance to unlink itself from
-- its supervisor.
--
-- This module is far simpler than the Erlang/OTP counter part,
-- of a @simple_one_for_one@ supervisor.
--
-- The future of this supervisor might not be a-lot more than
-- it currently is. The ability to restart processes might be
-- implemented outside of this supervisor module.
--
-- One way to do that is to implement the restart logic in
-- a seperate module, since the child-id can be reused when a child
-- exits.
--
-- @since 0.23.0
module Control.Eff.Concurrent.Protocol.Supervisor
  ( Sup()
  , SpawnFun
  , ServerArgument(MkSupConfig)
  , supConfigChildStopTimeout
  , supConfigSpawnFun
  , SpawnErr(AlreadyStarted)
  , startSupervisor
  , stopSupervisor
  , isSupervisorAlive
  , monitorSupervisor
  , getDiagnosticInfo
  , spawnChild
  , lookupChild
  , stopChild
  ) where

import Control.DeepSeq (NFData(rnf))
import Control.Eff as Eff
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Client
import Control.Eff.Concurrent.Protocol.Request
import Control.Eff.Concurrent.Protocol.Server
import Control.Eff.Concurrent.Protocol.Supervisor.InternalState
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Extend (raise)
import Control.Eff.Log
import Control.Eff.State.Strict as Eff
import Control.Lens hiding ((.=), use)
import Data.Default
import Data.Dynamic
import Data.Foldable
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Type.Pretty
import GHC.Generics (Generic)
import GHC.Stack
import Control.Applicative ((<|>))

-- * Supervisor Server API
-- ** Types


-- | A function that will initialize the child process.
--
-- The process-id returned from this function is kept private, but
-- it is possible to return '(ProcessId, ProcessId)' for example.
--
-- The function is expected to return a value - usually a tuple of 'Endpoint' values for
-- the different aspects of a process, such as sending API requests, managing
-- observers and receiving other auxiliary API messages.
--
-- @since 0.23.0
type SpawnFun i e o = i -> Eff e (o, ProcessId)

-- | 'Pdu' type for supervisor processes.
--
-- The /supervisor/ process contains a 'SpawnFun' from which it can spawn new child processes.
--
-- The supervisor maps an identifier value of type @childId@ to a 'ProcessId' and a
-- 'spawnResult' type.
--
-- This 'spawnResult' is likely a tuple of 'Endpoint' process ids that allow type-safe interaction with
-- the process.
--
-- Also, this serves as handle or reference for interacting with a supervisor process.
--
-- A value of this type is returned by 'startSupervisor'
--
-- @since 0.23.0
newtype Sup childId spawnResult =
  MkSup (Endpoint (Sup childId spawnResult))
  deriving (Ord, Eq, Typeable, NFData)

-- | Constraints on the parameters to 'Sup'.
--
-- @since 0.24.0
type TangibleSup i o =
  ( Tangible i, Ord i, PrettyTypeShow (ToPretty i)
  , Tangible o, PrettyTypeShow (ToPretty o)
  )

instance TangibleSup i o => Show (Sup i o) where
  showsPrec d (MkSup ep) = showsPrec d ep

-- | The 'Pdu' instance contains methods to start, stop and lookup a child
-- process, as well as a diagnostic callback.
--
-- @since 0.23.0
data instance  Pdu (Sup i o) r where
        StartC :: i -> Pdu (Sup i o) ('Synchronous (Either (SpawnErr i o) o))
        StopC :: i -> Timeout -> Pdu (Sup i o) ('Synchronous Bool)
        LookupC :: i -> Pdu (Sup i o) ('Synchronous (Maybe o))
        GetDiagnosticInfo :: Pdu (Sup i o) ('Synchronous Text)
    deriving Typeable

type instance ToPretty (Sup i o) =
    PutStr "supervisor{" <++> ToPretty i <+> PutStr "=>" <+> ToPretty o <++> PutStr "}"

instance (Show i) => Show (Pdu (Sup i o) ('Synchronous r)) where
  showsPrec d (StartC c) = showParen (d >= 10) (showString "StartC " . showsPrec 10 c)
  showsPrec d (StopC c t) = showParen (d >= 10) (showString "StopC " . showsPrec 10 c . showChar ' ' . showsPrec 10 t)
  showsPrec d (LookupC c) = showParen (d >= 10) (showString "LookupC " . showsPrec 10 c)
  showsPrec _ GetDiagnosticInfo = showString "GetDiagnosticInfo"

instance (NFData i) => NFData (Pdu (Sup i o) ('Synchronous r)) where
  rnf (StartC ci) = rnf ci
  rnf (StopC ci t) = rnf ci `seq` rnf t
  rnf (LookupC ci) = rnf ci
  rnf GetDiagnosticInfo = ()

instance
  ( SetMember Process (Process q0) e
  , Lifted IO q0
  , Lifted IO e
  , Member Interrupts e
  , LogsTo h0 e
  , TangibleSup i o
  ) => Server (Sup i o) e where
  -- | Options that control the 'Sup i o' process.
  --
  -- This contains:
  --
  -- * a 'SpawnFun'
  -- * the 'Timeout' after requesting a normal child exit before brutally killing the child.
  --
  -- @since 0.24.0
  data ServerArgument (Sup i o) e = MkSupConfig
    { supConfigSpawnFun         :: SpawnFun i e o
    , supConfigChildStopTimeout :: Timeout
    }

  type ServerState (Sup i o) = Children i o

  serverInit _cfg = pure (def, ())
  stepServerLoop supConfig (ServerLoopRequest (Call orig req)) =
    case req of
      GetDiagnosticInfo ->  do
        p <- (pack . show <$> getChildren @i @o)
        sendReply orig p

      LookupC i -> do
        p <- fmap _childOutput <$> lookupChildById @i @o i
        sendReply orig p

      StopC i t -> do
        mExisting <- lookupAndRemoveChildById @i @o i
        case mExisting of
          Nothing -> sendReply orig False
          Just existingChild -> do
            stopOrKillChild i existingChild t
            sendReply orig True

      StartC i -> do
        (o, cPid) <- raise (raise (supConfigSpawnFun supConfig i))
        cMon <- monitor cPid
        mExisting <- lookupChildById i
        case mExisting of
          Nothing -> do
            putChild i (MkChild o cPid cMon)
            sendReply orig (Right o)
          Just existingChild ->
            sendReply orig (Left (AlreadyStarted i (existingChild ^. childOutput)))

  stepServerLoop _supConfig (ServerLoopProcessDown (ProcessDown mrChild reason)) = do
      oldEntry <- lookupAndRemoveChildByMonitor @i @o mrChild
      case oldEntry of
        Nothing ->
          logWarning ("unexpected process down: "
                     <> pack (show mrChild)
                     <> " reason: "
                     <> pack (show reason)
                     )
        Just (i, c) -> do
          logInfo (  "process down: "
                  <> pack (show mrChild)
                  <> " reason: "
                  <> pack (show reason)
                  <> " for child "
                  <> pack (show i)
                  <> " => "
                  <> pack (show (_childOutput c))
                  )
  stepServerLoop _supConfig o = logWarning ("unexpected: " <> pack (show o))

  recoverFromInterrupt supConfig e = do
      let (logSev, exitReason) =
            case e of
              NormalExitRequested ->
                (debugSeverity, ExitNormally)
              _ ->
                (warningSeverity, interruptToExit (ErrorInterrupt ("supervisor interrupted: " <> show e)))
      stopAllChildren @i @o (supConfigChildStopTimeout supConfig)
      logWithSeverity logSev ("supervisor stopping: " <> pack (show e))
      exitBecause exitReason

-- | Runtime-Errors occurring when spawning child-processes.
--
-- @since 0.23.0
data SpawnErr i o
  = AlreadyStarted i
                   o
  deriving (Eq, Ord, Show, Typeable, Generic)

instance (NFData i, NFData o) => NFData (SpawnErr i o)


-- ** Functions
-- | Start and link a new supervisor process with the given 'SpawnFun'unction.
--
-- To spawn new child processes use 'spawnChild'.
--
-- @since 0.23.0
startSupervisor
  :: forall i e o h
  . ( HasCallStack
    , LogsTo h (InterruptableProcess e)
    , Lifted IO e
    , TangibleSup i o
    , Server (Sup i o) (InterruptableProcess e)
    )
  => ServerArgument (Sup i o) (InterruptableProcess e)
  -> Eff (InterruptableProcess e) (Sup i o)
startSupervisor supConfig = MkSup <$> spawnProtocolServer supConfig

-- | Stop the supervisor and shutdown all processes.
--
-- Block until the supervisor has finished.
--
-- @since 0.23.0
stopSupervisor
  :: ( HasCallStack
     , Member Interrupts e
     , SetMember Process (Process q0) e
     , Member Logs e
     , Lifted IO e
     , TangibleSup i o
     )
  => Sup i o
  -> Eff e ()
stopSupervisor (MkSup ep) = do
  logInfo ("stopping supervisor: " <> pack (show ep))
  mr <- monitor (_fromEndpoint ep)
  sendInterrupt (_fromEndpoint ep) NormalExitRequested
  r <- receiveSelectedMessage (selectProcessDown mr)
  logInfo ("supervisor stopped: " <> pack (show ep) <> " " <> pack (show r))

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
isSupervisorAlive (MkSup x) = isProcessAlive (_fromEndpoint x)

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
monitorSupervisor (MkSup x) = monitor (_fromEndpoint x)

-- | Start, link and monitor a new child process using the 'SpawnFun' passed
-- to 'startSupervisor'.
--
-- @since 0.23.0
spawnChild ::
     ( HasCallStack
     , Member Interrupts e
     , Member Logs e
     , SetMember Process (Process q0) e
     , TangibleSup i o
     )
  => Sup i o
  -> i
  -> Eff e (Either (SpawnErr i o) o)
spawnChild (MkSup ep) cId = call ep (StartC cId)

-- | Lookup the given child-id and return the output value of the 'SpawnFun'
--   if the client process exists.
--
-- @since 0.23.0
lookupChild ::
     ( HasCallStack
     , Member Interrupts e
     , Member Logs e
     , SetMember Process (Process q0) e
     , TangibleSup i o
     )
  => Sup i o
  -> i
  -> Eff e (Maybe o)
lookupChild (MkSup ep) cId = call ep (LookupC cId)

-- | Stop a child process, and block until the child has exited.
--
-- Return 'True' if a process with that ID was found, 'False' if no process
-- with the given ID was running.
--
-- @since 0.23.0
stopChild ::
     ( HasCallStack
     , Member Interrupts e
     , Member Logs e
     , SetMember Process (Process q0) e
     , TangibleSup i o
     )
  => Sup i o
  -> i
  -> Eff e Bool
stopChild (MkSup ep) cId = call ep (StopC cId (TimeoutMicros 4000000))

-- | Return a 'Text' describing the current state of the supervisor.
--
-- @since 0.23.0
getDiagnosticInfo
  :: ( HasCallStack
     , Member Interrupts e
     , SetMember Process (Process q0) e
     , TangibleSup i o
     )
  => Sup i o
  -> Eff e Text
getDiagnosticInfo (MkSup s) = call s GetDiagnosticInfo

-- Internal Functions

stopOrKillChild
  :: forall i o e q0 .
     ( HasCallStack
     , SetMember Process (Process q0) e
     , Member Interrupts e
     , Lifted IO e
     , Lifted IO q0
     , Member Logs e
     , Member (State (Children i o)) e
     , TangibleSup i o
     )
  => i
  -> Child o
  -> Timeout
  -> Eff e ()
stopOrKillChild cId c stopTimeout =
      do
        sup <- MkSup . asEndpoint @(Sup i o) <$> self
        t <- startTimer stopTimeout
        sendInterrupt (c^.childProcessId) NormalExitRequested
        r1 <- receiveSelectedMessage (   Right <$> selectProcessDown (c^.childMonitoring)
                                     <|> Left  <$> selectTimerElapsed t )
        case r1 of
          Left timerElapsed -> do
            logWarning (pack (show timerElapsed) <> ": child "<> pack (show cId) <>" => " <> pack(show (c^.childOutput)) <>" did not shutdown in time")
            sendShutdown
              (c^.childProcessId)
              (interruptToExit
                (TimeoutInterrupt
                  ("child did not shut down in time and was terminated by the "
                    ++ show sup)))
          Right downMsg ->
            logInfo ("child "<> pack (show cId) <>" => " <> pack(show (c^.childOutput)) <>" terminated: " <> pack (show (downReason downMsg)))

stopAllChildren
  :: forall i o e q0 .
     ( HasCallStack
     , SetMember Process (Process q0) e
     , Lifted IO e
     , Lifted IO q0
     , Member Logs e
     , Member (State (Children i o)) e
     , TangibleSup i o
     )
  => Timeout -> Eff e ()
stopAllChildren stopTimeout = removeAllChildren @i @o >>= pure . Map.assocs >>= traverse_ xxx
  where
    xxx (cId, c) = provideInterrupts (stopOrKillChild cId c stopTimeout) >>= either crash return
      where
        crash e = do
          logError (pack (show e) <> " while stopping child: " <> pack (show cId) <> " " <> pack (show c))
