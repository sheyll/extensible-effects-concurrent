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
-- a separate module, since the child-id can be reused when a child
-- exits.
--
-- @since 0.23.0
module Control.Eff.Concurrent.Protocol.Supervisor
  ( Sup()
  , ChildId
  , StartArgument(MkSupConfig)
  , supConfigChildStopTimeout
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
import Control.Eff.Concurrent.Protocol.Wrapper
import Control.Eff.Concurrent.Protocol.StatefulServer as Server
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
import Data.Kind
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Type.Pretty
import GHC.Generics (Generic)
import GHC.Stack
import Control.Applicative ((<|>))

-- * Supervisor Server API
-- ** Types

-- | The index type of 'Server' supervisors.
--
-- A @'Sup' p@ manages the life cycle of the processes, running the @'Server' p@
-- methods of that specific type.
--
-- The supervisor maps an identifier value of type @'ChildId' p@ to an @'Endpoint' p@.
--
-- @since 0.24.0
data Sup (p :: Type) deriving Typeable

instance Typeable p => HasPdu (Sup p) where
  -- | The 'Pdu' instance contains methods to start, stop and lookup a child
  -- process, as well as a diagnostic callback.
  --
  -- @since 0.23.0
  data instance  Pdu (Sup p) r where
          StartC :: ChildId p -> Pdu (Sup p) ('Synchronous (Either (SpawnErr p) (Endpoint (Protocol p))))
          StopC :: ChildId p -> Timeout -> Pdu (Sup p) ('Synchronous Bool)
          LookupC :: ChildId p -> Pdu (Sup p) ('Synchronous (Maybe (Endpoint (Protocol p))))
          GetDiagnosticInfo :: Pdu (Sup p) ('Synchronous Text)
      deriving Typeable

instance (Show (ChildId p)) => Show (Pdu (Sup p) ('Synchronous r)) where
  showsPrec d (StartC c) = showParen (d >= 10) (showString "StartC " . showsPrec 10 c)
  showsPrec d (StopC c t) = showParen (d >= 10) (showString "StopC " . showsPrec 10 c . showChar ' ' . showsPrec 10 t)
  showsPrec d (LookupC c) = showParen (d >= 10) (showString "LookupC " . showsPrec 10 c)
  showsPrec _ GetDiagnosticInfo = showString "GetDiagnosticInfo"

instance (NFData (ChildId p)) => NFData (Pdu (Sup p) ('Synchronous r)) where
  rnf (StartC ci) = rnf ci
  rnf (StopC ci t) = rnf ci `seq` rnf t
  rnf (LookupC ci) = rnf ci
  rnf GetDiagnosticInfo = ()

type instance ToPretty (Sup p) = "supervisor" <:> ToPretty p

-- | The type of value used to index running 'Server' processes managed by a 'Sup'.
--
-- Note, that the type you provide must be 'Tangible'.
--
-- @since 0.24.0
type family ChildId p

-- | Constraints on the parameters to 'Sup'.
--
-- @since 0.24.0
type TangibleSup p =
  ( Tangible (ChildId p)
  , Ord (ChildId p)
  , Typeable p
  )


instance
  ( LogIo q
  , TangibleSup p
  , Tangible (ChildId p)
  , Server p (Processes q)
  ) => Server (Sup p) (Processes q) where

  -- | Options that control the 'Sup p' process.
  --
  -- This contains:
  --
  -- * a 'SpawnFun'
  -- * the 'Timeout' after requesting a normal child exit before brutally killing the child.
  --
  -- @since 0.24.0
  data StartArgument (Sup p) (Processes q) = MkSupConfig
    {
      -- , supConfigChildRestartPolicy :: ChildRestartPolicy
      -- , supConfigResilience :: Resilience
      supConfigChildStopTimeout :: Timeout
    , supConfigStartFun :: ChildId p -> Server.StartArgument p (Processes q)
    }

  type Model (Sup p) = Children (ChildId p) p

  setup _ _cfg = pure (def, ())
  update _ supConfig (OnCall rt req) =
    case req of
      GetDiagnosticInfo ->  do
        p <- (pack . show <$> getChildren @(ChildId p) @p)
        sendReply rt p

      LookupC i -> do
        p <- fmap _childEndpoint <$> lookupChildById @(ChildId p) @p i
        sendReply rt p

      StopC i t -> do
        mExisting <- lookupAndRemoveChildById @(ChildId p) @p i
        case mExisting of
          Nothing -> sendReply rt False
          Just existingChild -> do
            stopOrKillChild i existingChild t
            sendReply rt True

      StartC i -> do
        mExisting <- lookupChildById @(ChildId p) @p i
        case mExisting of
          Nothing -> do
            childEp <- raise (raise (Server.start (supConfigStartFun supConfig i)))
            let childPid = _fromEndpoint childEp
            cMon <- monitor childPid
            putChild i (MkChild @p childEp cMon)
            sendReply rt (Right childEp)
          Just existingChild ->
            sendReply rt (Left (AlreadyStarted i (existingChild ^. childEndpoint)))

  update _ _supConfig (OnDown pd) = do
      oldEntry <- lookupAndRemoveChildByMonitor @(ChildId p) @p (downReference pd)
      case oldEntry of
        Nothing -> logWarning ("unexpected: " <> pack (show pd))
        Just (i, c) -> do
          logInfo (  pack (show pd)
                  <> " for child "
                  <> pack (show i)
                  <> " => "
                  <> pack (show (_childEndpoint c))
                  )
  update _ supConfig (OnInterrupt e) = do
      let (logSev, exitReason) =
            case e of
              NormalExitRequested ->
                (debugSeverity, ExitNormally)
              _ ->
                (warningSeverity, interruptToExit (ErrorInterrupt ("supervisor interrupted: " <> show e)))
      stopAllChildren @p (supConfigChildStopTimeout supConfig)
      logWithSeverity logSev ("supervisor stopping: " <> pack (show e))
      exitBecause exitReason

  update _ _supConfig o = logWarning ("unexpected: " <> pack (show o))


-- | Runtime-Errors occurring when spawning child-processes.
--
-- @since 0.23.0
data SpawnErr p = AlreadyStarted (ChildId p) (Endpoint (Protocol p))
  deriving (Typeable, Generic)

deriving instance Eq (ChildId p) => Eq (SpawnErr p)
deriving instance Ord (ChildId p) => Ord (SpawnErr p)
deriving instance (Typeable (Protocol p), Show (ChildId p)) => Show (SpawnErr p)

instance NFData (ChildId p) => NFData (SpawnErr p)


-- ** Functions
-- | Start and link a new supervisor process with the given 'SpawnFun'unction.
--
-- To spawn new child processes use 'spawnChild'.
--
-- @since 0.23.0
startSupervisor
  :: forall p e
  . ( HasCallStack
    , LogIo (Processes e)
    , TangibleSup p
    , Server (Sup p) (Processes e)
    )
  => StartArgument (Sup p) (Processes e)
  -> Eff (Processes e) (Endpoint (Sup p))
startSupervisor = Server.start

-- | Stop the supervisor and shutdown all processes.
--
-- Block until the supervisor has finished.
--
-- @since 0.23.0
stopSupervisor
  :: ( HasCallStack
     , HasProcesses e q
     , Member Logs e
     , Lifted IO e
     , TangibleSup p
     )
  => Endpoint (Sup p)
  -> Eff e ()
stopSupervisor ep = do
  logInfo ("stopping supervisor: " <> pack (show ep))
  mr <- monitor (_fromEndpoint ep)
  sendInterrupt (_fromEndpoint ep) NormalExitRequested
  r <- receiveSelectedMessage (selectProcessDown mr)
  logInfo ("supervisor stopped: " <> pack (show ep) <> " " <> pack (show r))

-- | Check if a supervisor process is still alive.
--
-- @since 0.23.0
isSupervisorAlive
  :: forall p q0 e .
     ( HasCallStack
     , Member Logs e
     , Typeable p
     , HasProcesses e q0
     )
  => Endpoint (Sup p)
  -> Eff e Bool
isSupervisorAlive x = isProcessAlive (_fromEndpoint x)

-- | Monitor a supervisor process.
--
-- @since 0.23.0
monitorSupervisor
  :: forall p q0 e .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleSup p
     )
  => Endpoint (Sup p)
  -> Eff e MonitorReference
monitorSupervisor x = monitor (_fromEndpoint x)

-- | Start, link and monitor a new child process using the 'SpawnFun' passed
-- to 'startSupervisor'.
--
-- @since 0.23.0
spawnChild
  :: forall p q0 e .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleSup p
     , Typeable (Protocol p)
     )
  => Endpoint (Sup p)
  -> ChildId p
  -> Eff e (Either (SpawnErr p) (Endpoint (Protocol p)))
spawnChild ep cId = call ep (StartC cId)

-- | Lookup the given child-id and return the output value of the 'SpawnFun'
--   if the client process exists.
--
-- @since 0.23.0
lookupChild ::
    forall p e q0 .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleSup p
     , Typeable (Protocol p)
     )
  => Endpoint (Sup p)
  -> ChildId p
  -> Eff e (Maybe (Endpoint (Protocol p)))
lookupChild ep cId = call ep (LookupC @p cId)

-- | Stop a child process, and block until the child has exited.
--
-- Return 'True' if a process with that ID was found, 'False' if no process
-- with the given ID was running.
--
-- @since 0.23.0
stopChild ::
    forall p e q0 .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleSup p
     )
  => Endpoint (Sup p)
  -> ChildId p
  -> Eff e Bool
stopChild ep cId = call ep (StopC @p cId (TimeoutMicros 4000000))

-- | Return a 'Text' describing the current state of the supervisor.
--
-- @since 0.23.0
getDiagnosticInfo
  :: forall p e q0 .
     ( HasCallStack
     , HasProcesses e q0
     , TangibleSup p
     )
  => Endpoint (Sup p)
  -> Eff e Text
getDiagnosticInfo s = call s (GetDiagnosticInfo @p)

-- Internal Functions

stopOrKillChild
  :: forall p e q0 .
     ( HasCallStack
     , HasProcesses e q0
     , Lifted IO e
     , Lifted IO q0
     , Member Logs e
     , Member (State (Children (ChildId p) p)) e
     , TangibleSup p
     , Typeable (Protocol p)
     )
  => ChildId p
  -> Child p
  -> Timeout
  -> Eff e ()
stopOrKillChild cId c stopTimeout =
      do
        sup <- asEndpoint @(Sup p) <$> self
        t <- startTimer stopTimeout
        sendInterrupt (_fromEndpoint (c^.childEndpoint)) NormalExitRequested
        r1 <- receiveSelectedMessage (   Right <$> selectProcessDown (c^.childMonitoring)
                                     <|> Left  <$> selectTimerElapsed t )
        case r1 of
          Left timerElapsed -> do
            logWarning (pack (show timerElapsed) <> ": child "<> pack (show cId) <>" => " <> pack(show (c^.childEndpoint)) <>" did not shutdown in time")
            sendShutdown
              (_fromEndpoint (c^.childEndpoint))
              (interruptToExit
                (TimeoutInterrupt
                  ("child did not shut down in time and was terminated by the "
                    ++ show sup)))
          Right downMsg ->
            logInfo ("child "<> pack (show cId) <>" => " <> pack(show (c^.childEndpoint)) <>" terminated: " <> pack (show (downReason downMsg)))

stopAllChildren
  :: forall p e q0 .
     ( HasCallStack
     , HasProcesses e q0
     , Lifted IO e
     , Lifted IO q0
     , Member Logs e
     , Member (State (Children (ChildId p) p)) e
     , TangibleSup p
     , Typeable (Protocol p)
     )
  => Timeout -> Eff e ()
stopAllChildren stopTimeout = removeAllChildren @(ChildId p) @p >>= pure . Map.assocs >>= traverse_ xxx
  where
    xxx (cId, c) = provideInterrupts (stopOrKillChild cId c stopTimeout) >>= either crash return
      where
        crash e = do
          logError (pack (show e) <> " while stopping child: " <> pack (show cId) <> " " <> pack (show c))
