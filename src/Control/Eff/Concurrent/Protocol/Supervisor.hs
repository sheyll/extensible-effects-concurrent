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
  ( startLink
  , statefulChild
  , stopSupervisor
  , isSupervisorAlive
  , monitorSupervisor
  , getDiagnosticInfo
  , spawnChild
  , spawnOrLookup
  , lookupChild
  , stopChild
  , Sup()
  , Pdu(StartC, StopC, LookupC, GetDiagnosticInfo)
  , ChildId
  , Stateful.StartArgument(MkSupConfig)
  , supConfigChildStopTimeout
  , SpawnErr(AlreadyStarted)
  , ChildEvent(OnChildSpawned, OnChildDown, OnSupervisorShuttingDown)
  ) where

import Control.DeepSeq (NFData(rnf))
import Control.Eff as Eff
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Client
import Control.Eff.Concurrent.Protocol.Wrapper
import Control.Eff.Concurrent.Protocol.Observer
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
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

-- ** Functions

-- | Start and link a new supervisor process with the given 'SpawnFun'unction.
--
-- To spawn new child processes use 'spawnChild'.
--
-- @since 0.23.0
startLink
  :: forall p e
  . ( HasCallStack
    , LogIo (Processes e)
    , TangibleSup p
    , Stateful.Server (Sup p) (Processes e)
    )
  => Stateful.StartArgument (Sup p) (Processes e)
  -> Eff (Processes e) (Endpoint (Sup p))
startLink = Stateful.startLink

-- | A smart constructor for 'MkSupConfig' that makes it easy to start a 'Stateful.Server' instance.
--
-- The user needs to instantiate @'ChildId' p@.
--
-- @since 0.29.3
statefulChild
  :: forall p e
  . ( HasCallStack
    , LogIo (Processes e)
    , TangibleSup (Stateful.Stateful p)
    , Stateful.Server (Sup (Stateful.Stateful p)) (Processes e)
    )
  => Timeout
  -> (ChildId p -> Stateful.StartArgument p (Processes e))
  -> Stateful.StartArgument (Sup (Stateful.Stateful p)) (Processes e)
statefulChild t f = MkSupConfig t (Stateful.Init . f)

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

-- | Start and monitor a new child process using the 'SpawnFun' passed
-- to 'startLink'.
--
-- @since 0.23.0
spawnChild
  :: forall p q0 e .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleSup p
     , Typeable (Effectful.ServerPdu p)
     )
  => Endpoint (Sup p)
  -> ChildId p
  -> Eff e (Either (SpawnErr p) (Endpoint (Effectful.ServerPdu p)))
spawnChild ep cId = call ep (StartC cId)

-- | Start and monitor a new child process using the 'SpawnFun' passed
-- to 'startLink'.
--
-- Call 'spawnChild' and unpack the 'Either' result,
-- ignoring the 'AlreadyStarted' error.
--
-- @since 0.29.2
spawnOrLookup
  :: forall p q0 e .
     ( HasCallStack
     , Member Logs e
     , HasProcesses e q0
     , TangibleSup p
     , Typeable (Effectful.ServerPdu p)
     )
  => Endpoint (Sup p)
  -> ChildId p
  -> Eff e (Endpoint (Effectful.ServerPdu p))
spawnOrLookup supEp cId =
  do res <- spawnChild supEp cId
     pure $
      case res of
        Left (AlreadyStarted _ ep) -> ep
        Right ep -> ep

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
     , Typeable (Effectful.ServerPdu p)
     )
  => Endpoint (Sup p)
  -> ChildId p
  -> Eff e (Maybe (Endpoint (Effectful.ServerPdu p)))
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
  type instance EmbeddedPduList (Sup p) = '[ObserverRegistry (ChildEvent p)]
  -- | The 'Pdu' instance contains methods to start, stop and lookup a child
  -- process, as well as a diagnostic callback.
  --
  -- @since 0.23.0
  data instance  Pdu (Sup p) r where
          StartC :: ChildId p -> Pdu (Sup p) ('Synchronous (Either (SpawnErr p) (Endpoint (Effectful.ServerPdu p))))
          StopC :: ChildId p -> Timeout -> Pdu (Sup p) ('Synchronous Bool)
          LookupC :: ChildId p -> Pdu (Sup p) ('Synchronous (Maybe (Endpoint (Effectful.ServerPdu p))))
          GetDiagnosticInfo :: Pdu (Sup p) ('Synchronous Text)
          ChildEventObserverRegistry :: Pdu (ObserverRegistry (ChildEvent p)) r -> Pdu (Sup p) r
      deriving Typeable

instance (Typeable p, Show (ChildId p)) => Show (Pdu (Sup p) r) where
  showsPrec d (StartC c) = showParen (d >= 10) (showString "StartC " . showsPrec 10 c)
  showsPrec d (StopC c t) = showParen (d >= 10) (showString "StopC " . showsPrec 10 c . showChar ' ' . showsPrec 10 t)
  showsPrec d (LookupC c) = showParen (d >= 10) (showString "LookupC " . showsPrec 10 c)
  showsPrec _ GetDiagnosticInfo = showString "GetDiagnosticInfo"
  showsPrec d (ChildEventObserverRegistry c) = showParen (d >= 10) (showString "ChildEventObserverRegistry " . showsPrec 10 c)

instance (NFData (ChildId p)) => NFData (Pdu (Sup p) r) where
  rnf (StartC ci) = rnf ci
  rnf (StopC ci t) = rnf ci `seq` rnf t
  rnf (LookupC ci) = rnf ci
  rnf GetDiagnosticInfo = ()
  rnf (ChildEventObserverRegistry x) = rnf x

instance Typeable p => HasPduPrism (Sup p) (ObserverRegistry (ChildEvent p)) where
 embedPdu = ChildEventObserverRegistry
 fromPdu (ChildEventObserverRegistry x) = Just x
 fromPdu _ = Nothing

type instance ToPretty (Sup p) = "supervisor" <:> ToPretty p

-- | The event type to indicate that a child was started or stopped.
--
-- The need for this type originated for the watchdog functionality introduced
-- in 0.29.3.
-- The watch dog shall restart a crashed child, and in order to do so, it
-- must somehow monitor the child.
-- Since no order is specified in which processes get the 'ProcessDown' events,
-- a watchdog cannot monitor a child and restart it immediately, because it might
-- have received the process down event before the supervisor.
-- So instead the watchdog can simply use the supervisor events, and monitor only the
-- supervisor process.
--
-- @since 0.29.3
data ChildEvent p where
  OnChildSpawned :: Endpoint (Sup p) -> ChildId p -> Endpoint (Effectful.ServerPdu p) -> ChildEvent p
  OnChildDown :: Endpoint (Sup p) -> ChildId p -> Endpoint (Effectful.ServerPdu p) -> Interrupt 'NoRecovery -> ChildEvent p
  OnSupervisorShuttingDown :: Endpoint (Sup p) -> ChildEvent p -- ^ The supervisor is shutting down and will soon begin stopping/killing its children
  deriving (Typeable, Generic)

instance (NFData (ChildId p)) => NFData (ChildEvent p)

instance (Typeable p, Typeable (Effectful.ServerPdu p), Show (ChildId p)) => Show (ChildEvent p) where
  showsPrec d x =
    case x of
     OnChildSpawned s i e ->
        showParen (d >= 10)
          (shows s . showString ": child-spawned: " . shows i . showChar ' ' . shows e)
     OnChildDown s i e r ->
        showParen (d >= 10)
          (shows s . showString ": child-down: " . shows i . showChar ' ' . shows e . showChar ' ' . showsPrec 10 r)
     OnSupervisorShuttingDown s ->
        shows s . showString ": shutting down"

-- | The type of value used to index running 'Server' processes managed by a 'Sup'.
--
-- Note, that the type you provide must be 'Tangible'.
--
-- @since 0.24.0
type family ChildId p

type instance ChildId (Stateful.Stateful p) = ChildId p

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
  , Typeable (Effectful.ServerPdu p)
  , Effectful.Server p (Processes q)
  , HasProcesses (Effectful.ServerEffects p (Processes q)) q
  ) => Stateful.Server (Sup p) (Processes q) where

  -- | Options that control the 'Sup p' process.
  --
  -- This contains:
  --
  -- * a 'SpawnFun'
  -- * the 'Timeout' after requesting a normal child exit before brutally killing the child.
  --
  -- @since 0.24.0
  data instance StartArgument (Sup p) (Processes q) = MkSupConfig
    {
      -- , supConfigChildRestartPolicy :: ChildRestartPolicy
      -- , supConfigResilience :: Resilience
      supConfigChildStopTimeout :: Timeout
    , supConfigStartFun :: ChildId p -> Effectful.Init p (Processes q)
    }

  data instance Model (Sup p) =
    SupModel { _children :: Children (ChildId p) p
             , _childEventObserver :: ObserverRegistry (ChildEvent p)
             }
      deriving Typeable

  setup _ _cfg = pure (SupModel def emptyObserverRegistry, ())
  update _ _supConfig (Stateful.OnCast req) =
    case  req of
      ChildEventObserverRegistry x ->
        Stateful.zoomModel @(Sup p) childEventObserverLens (observerRegistryHandlePdu x)

  update me supConfig (Stateful.OnCall rt req) =
    case req of
      ChildEventObserverRegistry x ->
        logEmergency ("unreachable: " <> pack (show x))

      GetDiagnosticInfo -> zoomToChildren @p $ do
        p <- (pack . show <$> getChildren @(ChildId p) @p)
        sendReply rt p

      LookupC i -> zoomToChildren @p $ do
        p <- fmap _childEndpoint <$> lookupChildById @(ChildId p) @p i
        sendReply rt p

      StopC i t -> zoomToChildren @p $ do
        mExisting <- lookupAndRemoveChildById @(ChildId p) @p i
        case mExisting of
          Nothing -> sendReply rt False
          Just existingChild -> do
            reason <- stopOrKillChild i existingChild t
            sendReply rt True
            Stateful.zoomModel @(Sup p)
              childEventObserverLens
              (observerRegistryNotify @(ChildEvent p) (OnChildDown me i (existingChild^.childEndpoint) reason))

      StartC i -> do
        mExisting <- zoomToChildren @p $ lookupChildById @(ChildId p) @p i
        case mExisting of
          Nothing -> do
            childEp <- raise (raise (Effectful.startLink (supConfigStartFun supConfig i)))
            let childPid = _fromEndpoint childEp
            cMon <- monitor childPid
            zoomToChildren @p $ putChild i (MkChild @p childEp cMon)
            sendReply rt (Right childEp)
            Stateful.zoomModel @(Sup p)
              childEventObserverLens
              (observerRegistryNotify @(ChildEvent p) (OnChildSpawned me i childEp))
          Just existingChild ->
            sendReply rt (Left (AlreadyStarted i (existingChild ^. childEndpoint)))

  update me _supConfig (Stateful.OnDown pd) = do
      oldEntry <- zoomToChildren @p $ lookupAndRemoveChildByMonitor @(ChildId p) @p (downReference pd)
      case oldEntry of
        Nothing -> logWarning ("unexpected: " <> pack (show pd))
        Just (i, c) -> do
          logInfo (  pack (show pd)
                  <> " for child "
                  <> pack (show i)
                  <> " => "
                  <> pack (show (_childEndpoint c))
                  )
          Stateful.zoomModel @(Sup p)
              childEventObserverLens
              (observerRegistryNotify @(ChildEvent p) (OnChildDown me i (c^.childEndpoint) (downReason pd)))
  update me supConfig (Stateful.OnInterrupt e) =
    case e of
      NormalExitRequested -> do
        logDebug ("supervisor stopping: " <> pack (show e))
        Stateful.zoomModel @(Sup p)
            childEventObserverLens
            (observerRegistryNotify @(ChildEvent p) (OnSupervisorShuttingDown me))
        stopAllChildren @p me (supConfigChildStopTimeout supConfig)
        exitNormally
      LinkedProcessCrashed linked ->
        logNotice (pack (show linked))
      _ -> do
        logWarning ("supervisor interrupted: " <> pack (show e))
        Stateful.zoomModel @(Sup p)
            childEventObserverLens
            (observerRegistryNotify @(ChildEvent p) (OnSupervisorShuttingDown me))
        stopAllChildren @p me (supConfigChildStopTimeout supConfig)
        exitBecause (interruptToExit e)

  update _ _supConfig o = logWarning ("unexpected: " <> pack (show o))


zoomToChildren :: forall p c e
  . Member (Stateful.ModelState (Sup p)) e
  => Eff (State (Children (ChildId p) p) ': e) c
  -> Eff e c
zoomToChildren = Stateful.zoomModel @(Sup p) childrenLens

childrenLens :: Lens' (Stateful.Model (Sup p)) (Children (ChildId p) p)
childrenLens = lens _children (\(SupModel _ o) c -> SupModel c o)

childEventObserverLens :: Lens' (Stateful.Model (Sup p)) (ObserverRegistry (ChildEvent p))
childEventObserverLens = lens _childEventObserver (\(SupModel o _) c -> SupModel o c)

-- | Runtime-Errors occurring when spawning child-processes.
--
-- @since 0.23.0
data SpawnErr p = AlreadyStarted (ChildId p) (Endpoint (Effectful.ServerPdu p))
  deriving (Typeable, Generic)

deriving instance Eq (ChildId p) => Eq (SpawnErr p)
deriving instance Ord (ChildId p) => Ord (SpawnErr p)
deriving instance (Typeable (Effectful.ServerPdu p), Show (ChildId p)) => Show (SpawnErr p)

instance NFData (ChildId p) => NFData (SpawnErr p)

-- Internal Functions

stopOrKillChild
  :: forall p e q0 .
     ( HasCallStack
     , HasProcesses e q0
     , Lifted IO e
     , Lifted IO q0
     , Member Logs e
     , Member (Stateful.ModelState (Sup p)) e
     , TangibleSup p
     , Typeable (Effectful.ServerPdu p)
     )
  => ChildId p
  -> Child p
  -> Timeout
  -> Eff e (Interrupt 'NoRecovery)
stopOrKillChild cId c stopTimeout =
      do
        sup <- asEndpoint @(Sup p) <$> self
        t <- startTimer stopTimeout
        sendInterrupt (_fromEndpoint (c^.childEndpoint)) NormalExitRequested
        r1 <- receiveSelectedMessage (   Right <$> selectProcessDown (c^.childMonitoring)
                                     <|> Left  <$> selectTimerElapsed t )
        demonitor (c^.childMonitoring)
        unlinkProcess (_fromEndpoint (c^.childEndpoint))
        case r1 of
          Left timerElapsed -> do
            logWarning (pack (show timerElapsed) <> ": child "<> pack (show cId) <>" => " <> pack(show (c^.childEndpoint)) <>" did not shutdown in time")
            let reason = interruptToExit (TimeoutInterrupt
                                           ("child did not shut down in time and was terminated by the "
                                             ++ show sup))
            sendShutdown (_fromEndpoint (c^.childEndpoint)) reason
            return reason
          Right downMsg -> do
            logInfo ("child "<> pack (show cId) <>" => " <> pack(show (c^.childEndpoint)) <>" terminated: " <> pack (show (downReason downMsg)))
            return (downReason downMsg)

stopAllChildren
  :: forall p e q0 .
     ( HasCallStack
     , HasProcesses e q0
     , Lifted IO e
     , Lifted IO q0
     , Member Logs e
     , Member (Stateful.ModelState (Sup p)) e
     , TangibleSup p
     , Typeable (Effectful.ServerPdu p)
     )
  => Endpoint (Sup p) -> Timeout -> Eff e ()
stopAllChildren me stopTimeout =
  zoomToChildren @p
    (removeAllChildren @(ChildId p) @p)
  >>= pure . Map.assocs
  >>= traverse_ killAndNotify
  where
    killAndNotify (cId, c) = do
      reason <- provideInterrupts (stopOrKillChild cId c stopTimeout) >>= either crash return
      Stateful.zoomModel @(Sup p)
          childEventObserverLens
          (observerRegistryNotify @(ChildEvent p) (OnChildDown me cId (c^.childEndpoint) reason))

      where
        crash e = do
          logError (pack (show e) <> " while stopping child: " <> pack (show cId) <> " " <> pack (show c))
          return (interruptToExit e)
