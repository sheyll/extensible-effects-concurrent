{-# LANGUAGE UndecidableInstances #-}
-- | Monitor a process and act when it is unresponsive.
--
-- Behaviour of the watchdog:
--
-- When a child crashes:
-- * if the allowed maximum number crashes per time span has been reached for the process,
-- ** cancel all other timers
-- ** don't start the child again
-- ** if this is a /permanent/ watchdog crash the watchdog
-- * otherwise
-- ** tell the broker to start the child
-- ** record a crash and start a timer to remove the record later
-- ** monitor the child
--
-- When a child crash timer elapses:
-- * remove the crash record
--
-- @since 0.30.0
module Control.Eff.Concurrent.Protocol.Watchdog
  ( startLink
  , Watchdog
  , attachTemporary
  , attachPermanent
  , getCrashReports
  , CrashRate(..)
  , crashCount
  , crashTimeSpan
  , crashesPerSeconds
  , CrashCount
  , CrashTimeSpan
  , ChildWatch(..)
  , parent
  , crashes
  , ExonerationTimer(..)
  , CrashReport(..)
  , crashTime
  , crashReason
  , exonerationTimerReference
  ) where

import Control.DeepSeq
import Control.Eff (Eff, Member, lift, Lifted)
import Control.Eff.Concurrent.Misc
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Client
import Control.Eff.Concurrent.Protocol.Wrapper
import qualified Control.Eff.Concurrent.Protocol.Observer as Observer
import Control.Eff.Concurrent.Protocol.Observer (Observer)
import qualified Control.Eff.Concurrent.Protocol.Broker as Broker
import Control.Eff.Concurrent.Protocol.Broker (Broker)
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as Stateful
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as Effectful
import Control.Lens
import Control.Eff.Log
import Data.Proxy
import Data.Set (Set)
import Data.Typeable
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Kind (Type)
import Data.Default
import Data.Text (pack)
import GHC.Stack (HasCallStack)
import Data.Maybe (isJust)
import Data.Foldable (traverse_, forM_)
import Control.Monad (when)


-- | The phantom for watchdog processes, that watch the given type of servers
--
-- This type is used for the 'Effectful.Server' and 'HasPdu' instances.
--
-- @since 0.30.0
data Watchdog (child :: Type) deriving Typeable

instance ToTypeLogMsg child => ToTypeLogMsg (Watchdog child) where
  toTypeLogMsg _ = toTypeLogMsg (Proxy @child) <> packLogMsg "_watchdog"

-- | Start and link a new watchdog process.
--
-- The watchdog process will register itself to the 'Broker.ChildEvent's and
-- restart crashed children.
--
-- @since 0.30.0
startLink
  :: forall child e q
  . ( HasCallStack
    , Typeable child
    , FilteredLogging (Processes q)
    , Member Logs q
    , HasProcesses e q
    , ToTypeLogMsg child
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , ToLogMsg (Broker.ChildId child)
    , HasPdu (Effectful.ServerPdu child)
    , ToTypeLogMsg (Effectful.ServerPdu child)
    , Lifted IO q
    )
  => CrashRate -> Eff e (Endpoint (Watchdog child))
startLink = Stateful.startLink @(Watchdog child) . StartWatchDog

-- | Restart children of the given broker.
--
-- When the broker exits, ignore the children of that broker.
--
-- @since 0.30.0
attachTemporary
  :: forall child q e
  . ( HasCallStack
    , FilteredLogging e
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , ToTypeLogMsg child
    , ToTypeLogMsg (Effectful.ServerPdu child)
    , ToLogMsg (Broker.ChildId child)
    , HasProcesses e q
    )
  => Endpoint (Watchdog child) -> Endpoint (Broker child) -> Eff e ()
attachTemporary wd broker =
  callWithTimeout wd (Attach broker False) (TimeoutMicros 1_000_000)

-- | Restart children of the given broker.
--
-- When the broker exits, the watchdog process will exit, too.
--
-- @since 0.30.0
attachPermanent
  :: forall child q e
  . ( HasCallStack
    , FilteredLogging e
    , Typeable child
    , HasPdu (Effectful.ServerPdu child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , ToTypeLogMsg child
    , ToTypeLogMsg (Effectful.ServerPdu child)
    , ToLogMsg (Broker.ChildId child)
    , HasProcesses e q
    )
  => Endpoint (Watchdog child) -> Endpoint (Broker child) -> Eff e ()
attachPermanent wd broker =
  callWithTimeout wd (Attach broker True) (TimeoutMicros 1_000_000)

-- | Return a list of 'CrashReport's.
--
-- Useful for diagnostics
--
-- @since 0.30.0
getCrashReports
  :: forall child q e
  . ( HasCallStack
    , FilteredLogging e
    , Typeable child
    , ToTypeLogMsg child
    , ToTypeLogMsg (Effectful.ServerPdu child)
    , HasPdu (Effectful.ServerPdu child)
    , ToLogMsg (Broker.ChildId child)
    , Tangible (Broker.ChildId child)
    , Ord (Broker.ChildId child)
    , HasProcesses e q
    , Lifted IO q
    , Lifted IO e
    , Member Logs e
    )
  => Endpoint (Watchdog child) -> Eff e (Map (Broker.ChildId child) (ChildWatch child))
getCrashReports wd = callWithTimeout wd GetCrashReports (TimeoutMicros 5_000_000)

instance Typeable child => HasPdu (Watchdog child) where
  type instance EmbeddedPduList (Watchdog child) = '[Observer (Broker.ChildEvent child)]
  data Pdu (Watchdog child) r where
    Attach :: Endpoint (Broker child) -> Bool -> Pdu (Watchdog child) ('Synchronous ())
    GetCrashReports :: Pdu (Watchdog child) ('Synchronous (Map (Broker.ChildId child) (ChildWatch child)))
    OnChildEvent :: Broker.ChildEvent child -> Pdu (Watchdog child) 'Asynchronous
      deriving Typeable

instance Typeable child => HasPduPrism (Watchdog child) (Observer (Broker.ChildEvent child)) where
  embedPdu (Observer.Observed e) = OnChildEvent e
  fromPdu (OnChildEvent x) = Just (Observer.Observed x)
  fromPdu _ = Nothing

instance (NFData (Broker.ChildId child)) => NFData (Pdu (Watchdog child) r) where
  rnf (Attach e b) = rnf e `seq` rnf b `seq` ()
  rnf GetCrashReports = ()
  rnf (OnChildEvent o) = rnf o

instance
  ( Show (Broker.ChildId child)
  , Typeable child
  , Typeable (Effectful.ServerPdu child)
  )
  => Show (Pdu (Watchdog child) r) where
  showsPrec d (Attach e False) = showParen (d>=10) (showString "attach-temporary: " . shows e)
  showsPrec d (Attach e True) = showParen (d>=10) (showString "attach-permanent: " . shows e)
  showsPrec _ GetCrashReports = showString "get-crash-reports"
  showsPrec d (OnChildEvent o) = showParen (d>=10) (showString "on-child-event: " . showsPrec 10 o)

instance (ToTypeLogMsg child) => ToTypeLogMsg (Pdu (Watchdog child) r) where
  toTypeLogMsg _ = toTypeLogMsg (Proxy @(Watchdog child)) <> packLogMsg "_pdu"

instance (ToLogMsg (Broker.ChildId child), ToTypeLogMsg child, ToTypeLogMsg (Effectful.ServerPdu child)) => ToLogMsg (Pdu (Watchdog child) r) where
  toLogMsg (Attach e isPermanent) =
       toTypeLogMsg (Proxy @(Watchdog child))
    <> packLogMsg " attach-"
    <> packLogMsg (if isPermanent then  "permanent" else "temporary")
    <> packLogMsg ": "
    <> toLogMsg e
  toLogMsg GetCrashReports =
       toTypeLogMsg (Proxy @(Watchdog child))
    <> packLogMsg " get-crash-reports"
  toLogMsg (OnChildEvent o) =
       toTypeLogMsg (Proxy @(Watchdog child))
    <> packLogMsg " on-child-event: "
    <> toLogMsg o

-- ------------------ Broker Watches

data BrokerWatch =
  MkBrokerWatch { _brokerMonitor ::  MonitorReference, _isPermanent :: Bool }
  deriving (Typeable)

instance Show BrokerWatch where
  showsPrec d (MkBrokerWatch mon False) = showParen (d>=10) (showString "temporary-broker: " . showsPrec 10 mon)
  showsPrec d (MkBrokerWatch mon True) = showParen (d>=10) (showString "permanent-broker: " . showsPrec 10 mon)

instance ToLogMsg BrokerWatch where
  toLogMsg (MkBrokerWatch mon isPermanent) =
       packLogMsg (if isPermanent then "permanent" else "temporary")
    <> packLogMsg "-child: "
    <> toLogMsg mon

brokerMonitor :: Lens' BrokerWatch MonitorReference
brokerMonitor = lens _brokerMonitor (\(MkBrokerWatch _ x) m -> MkBrokerWatch m x)

isPermanent :: Lens' BrokerWatch Bool
isPermanent = lens _isPermanent (\(MkBrokerWatch x _) m -> MkBrokerWatch x m)

-- --- Server Definition

instance ToTypeLogMsg child => ToLogMsg (Stateful.StartArgument (Watchdog child)) where
  toLogMsg cfg =
       toTypeLogMsg (Proxy @(Watchdog child))
    <> packLogMsg " configuration: "
    <> toLogMsg (_crashRate cfg)

instance
  ( Typeable child
  , ToTypeLogMsg child
  , HasPdu (Effectful.ServerPdu child)
  , ToTypeLogMsg (Effectful.ServerPdu child)
  , Tangible (Broker.ChildId child)
  , Ord (Broker.ChildId child)
  , Eq (Broker.ChildId child)
  , ToLogMsg (Broker.ChildId child)
  , Lifted IO e
  , Member Logs e
  ) => Stateful.Server (Watchdog child) (Processes e) where

  data instance StartArgument (Watchdog child) =
    StartWatchDog { _crashRate :: CrashRate
                  }
      deriving Typeable

  data instance Model (Watchdog child) =
    WatchdogModel { _brokers :: Map (Endpoint (Broker child)) BrokerWatch
                  , _watched :: Map (Broker.ChildId child) (ChildWatch child)
                  }

  update me startArg =
    \case
      Effectful.OnCall rt evt@(Attach broker permanent) -> do
        logDebug evt
        oldMonitor <- Stateful.preuseModel @(Watchdog child) (brokers . at broker . _Just . brokerMonitor)
        newMonitor <- maybe (monitor (broker^.fromEndpoint)) return oldMonitor
        case oldMonitor of
          Nothing -> do
            logDebug "start observing: " broker
            Observer.registerObserver @(Broker.ChildEvent child) broker me
          Just _ ->
            logDebug "already observing " broker
        let newBrokerWatch = MkBrokerWatch newMonitor permanent
        Stateful.modifyModel (brokers . at broker ?~ newBrokerWatch)
        Observer.registerObserver @(Broker.ChildEvent child) broker me
        sendReply rt ()

      Effectful.OnCall rt GetCrashReports ->
        Stateful.useModel @(Watchdog child) watched >>= sendReply rt

      Effectful.OnCast (OnChildEvent e) ->
        case e of
          down@(Broker.OnBrokerShuttingDown broker) -> do
            logInfo "received: " down
            removeBroker me broker

          spawned@(Broker.OnChildSpawned broker _ _) -> do
            logInfo "received: " spawned
            currentModel <- Stateful.getModel @(Watchdog child)
            when (not (Set.member broker (currentModel ^. brokers . to Map.keysSet)))
             (logWarning "received child event for unknown broker: " spawned)

          down@(Broker.OnChildDown broker cId _ ExitNormally) -> do
            logInfo "received: " down
            currentModel <- Stateful.getModel @(Watchdog child)
            if not (Set.member broker (currentModel ^. brokers . to Map.keysSet))
             then logWarning "received child event for unknown broker: " down
             else removeAndCleanChild @child cId

          down@(Broker.OnChildDown broker cId _ reason) -> do
            logInfo "received: " down
            currentModel <- Stateful.getModel @(Watchdog child)
            if not (Set.member broker (currentModel ^. brokers . to Map.keysSet))
             then
              logWarning "received child event for unknown broker: " down
             else do
              let recentCrashes = countRecentCrashes broker cId currentModel
                  rate = startArg ^. crashRate
                  maxCrashCount = rate ^. crashCount
              if recentCrashes < maxCrashCount then do
                logNotice "restarting (" (show recentCrashes) "/" (show maxCrashCount) "): " cId " of " broker
                res <- Broker.spawnChild broker cId
                logNotice "restarted: " cId " of " broker  ": " res
                crash <- startExonerationTimer @child cId reason (rate ^. crashTimeSpan)
                if isJust (currentModel ^? childWatchesById cId)
                  then do
                    logDebug "recording crash for child: " cId " of " broker
                    Stateful.modifyModel (watched @child . at cId . _Just . crashes %~ Set.insert crash)
                  else do
                    logDebug "recording crash for new child: " cId " of " broker
                    Stateful.modifyModel (watched @child . at cId .~ Just (MkChildWatch broker (Set.singleton crash)))
              else do
                logWarning
                  "restart rate exceeded: " rate
                  ", for child: " cId
                  " of " broker
                removeAndCleanChild @child cId
                forM_ (currentModel ^? brokers . at broker . _Just) $ \bw ->
                  if  bw ^. isPermanent then do
                    logError "a child of a permanent broker crashed too often, interrupting: " broker
                    let r =  ExitUnhandledError (pack "restart frequency exceeded")
                    demonitor (bw ^. brokerMonitor)
                    sendShutdown (broker ^. fromEndpoint) r
                    exitBecause r
                    -- TODO shutdown all other permanent brokers!
                  else
                    logError "a child of a temporary broker crashed too often: " broker

      Effectful.OnDown pd@(ProcessDown _mref _ pid) -> do
        logDebug "on-down: " pd
        let broker = asEndpoint pid
        removeBroker @child me broker

      Effectful.OnTimeOut t -> do
        logError "on-timeout: " t

      Effectful.OnMessage (fromStrictDynamic -> Just (MkExonerationTimer cId ref :: ExonerationTimer (Broker.ChildId child))) -> do
        logInfo "on-exoneration-timeout: " cId
        Stateful.modifyModel
          (watched @child . at cId . _Just . crashes %~ Set.filter (\c -> c^.exonerationTimerReference /= ref))

      Effectful.OnMessage t -> do
        logError "on-message: " t

      Effectful.OnInterrupt reason -> do
        logError "on-interrupt: " reason

-- ------------------ Start Argument

crashRate :: Lens' (Stateful.StartArgument (Watchdog child)) CrashRate
crashRate = lens _crashRate (\m x -> m {_crashRate = x})

-- ----------------- Crash Rate

-- | The limit of crashes (see 'CrashCount') per time span (see 'CrashTimeSpan') that justifies restarting
-- child processes.
--
-- Used as parameter for 'startLink'.
--
-- Use 'crashesPerSeconds' to construct a value.
--
-- This governs how long the 'ExonerationTimer' runs before cleaning up a 'CrashReport' in a 'ChildWatch'.
--
-- @since 0.30.0
data CrashRate =
  CrashesPerSeconds { _crashCount :: CrashCount
                    , _crashTimeSpan :: CrashTimeSpan
                    }
  deriving (Typeable, Eq, Ord)

instance ToLogMsg CrashRate where
  toLogMsg = packLogMsg . show

-- | The default is three crashes in 30 seconds.
--
-- @since 0.30.0
instance Default CrashRate where
  def = 3 `crashesPerSeconds` 30

instance Show CrashRate where
  showsPrec d (CrashesPerSeconds count time) =
    showParen (d>=7) (shows count . showString " crashes/" . shows time . showString " seconds")

instance NFData CrashRate where
  rnf (CrashesPerSeconds c t) = c `seq` t `seq` ()

-- | Number of crashes in 'CrashRate'.
--
-- @since 0.30.0
type CrashCount = Int

-- | Time span in which crashes are counted in 'CrashRate'.
--
-- @since 0.30.0
type CrashTimeSpan = Int

-- | A smart constructor for 'CrashRate'.
--
-- The first parameter is the number of crashes allowed per number of seconds (second parameter)
-- until the watchdog should give up restarting a child.
--
-- @since 0.30.0
crashesPerSeconds :: CrashCount -> CrashTimeSpan -> CrashRate
crashesPerSeconds = CrashesPerSeconds

-- | A lens for '_crashCount'.
--
-- @since 0.30.0
crashCount :: Lens' CrashRate CrashCount
crashCount = lens _crashCount (\(CrashesPerSeconds _ time) count -> CrashesPerSeconds count time )

-- | A lens for '_crashTimeSpan'.
--
-- @since 0.30.0
crashTimeSpan :: Lens' CrashRate CrashTimeSpan
crashTimeSpan = lens _crashTimeSpan (\(CrashesPerSeconds count _) time -> CrashesPerSeconds count time)

-- ------------------ Crash

-- | An internal data structure that records a single crash of a child of an attached 'Broker'.
--
-- See 'attachPermanent' and 'attachTemporary'.
--
-- @since 0.30.0
data CrashReport =
  MkCrashReport { _exonerationTimerReference :: TimerReference
                  -- ^ After a crash, an 'ExonerationTimer' according to the 'CrashRate' of the 'Watchdog'
                  -- is started, this is the reference
                , _crashTime :: UTCTime
                  -- ^ Recorded time of the crash
                , _crashReason :: Interrupt 'NoRecovery
                  -- ^ Recorded crash reason
                }
  deriving (Eq, Ord, Typeable)

instance ToLogMsg CrashReport where
  toLogMsg = packLogMsg . show

instance Show CrashReport where
  showsPrec d c =
    showParen (d>=10)
      ( showString "crash report: "
      . showString " time: " . showsPrec 10 (c^.crashTime)
      . showString " reason: " . showsPrec 10 (c^.crashReason)
      . showString " " . showsPrec 10 (c^.exonerationTimerReference)
      )

instance NFData CrashReport where
  rnf (MkCrashReport !a !b !c) = rnf a `seq` rnf b `seq` rnf c `seq` ()

-- | Lens for '_crashTime'
--
-- @since 0.30.0
crashTime :: Lens' CrashReport UTCTime
crashTime = lens _crashTime (\c t -> c { _crashTime = t})

-- | Lens for '_crashReason'
--
-- @since 0.30.0
crashReason :: Lens' CrashReport (Interrupt 'NoRecovery)
crashReason = lens _crashReason (\c t -> c { _crashReason = t})

-- | Lens for '_exonerationTimerReference'
--
-- @since 0.30.0
exonerationTimerReference :: Lens' CrashReport TimerReference
exonerationTimerReference = lens _exonerationTimerReference (\c t -> c { _exonerationTimerReference = t})

startExonerationTimer :: forall child a q e .
     (HasProcesses e q, Lifted IO q, Lifted IO e, Show a, NFData a, Typeable a, Typeable child)
     => a -> Interrupt 'NoRecovery -> CrashTimeSpan -> Eff e CrashReport
startExonerationTimer cId r t = do
  let title = MkProcessTitle (pack "ExonerationTimer<" <> pack (showSTypeable @child ">") <> pack (show cId))
  me <- self
  ref <- sendAfterWithTitle title me (TimeoutMicros (t * 1_000_000)) (MkExonerationTimer cId)
  now <- lift getCurrentTime
  return (MkCrashReport ref now r)

-- | The timer started based on the 'CrashRate' '_crashTimeSpan' when a 'CrashReport' is recorded.
--
-- After this timer elapses, the 'Watchdog' server will remove the 'CrashReport' from the 'ChildWatch' of
-- that child.
--
-- @since 0.30.0
data ExonerationTimer a =  MkExonerationTimer !a !TimerReference
  deriving (Eq, Ord, Typeable)

instance NFData a => NFData (ExonerationTimer a) where
  rnf (MkExonerationTimer !x !r) = rnf r `seq` rnf x `seq` ()

instance Show a => Show (ExonerationTimer a) where
  showsPrec d (MkExonerationTimer x r) =
    showParen (d >= 10)
      ( showString "exonerate: " . showsPrec 10 x
      . showString " after: " .  showsPrec 10 r
      )

-- --------------------------- Child Watches

-- | An internal data structure that keeps the 'CrashReport's of a child of an attached 'Broker' monitored by a 'Watchdog'.
--
-- See 'attachPermanent' and 'attachTemporary', 'ExonerationTimer', 'CrashRate'.
--
-- @since 0.30.0
data ChildWatch child =
  MkChildWatch
    { _parent :: Endpoint (Broker child)
      -- ^ The attached 'Broker' that started the child
    , _crashes :: Set CrashReport
      -- ^ The crashes of the child. If the number of crashes
      -- surpasses the allowed number of crashes before the
      -- 'ExonerationTimer's clean them, the child is finally crashed.
    }
   deriving Typeable

instance NFData (ChildWatch child) where
  rnf (MkChildWatch p c) =
    rnf p `seq` rnf c `seq` ()

instance (Typeable child, Typeable (Broker.ChildId child), Show (Broker.ChildId child)) => Show (ChildWatch child) where
  showsPrec d (MkChildWatch p c) =
    showParen (d>=10) ( showString "child-watch: parent: "
                      . showsPrec 10 p
                      . showString " crashes: "
                      . foldr (.) id (showsPrec 10 <$> Set.toList c)
                      )

instance (ToTypeLogMsg child, ToLogMsg (Broker.ChildId child)) => ToLogMsg (ChildWatch child) where
  toLogMsg (MkChildWatch p c) =
    packLogMsg "parent: " <> toLogMsg p <>
      case Set.toList c of
        [] -> packLogMsg " recorded no crashes"
        (r:rs) -> packLogMsg " recorded crashes: " <> toLogMsg r <> foldr ((<>) . (packLogMsg " " <>) . toLogMsg) (packLogMsg "") rs

-- | A lens for '_parent'.
--
-- @since 0.30.0
parent :: Lens' (ChildWatch child) (Endpoint (Broker child))
parent = lens _parent (\m x -> m {_parent = x})

-- | A lens for '_crashes'
--
-- @since 0.30.0
crashes :: Lens' (ChildWatch child) (Set CrashReport)
crashes = lens _crashes (\m x -> m {_crashes = x})

-- ------------------ Model

instance Default (Stateful.Model (Watchdog child)) where
  def = WatchdogModel def Map.empty

instance ( Show (Broker.ChildId child)
         , Typeable (Broker.ChildId child)
         , Typeable child
         )
          => Show (Stateful.Model (Watchdog child))
  where
  showsPrec d  (WatchdogModel bs cs) =
    showParen (d>=10)
      (showString "watchdog model broker watches: "
      . showsPrec 10 bs
      . showString " watchdog model child watches: "
      . showsPrec 10 cs
      )

-- -------------------------- Model -> Child Watches

watched :: Lens' (Stateful.Model (Watchdog child)) (Map (Broker.ChildId child) (ChildWatch child))
watched = lens _watched (\m x -> m {_watched = x})

childWatches :: IndexedTraversal' (Broker.ChildId child) (Stateful.Model (Watchdog child)) (ChildWatch child)
childWatches = watched . itraversed

childWatchesById ::
     Eq (Broker.ChildId child)
  => Broker.ChildId child
  -> Traversal' (Stateful.Model (Watchdog child)) (ChildWatch child)
childWatchesById theCId = childWatches . ifiltered (\cId _ -> cId == theCId)

childWatchesByParenAndId ::
     Eq (Broker.ChildId child)
  => Endpoint (Broker child)
  -> Broker.ChildId child
  -> Traversal' (Stateful.Model (Watchdog child)) (ChildWatch child)
childWatchesByParenAndId theParent theCId =
  childWatches . ifiltered (\cId cw -> cw ^. parent == theParent && cId == theCId)

countRecentCrashes ::
     Eq (Broker.ChildId child)
  => Endpoint (Broker child)
  -> Broker.ChildId child
  -> Stateful.Model (Watchdog child)
  -> CrashCount
countRecentCrashes theParent theCId theModel =
 length (theModel ^.. childWatchesByParenAndId theParent theCId . crashes . folded)

-- --------------------------- Model -> Broker Watches

brokers :: Lens' (Stateful.Model (Watchdog child)) (Map (Endpoint (Broker child)) BrokerWatch)
brokers = lens _brokers (\m x -> m {_brokers = x})

-- -------------------------- Server Implementation Helpers

removeAndCleanChild ::
  forall child q e.
  ( HasProcesses e q
  , Typeable child
  , ToTypeLogMsg child
  , Typeable (Broker.ChildId child)
  , Ord (Broker.ChildId child)
  , Show (Broker.ChildId child)
  , ToLogMsg (Broker.ChildId child)
  , Member (Stateful.ModelState (Watchdog child)) e
  , Member Logs e
  )
  => Broker.ChildId child
  -> Eff e ()
removeAndCleanChild cId = do
    oldModel <- Stateful.modifyAndGetModel (watched @child . at cId .~ Nothing)
    forMOf_ (childWatchesById cId) oldModel $ \w -> do
      logDebug "removing client entry: " cId
      forMOf_ (crashes . folded . exonerationTimerReference) w cancelTimer
      logDebug w

removeBroker ::
  forall child q e.
  ( HasProcesses e q
  , Typeable child
  , Tangible (Broker.ChildId child)
  , Typeable (Effectful.ServerPdu child)
  , Ord (Broker.ChildId child)
  , Show (Broker.ChildId child)
  , ToLogMsg (Broker.ChildId child)
  , ToTypeLogMsg child
  , ToTypeLogMsg (Effectful.ServerPdu child)
  , Member (Stateful.ModelState (Watchdog child)) e
  , Member Logs e
  )
  => Endpoint (Watchdog child)
  -> Endpoint (Broker child)
  -> Eff e ()
removeBroker me broker = do
    oldModel <- Stateful.getAndModifyModel @(Watchdog child)
                  ( (brokers . at broker .~ Nothing)
                  . (watched %~ Map.filter (\cw -> cw^.parent /= broker))
                  )
    forM_ (oldModel ^? brokers . at broker . _Just) $ \deadBroker ->  do
      logNotice "dettaching: " deadBroker " " broker
      let forgottenChildren = oldModel ^.. watched . itraversed . filtered (\cw -> cw^.parent == broker)
      traverse_ (logNotice "forgetting: ") forgottenChildren
      Observer.forgetObserver @(Broker.ChildEvent child) broker me
      when (view isPermanent deadBroker) $ do
        logError "permanent broker exited: " broker
        exitBecause (ExitOtherProcessNotRunning (broker ^. fromEndpoint))
