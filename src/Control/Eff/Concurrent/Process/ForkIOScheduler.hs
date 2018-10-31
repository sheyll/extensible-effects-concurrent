-- | Implement Erlang style message passing concurrency.
--
-- This handles the 'MessagePassing' and 'Process' effects, using
-- 'STM.TQueue's and 'forkIO'.
--
-- This aims to be a pragmatic implementation, so even logging is
-- supported.
--
-- At the core is a /main process/ that enters 'schedule'
-- and creates all of the internal state stored in 'STM.TVar's
-- to manage processes with message queues.
--
-- The 'Eff' handler for 'Process' and 'MessagePassing' use
-- are implemented and available through 'spawn'.
--
-- 'spawn' uses 'forkFinally' and 'STM.TQueue's and tries to catch
-- most exceptions.
module Control.Eff.Concurrent.Process.ForkIOScheduler
  ( schedule
  , defaultMain
  , defaultMainWithLogChannel
  , SchedulerError(..)
  , SchedulerIO
  , forkIoScheduler
  , HasSchedulerIO
  )
where

import           Data.Foldable
import           GHC.Stack
import           Data.Bifunctor
import           Data.Maybe
import           Data.Kind                      ( )
import qualified Control.Exception             as Exc
import           Control.Concurrent            as Concurrent
import           Control.Concurrent.STM        as STM
import           Control.Eff
import           Control.Eff.Cut               as Eff
import           Control.Eff.Extend
import           Control.Eff.Exception         as Eff
import           Control.Eff.Choose            as Eff
import           Control.Eff.Concurrent.Process
import           Control.Eff.Lift
import           Control.Eff.Log
import           Control.Eff.Reader.Strict     as Reader
import           Control.Monad.Log              ( runLoggingT )
import           Control.Lens
import           Control.Monad                  ( when
                                                , void
                                                , join
                                                , (>=>)
                                                )
import qualified Control.Monad.State           as Mtl
import           Data.Dynamic
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Text.Printf
import           Data.Sequence                  ( Seq(..)
                                                , (><)
                                                )
import           Debug.Trace
import qualified Data.Sequence                 as Seq

-- | Information about a process, needed to implement 'MessagePassing' and
-- 'Process' handlers. The message queue is backed by a 'STM.TQueue' and contains
-- 'MessageQEntry' values.
data ProcessInfo =
                 ProcessInfo { _processId         :: ProcessId
                             , _messageQ          :: STM.TVar (Seq (Maybe Dynamic))
                             , _shutdownRequested :: STM.TVar Bool
                             }

makeLenses ''ProcessInfo

instance Show ProcessInfo where
  show p =  "ProcessInfo: " ++ show (p ^. processId)

-- | Contains all process info'elements, as well as the state needed to
-- implement inter process communication. It contains also a 'LogChannel' to
-- which the logs of all processes are forwarded to.
data SchedulerState =
               SchedulerState { _nextPid :: ProcessId
                              , _processTable :: Map ProcessId ProcessInfo
                              , _threadIdTable :: Map ProcessId ThreadId
                              , _schedulerShuttingDown :: Bool
                              , _logChannel :: LogChannel LogMessage
                              }

makeLenses ''SchedulerState

-- | A newtype wrapper around an 'STM.TVar' holding the scheduler state.
-- This is needed by 'spawn' and provided by 'runScheduler'.
newtype SchedulerVar = SchedulerVar { fromSchedulerVar :: STM.TVar SchedulerState }
  deriving Typeable

-- | A sum-type with errors that can occur when scheduleing messages.
data SchedulerError =
    ProcessNotFound ProcessId
    -- ^ No process info was found for a 'ProcessId' during internal
    -- processing. NOTE: This is **ONLY** caused by internal errors, probably by
    -- an incorrect 'MessagePassing' handler in this module. **Sending a message
    -- to a process ALWAYS succeeds!** Even if the process does not exist.
  | ProcessRaisedError String
    -- ^ A process called 'raiseError'.
  | ProcessExitError String
    -- ^ A process called 'exitWithError'.
  | ProcessShuttingDown
    -- ^ A process exits.
  | SchedulerShuttingDown
    -- ^ An action was not performed while the scheduler was exiting.
  deriving (Typeable, Show)

instance Exc.Exception SchedulerError

-- | An alias for the constraints for the effects essential to this scheduler
-- implementation, i.e. these effects allow 'spawn'ing new 'Process'es.
-- See SchedulerIO
type HasSchedulerIO r = ( HasCallStack
                        , SetMember Lift (Lift IO) r
                        , Member (Logs LogMessage) r
                        , Member (Reader SchedulerVar) r
                        , Member Choose r
                        , Member (Exc CutFalse) r
                        )

-- | The concrete list of 'Eff'ects for this scheduler implementation.
-- See HasSchedulerIO
type SchedulerIO =
              '[ Exc CutFalse
               , Choose
               , Reader SchedulerVar
               , Logs LogMessage
               , Lift IO
               ]

-- | A 'SchedulerProxy' for 'SchedulerIO'
forkIoScheduler :: SchedulerProxy SchedulerIO
forkIoScheduler = SchedulerProxy

-- | This is the main entry point to running a message passing concurrency
-- application. This function takes a 'Process' on top of the 'SchedulerIO'
-- effect and a 'LogChannel' for concurrent logging.
schedule
  :: HasCallStack
  => Eff (ConsProcess SchedulerIO) ()
  -> LogChannel LogMessage
  -> IO ()
schedule e logC =
  withFrozenCallStack $ void $ withNewSchedulerState $ \schedulerStateVar -> do
    pidVar <- newEmptyTMVarIO
    scheduleProcessWithShutdownAction schedulerStateVar pidVar
      $ interceptLogging (setLogMessageThreadId >=> logMsg)
      $ do
          logNotice "++++++++ main process started ++++++++"
          e
          logNotice "++++++++ main process returned ++++++++"
 where
  withNewSchedulerState :: HasCallStack => (SchedulerVar -> IO a) -> IO a
  withNewSchedulerState mainProcessAction = do
    myTId <- myThreadId
    Exc.bracket
      (newTVarIO (SchedulerState myPid Map.empty Map.empty False logC))
      (tearDownScheduler myTId)
      (mainProcessAction . SchedulerVar)
   where
    myPid = 1
    tearDownScheduler myTId v = do
      logChannelPutIO logC =<< debugMessageIO "begin scheduler tear down"
      sch <-
        (atomically
          (do
            sch <- readTVar v
            let sch' = sch & schedulerShuttingDown .~ True
            writeTVar v sch'
            return sch'
          )
        )
      logChannelPutIO logC =<< debugMessageIO
        (  "killing "
        ++ let ts = (sch ^.. threadIdTable . traversed)
           in  if length ts > 100
                 then show (length ts) ++ " threads"
                 else show ts
        )

      imapM_ (killProcThread myTId) (sch ^. threadIdTable)
      Concurrent.yield
      atomically
        (do
          scheduler <- readTVar v
          let allThreadsDead =
                scheduler
                  ^. threadIdTable
                  .  to Map.null
                  && scheduler
                  ^. processTable
                  .  to Map.null
          STM.check allThreadsDead
        )
      logChannelPutIO logC =<< infoMessageIO "all threads dead"

    killProcThread myTId _pid tid = when (myTId /= tid) (killThread tid)

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMain :: HasCallStack => Eff (ConsProcess SchedulerIO) () -> IO ()
defaultMain c = withFrozenCallStack $ runLoggingT
  (logChannelBracket 128
                     (Just (infoMessage "main process started"))
                     (schedule c)
  )
  (printLogMessage :: LogMessage -> IO ())

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMainWithLogChannel
  :: HasCallStack
  => LogChannel LogMessage
  -> Eff (ConsProcess SchedulerIO) ()
  -> IO ()
defaultMainWithLogChannel logC c =
  withFrozenCallStack $ closeLogChannelAfter logC (schedule c logC)

scheduleProcessWithShutdownAction
  :: HasCallStack
  => SchedulerVar
  -> STM.TMVar ProcessId
  -> Eff (ConsProcess SchedulerIO) ()
  -> IO ()
scheduleProcessWithShutdownAction schedulerVar pidVar procAction = do
  cleanupVar <- newEmptyTMVarIO
  logC       <- getLogChannelIO schedulerVar
  eeres      <- Exc.try (runProcEffects cleanupVar logC)
  let eres = case eeres of
        Left  se  -> [se]
        Right ses -> ses
  getAndExecCleanup cleanupVar eres logC
  logChannelPutIO logC =<< debugMessageIO "process cleanup finished"
 where

  runProcEffects cleanupVar l = do
    eresVar <- newEmptyTMVarIO
    fmap (Data.Bifunctor.first Exc.SomeException) <$> runLift
      (logToChannel
        l
        (runReader
          schedulerVar
          (Eff.makeChoice
            (Eff.call
              (scheduleProcessWithCleanup (shutdownAction eresVar)
                                          saveCleanupAndSchedule
              )
            )
          )
        )
      )
   where
    shutdownAction :: TMVar SchedulerError -> ShutdownAction
    shutdownAction eresVar = ShutdownAction
      (\eres -> do
        let ex = either id (const ProcessShuttingDown) eres
        mt <- myThreadId
        traceM (show mt ++ "++++++++++++++++++++++ about to throw " ++ show ex)
        STM.atomically (putTMVar eresVar ex)
      )
    saveCleanupAndSchedule cleanUpAction pid = do
      lift
        (atomically
          (do
            STM.putTMVar cleanupVar cleanUpAction
            STM.putTMVar pidVar pid
          )
        )
      interceptLogging
          (setLogMessageThreadId >=> logMsg . over
            lmMessage
            (printf "% 9s %s" (show pid))
          )
        $ do
            logDebug "begin process"
            procAction
  getAndExecCleanup cleanupVar eress lc = do
    mt <- myThreadId
    traceM
      (show mt ++ "++++++++++++++++++++++ clean up with eres: " ++ show eress)
    mcleanup <- atomically (STM.tryTakeTMVar cleanupVar)
    traverse_ execCleanup mcleanup
   where
    execCleanup ca = do
      runCleanUpAction ca
      traverse_
        (\eres -> logChannelPutIO lc =<< case eres of
          Left se -> case Exc.fromException se of
            Nothing -> errorMessageIO
              ("process caught exception: " ++ Exc.displayException se)
            Just schedulerErr
              -> let exceptionMsg = " - full exception message: "
                       ++ Exc.displayException se
                 in
                   case schedulerErr of
                     ProcessShuttingDown -> debugMessageIO "process shutdown"
                     ProcessExitError m ->
                       errorMessageIO
                         (  "process exited with error: "
                         ++ show m
                         ++ exceptionMsg
                         )
                     ProcessRaisedError m -> errorMessageIO
                       ("process raised error: " ++ show m ++ exceptionMsg)
                     _ ->
                       errorMessageIO
                         (  "scheduler error: "
                         ++ show schedulerErr
                         ++ exceptionMsg
                         )
          Right _ -> debugMessageIO "process function returned"
        )
        eress


getLogChannel :: HasSchedulerIO r => Eff r (LogChannel LogMessage)
getLogChannel = do
  s <- getSchedulerVar
  lift (getLogChannelIO s)

getLogChannelIO :: SchedulerVar -> IO (LogChannel LogMessage)
getLogChannelIO s = view logChannel <$> readTVarIO (fromSchedulerVar s)

overProcessInfo
  :: HasSchedulerIO r
  => ProcessId
  -> Mtl.StateT ProcessInfo STM.STM a
  -> Eff r (Either SchedulerError a)
overProcessInfo pid stAction = overScheduler
  (do
    res <- use (processTable . at pid)
    case res of
      Nothing    -> return (Left (ProcessNotFound pid))
      Just pinfo -> do
        (x, pinfoOut) <- Mtl.lift (Mtl.runStateT stAction pinfo)
        processTable . at pid . _Just .= pinfoOut
        return (Right x)
  )

-- ** MessagePassing execution

spawnImpl
  :: HasCallStack
  => Eff (ConsProcess SchedulerIO) ()
  -> Eff SchedulerIO ProcessId
spawnImpl mfa = do
  schedulerVar <- ask
  pidVar       <- lift STM.newEmptyTMVarIO
  void $ lift $ Concurrent.forkIO $ void $ scheduleProcessWithShutdownAction
    schedulerVar
    pidVar
    mfa
  lift Concurrent.yield -- this is important, removing this causes test failures
  lift (atomically (STM.readTMVar pidVar))

newtype CleanUpAction = CleanUpAction { runCleanUpAction :: IO () }

scheduleProcessWithCleanup
  :: HasCallStack
  => ShutdownAction
  -> (CleanUpAction -> ProcessId -> Eff (ConsProcess SchedulerIO) ())
  -> Eff SchedulerIO (Either SchedulerError ())
scheduleProcessWithCleanup shutdownAction processAction = withMessageQueue
  (\cleanUpAction pinfo -> handle_relay_s
    0
    (const return)
    (go (pinfo ^. processId))
    (processAction cleanUpAction (pinfo ^. processId))
  )
 where
  go
    :: forall v a
     . HasCallStack
    => ProcessId
    -> Int
    -> Process SchedulerIO v
    -> (Int -> Arr SchedulerIO v a)
    -> Eff SchedulerIO a
  go pid nextRef (SendMessage toPid reqIn) k =
    shutdownOrGo pid (k nextRef) $ do
      eres <- do
        psVar <- getSchedulerTVar
        lift
          (   Right
          <$> (do
                p <- readTVarIO psVar
                let mto = p ^. processTable . at toPid
                case mto of
                  Just toProc -> do
                    atomically
                      (modifyTVar' (toProc ^. messageQ) (Just reqIn :<|))
                    return True
                  Nothing -> return False
              )
          )
      lift Concurrent.yield
      let kArg = either (OnError . show @SchedulerError) ResumeWith eres
      k nextRef kArg

  go pid nextRef (SendShutdown toPid) k = shutdownOrGo pid (k nextRef) $ do
    eres <- do
      psVar <- getSchedulerTVar
      lift
        (   Right
        <$> (do
              p <- readTVarIO psVar
              let mto = p ^. processTable . at toPid
              case mto of
                Just toProc -> atomically $ do
                  writeTVar (toProc ^. shutdownRequested) True
                  modifyTVar' (toProc ^. messageQ) (Nothing :<|) -- this wakes up a receiver
                  return True
                Nothing -> return False
            )
        )
    let kArg   = either (OnError . show @SchedulerError) resume eres
        resume = if toPid == pid then const ShutdownRequested else ResumeWith
    lift Concurrent.yield
    k nextRef kArg

  go pid nextRef (Spawn child) k = shutdownOrGo pid (k nextRef) $ do
    res <- spawnImpl child
    k nextRef (ResumeWith res)

  go pid nextRef ReceiveMessage k = shutdownOrGo pid (k nextRef) $ do
    emq <- overProcessInfo pid (use messageQ)
    case emq of
      Left  e  -> k nextRef (OnError (show @SchedulerError e))
      Right mq -> do
        emdynMsg <- lift
          (Right <$> atomically
            (do
              messages <- readTVar mq
              case messages of
                r :<| st -> do
                  writeTVar mq st
                  return r
                Seq.Empty -> retry
            )
          )
        k
          nextRef
          (either (OnError . show @SchedulerError)
                  (maybe RetryLastAction ResumeWith)
                  emdynMsg
          )

  go pid nextRef (ReceiveMessageSuchThat selectMessage) k =
    shutdownOrGo pid (k nextRef) $ do
      emq <- overProcessInfo pid (use messageQ)
      case emq of
        Left  e  -> k nextRef (OnError (show @SchedulerError e))
        Right mq -> do
          let
            readTQueueUntilMatches =
              let
                partitionMessages Seq.Empty _acc =
                  trace (show pid ++ ": message queue empty") Nothing
                partitionMessages (Nothing :<| msgRest) acc =
                  (trace
                    (printf "%s: interrupted! putting back %d + %d"
                            (show pid)
                            (Seq.length acc)
                            (Seq.length msgRest)
                    )
                    Just
                    (Nothing, acc >< msgRest)
                  )
                partitionMessages (Just m :<| msgRest) acc = maybe
                  (partitionMessages msgRest (acc :|> Just m))
                  (\res -> Just
                    (trace
                      (printf "%s: message matched, putting back %d + %d"
                              (show pid)
                              (Seq.length acc)
                              (Seq.length msgRest)
                      )
                      (Just res, acc >< msgRest)
                    )
                  )
                  (runMessageSelector selectMessage m)
                untilMessageMatches = do
                  messages <- readTVar mq
                  maybe
                    retry
                    (\(mMsg, messagesOut) -> do
                      writeTVar mq messagesOut
                      return mMsg
                    )
                    (partitionMessages messages Seq.Empty)
              in
                untilMessageMatches
          emdynMsg <- lift (Right <$> atomically readTQueueUntilMatches)
          k
            nextRef
            (either (OnError . show @SchedulerError)
                    (maybe RetryLastAction ResumeWith)
                    emdynMsg
            )

  go pid nextRef SelfPid k = shutdownOrGo pid (k nextRef) $ do
    lift Concurrent.yield
    k nextRef (ResumeWith pid)

  go pid nextRef MakeReference k = shutdownOrGo pid (k nextRef) $ do
    lift Concurrent.yield
    k (nextRef + 1) (ResumeWith nextRef)

  go pid nextRef YieldProcess !k = shutdownOrGo pid (k nextRef) $ do
    lift Concurrent.yield
    k nextRef (ResumeWith ())

  go _pid _nextRef Shutdown _k = invokeShutdownAction shutdownAction (Right ())

  go _pid _nextRef (ExitWithError msg) _k =
    invokeShutdownAction shutdownAction (Left (ProcessExitError msg))

  go _pid _nextRef (RaiseError msg) _k =
    invokeShutdownAction shutdownAction (Left (ProcessExitError msg))

  shutdownOrGo
    :: forall v a
     . HasCallStack
    => ProcessId
    -> (ResumeProcess v -> Eff SchedulerIO a)
    -> Eff SchedulerIO a
    -> Eff SchedulerIO a
  shutdownOrGo pid !k !ok = do
    psVar          <- getSchedulerTVar
    eHasShutdowReq <- lift
      (do
        p <- readTVarIO psVar
        let mPinfo = p ^. processTable . at pid
        case mPinfo of
          Just !pinfo -> atomically
            (do
              wasRequested <- readTVar (pinfo ^. shutdownRequested)
              -- reset the shutdwown request flag, in case the shutdown
              -- is prevented by the process
              writeTVar (pinfo ^. shutdownRequested) False
              return (Right wasRequested)
            )
          Nothing -> return (Left (ProcessNotFound pid))
      )
    case eHasShutdowReq of
      Right True  -> k ShutdownRequested
      Right False -> ok
      Left  e     -> k (OnError (show e))


data ShutdownAction =
  ShutdownAction (forall a . Either SchedulerError () -> IO a)

invokeShutdownAction
  :: (HasCallStack, SetMember Lift (Lift IO) r, Member (Logs LogMessage) r)
  => ShutdownAction
  -> Either SchedulerError ()
  -> Eff r a
invokeShutdownAction (ShutdownAction a) res = lift (a res)

withMessageQueue
  :: HasSchedulerIO r
  => (CleanUpAction -> ProcessInfo -> Eff r a)
  -> Eff r (Either SchedulerError a)
withMessageQueue m = do
  mpinfo <- createQueue
  lc     <- getLogChannel
  case mpinfo of
    Right pinfo -> do
      cleanUpAction <-
        getSchedulerTVar >>= return . CleanUpAction . destroyQueue
          lc
          (pinfo ^. processId)
      Right <$> m cleanUpAction pinfo
    Left e -> return $ Left e
 where
  createQueue = do
    myTId <- lift myThreadId
    overScheduler
      (do
        abortNow <- use schedulerShuttingDown
        if abortNow
          then return (Left SchedulerShuttingDown)
          else do
            pid               <- nextPid <<+= 1
            channel           <- Mtl.lift (newTVar Seq.Empty)
            shutdownIndicator <- Mtl.lift (newTVar False)
            let pinfo = ProcessInfo pid channel shutdownIndicator
            threadIdTable . at pid .= Just myTId
            processTable . at pid .= Just pinfo
            return (Right pinfo)
      )
  destroyQueue lc pid psVar = do
    didWork <- Exc.try
      (STM.atomically
        (STM.modifyTVar'
          psVar
          (\ps ->
            ps
              &  processTable
              .  at pid
              .~ Nothing
              &  threadIdTable
              .  at pid
              .~ Nothing
          )
        )
      )
    case didWork of
      Right () -> return ()
      Left (e :: Exc.SomeException) ->
        errorMessageIO ("failed to destroy queue: " ++ show e)
          >>= logChannelPutIO lc

overScheduler
  :: HasSchedulerIO r
  => Mtl.StateT SchedulerState STM.STM (Either SchedulerError a)
  -> Eff r (Either SchedulerError a)
overScheduler stAction = do
  psVar <- getSchedulerTVar
  lift (overSchedulerIO psVar stAction)

overSchedulerIO
  :: STM.TVar SchedulerState -> Mtl.StateT SchedulerState STM.STM a -> IO a
overSchedulerIO psVar stAction = STM.atomically
  (do
    ps                   <- STM.readTVar psVar
    (result, psModified) <- Mtl.runStateT stAction ps
    STM.writeTVar psVar psModified
    return result
  )

getSchedulerTVar :: HasSchedulerIO r => Eff r (TVar SchedulerState)
getSchedulerTVar = fromSchedulerVar <$> ask

getSchedulerVar :: HasSchedulerIO r => Eff r SchedulerVar
getSchedulerVar = ask
