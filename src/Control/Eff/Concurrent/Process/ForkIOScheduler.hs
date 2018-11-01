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
module Control.Eff.Concurrent.Process.ForkIOScheduler
  ( schedule
  , defaultMain
  , defaultMainWithLogChannel
  , ProcEff
  , SchedulerIO
  , HasSchedulerIO
  , forkIoScheduler
  )
where

import           GHC.Stack
import           Data.Kind                      ( )
import qualified Control.Exception             as Exc
import           Control.Eff.ExceptionExtra    (liftTry)
import           Control.Concurrent            as Concurrent
import           Control.Concurrent.STM        as STM
import           Control.Eff
import           Control.Eff.Extend
import qualified Control.Eff.Exception         as Eff
import qualified Control.Eff.State.Lazy        as Eff
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
-- import           Debug.Trace
import qualified Data.Sequence                 as Seq
import           System.Timeout


-- | Information about a process, needed to implement 'MessagePassing' and
-- 'Process' handlers. The message queue is backed by a 'STM.TQueue' and contains
-- 'MessageQEntry' values.
data ProcessInfo =
                 ProcessInfo { _processId         :: ProcessId
                             , _messageQ          :: STM.TVar (Seq (Maybe Dynamic))
                             , _shutdownRequested :: STM.TVar Bool
                             , _processState      :: ProcessState
                             }

makeLenses ''ProcessInfo

instance Show ProcessInfo where
  show p =  "ProcessInfo: " ++ show (p ^. processId) ++ " " ++ show (p ^. processState)

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

-- | The concrete list of 'Eff'ects of processes compatible with this scheduler.
-- This builds upon 'SchedulerIO'.
type ProcEff = ConsProcess SchedulerIO


-- | Type class constraint to indicate that an effect union contains the
-- effects required by every process and the scheduler implementation itself.
type HasSchedulerIO r = ( HasCallStack
                        , SetMember Lift (Lift IO) r
                        , SchedulerIO <:: r
                        )

-- | The concrete list of 'Eff'ects for this scheduler implementation.
type SchedulerIO =
              '[ Reader SchedulerVar
               , Logs LogMessage
               , Lift IO
               ]

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMain :: HasCallStack => Eff ProcEff () -> IO ()
defaultMain c = runLoggingT
  (logChannelBracket 128
                     (Just (infoMessage "main process started"))
                     (schedule c)
  )
  (printLogMessage :: LogMessage -> IO ())

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMainWithLogChannel
  :: HasCallStack => LogChannel LogMessage -> Eff ProcEff () -> IO ()
defaultMainWithLogChannel logC c =
  closeLogChannelAfter logC (schedule c logC)

-- | A 'SchedulerProxy' for 'SchedulerIO'
forkIoScheduler :: SchedulerProxy SchedulerIO
forkIoScheduler = SchedulerProxy

-- | This is the main entry point to running a message passing concurrency
-- application. This function takes a 'Process' on top of the 'SchedulerIO'
-- effect and a 'LogChannel' for concurrent logging.
schedule :: HasCallStack => Eff ProcEff () -> LogChannel LogMessage -> IO ()
schedule e logC =
  void $ withNewSchedulerState $ \schedulerStateVar -> do
    pidVar <- newEmptyTMVarIO
    runProcEff schedulerStateVar pidVar
      $ do
          logNotice "++++++++ main process started ++++++++"
          e
          logNotice "++++++++ main process returned ++++++++"
 where
  withNewSchedulerState :: HasCallStack => (SchedulerVar -> IO a) -> IO a
  withNewSchedulerState mainProcessAction =
    Exc.bracket
      (newTVarIO (SchedulerState myPid Map.empty Map.empty False logC))
      tearDownScheduler
      (mainProcessAction . SchedulerVar)
   where
    myPid = 1
    tearDownScheduler v = do
      logChannelPutIO logC =<< debugMessageIO "begin scheduler tear down"
      atomically (modifyTVar' v (schedulerShuttingDown .~ True))
      yield
      do sch <- readTVarIO v
         logChannelPutIO logC =<< debugMessageIO
           (  "killing "
           ++ let ts = (sch ^.. threadIdTable . traversed)
             in  if length ts > 100
                   then show (length ts) ++ " threads"
                   else show ts
           )
         imapM_ killProcThread (sch ^. threadIdTable)
      yield
      let shutdownTimeout = 1000000
      inTime <- timeout shutdownTimeout
        (atomically
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
          ))
      logChannelPutIO logC  =<<
        maybe
           (do pt <- atomically ( (view processTable <$> readTVar v) <*
                                   modifyTVar v
                                     ( (threadIdTable .~ Map.empty)
                                     . (processTable  .~ Map.empty))
                                )
               errorMessageIO ("timeout while shutting down threads " ++ show pt ++ "\n\n\n"))
           (const (infoMessageIO "all threads dead"))
           inTime
      threadDelay 1000000

    killProcThread _pid tid = killThread tid >> yield

runProcEff
  :: HasCallStack
  => SchedulerVar
  -> STM.TMVar ProcessId
  -> Eff ProcEff ()
  -> IO ()
runProcEff schedulerVar pidVar procAction = do
  logC       <- getLogChannelIO schedulerVar
  eres       <- go logC
  logResult eres logC
 where
  go :: LogChannel LogMessage -> IO ProcessExitReason
  go l = do
    eres2   <- Exc.try
      (runLift
        (logToChannel
          l
          (runReader schedulerVar
              (interceptLogging (setLogMessageThreadId >=> logMsg)
                  (runProcessWithMessageQueue storePidAndRunProcess)))))
    case eres2 of
      Left  s    -> return (ProcessCaughtIOException  "runProcEff" s)
      Right res1 -> return res1
   where
    storePidAndRunProcess pid = do
      lift (atomically (STM.putTMVar pidVar pid))
      logDebug "enter process"
      procAction
      logDebug "process returned"
      return ProcessReturned
  logResult
    :: ProcessExitReason
    -> LogChannel LogMessage
    -> IO ()
  logResult ex lc =
    let lo = (logChannelPutIO lc =<<)
    in case ex of
        ProcessReturned     -> return ()
        ProcessShutDown -> lo $ debugMessageIO "shutdown"
        ProcessExitError m ->
          lo $ errorMessageIO ("exit with error: " ++ show m)
        ProcessCaughtIOException w m ->
          lo $ errorMessageIO ("runtime exception: "
                          ++ Exc.displayException m ++ " " ++ w)
        ProcessRaisedError m ->
          lo $ errorMessageIO ("unhandled process exception: " ++ show m)
        _ -> lo $ errorMessageIO ("scheduler error: " ++ show ex)

-- ** MessagePassing execution

spawnImpl :: (HasSchedulerIO r) => Eff ProcEff () -> Eff r ProcessId
spawnImpl mfa = do
  schedulerVar <- getSchedulerVar
  pidVar       <- lift STM.newEmptyTMVarIO
  void $ lift $ Concurrent.forkIO $ void $ runProcEff
    schedulerVar
    pidVar
    mfa
  lift Concurrent.yield -- this is important, removing this causes test failures
  lift (atomically (STM.readTMVar pidVar))

runProcessWithMessageQueue
  :: HasCallStack
  => (ProcessId -> Eff ProcEff ProcessExitReason)
  -> Eff SchedulerIO ProcessExitReason
runProcessWithMessageQueue procAction =
  withNewMessageQueue $  -- Sxyz = xy(xz)
    handleProcess <*> procAction

handleProcess
  :: HasCallStack
  => ProcessId
  -> Eff ProcEff ProcessExitReason
  -> Eff SchedulerIO ProcessExitReason
handleProcess pid =
   handle_relay_s
      0
      (const return)
      (\ !x !y !k ->
        go
          x
          y
          (\ !nextRef' ->
                 either
                 (\ !z -> do
                    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessShuttingDown
                    return z )
                 (\ !z -> do
                    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessIdle
                    lift yield
                    k nextRef' z))
      )
 where
  go
    :: forall v a
     . HasCallStack
    => Int
    -> Process SchedulerIO v
    -> (Int -> Arr SchedulerIO (Either ProcessExitReason v) a)
    -> Eff SchedulerIO a
  go nextRef (SendMessage toPid reqIn) k =
    shutdownOrGo (k nextRef) $ do
      void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessBusy
      eres <- runSchedulerState $ overProcessInfo toPid $ enqueueMessage reqIn
      let kArg = either (OnError . show @ProcessExitReason) ResumeWith eres
      k nextRef (Right kArg)

  go nextRef (SendShutdown toPid) k = shutdownOrGo (k nextRef) $ do
    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessBusy
    eres <- runSchedulerState $ overProcessInfo toPid setShutdownRequested
    let kArg   = either (OnError . show @ProcessExitReason) resume eres
        resume = if toPid == pid then const ShutdownRequested else ResumeWith
    k nextRef (Right kArg)

  go nextRef (Spawn child) k = shutdownOrGo (k nextRef) $ do
    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessBusy
    res <- spawnImpl child
    k nextRef (Right (ResumeWith res))

  go nextRef ReceiveMessage k = shutdownOrGo (k nextRef) $ do
    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState BlockedByReceive
    emdynMsg <- runSchedulerState $
      runProcessInfoState pid $ do
        mq <- use messageQ
        lift $ do
          messages <- readTVar mq
          case messages of
            r :<| st -> do
              writeTVar mq st
              return r
            Seq.Empty ->
              retry
    k nextRef
      (Right (either (OnError . show @ProcessExitReason)
                     (maybe RetryLastAction ResumeWith)
                     emdynMsg))

  go nextRef (ReceiveMessageSuchThat selectMessage) k =
    shutdownOrGo (k nextRef) $ do
      void $ runSchedulerState $ runProcessInfoState pid
           $ setProcessState BlockedByReceiveSuchThat
      emdynMsg <- runSchedulerState $
        runProcessInfoState pid $ do
          mq <- use messageQ
          lift $
            let
              untilMessageMatches = do
                messages <- readTVar mq
                maybe
                  retry
                  (\(mMsg, messagesOut) -> do
                    writeTVar mq messagesOut
                    return mMsg
                  )
                  (partitionMessages messages Seq.Empty)
              partitionMessages Seq.Empty _acc =
                Nothing
              partitionMessages (Nothing :<| msgRest) acc =
                (Just (Nothing, acc >< msgRest))
              partitionMessages (Just m :<| msgRest) acc = maybe
                (partitionMessages msgRest (acc :|> Just m))
                (\ !res -> Just (Just res, acc >< msgRest))
                (runMessageSelector selectMessage m)
           in untilMessageMatches
      k nextRef
        (Right (either (OnError . show @ProcessExitReason)
                (maybe RetryLastAction ResumeWith)
                emdynMsg
        ))

  go nextRef SelfPid k = shutdownOrGo (k nextRef) $ do
    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessBusy
    k nextRef (Right (ResumeWith pid))

  go nextRef MakeReference k = shutdownOrGo (k nextRef) $ do
    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessBusy
    k (nextRef + 1) (Right (ResumeWith nextRef))

  go nextRef YieldProcess !k = shutdownOrGo (k nextRef) $ do
    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessBusy
    k nextRef (Right (ResumeWith ()))

  go nextRef Shutdown k = do
    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessShuttingDown
    k nextRef (Left ProcessReturned)

  go nextRef (ExitWithError msg) k = do
    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessShuttingDown
    k nextRef (Left (ProcessExitError msg))

  go nextRef (RaiseError msg) k = do
    void $ runSchedulerState $ runProcessInfoState pid $ setProcessState ProcessShuttingDown
    k nextRef (Left (ProcessRaisedError msg))

  shutdownOrGo
    :: forall v a
     . HasCallStack
    => (Either ProcessExitReason (ResumeProcess v) -> Eff SchedulerIO a)
    -> Eff SchedulerIO a
    -> Eff SchedulerIO a
  shutdownOrGo !k !ok = do
    eHasShutdowReq <- runSchedulerState $
      overProcessInfoBool pid getAndResetShutdownRequested
    void $ runSchedulerState $ overProcessInfo pid $ setProcessState ProcessIdle
    case eHasShutdowReq of
      Right True  -> do
        logDebug "ShutdownRequested"
        k (Right ShutdownRequested)
      Right False -> ok
      Left  e     -> do
        logDebug ("ShutdownRequest check error: "++show e)
        k (Left e)

withNewMessageQueue
  :: HasCallStack
  => (ProcessId -> Eff SchedulerIO ProcessExitReason)
  -> Eff SchedulerIO ProcessExitReason
withNewMessageQueue m = do
  mpid <- createQueue
  case mpid of
    Right !pid ->
         interceptLogging
            (logMsg . over lmMessage
              (printf "% 9s %s" (show pid)))
            (do res <- liftTry (m pid)
                destroyQueue 3 pid
                return (either (ProcessCaughtIOException "after destroyQueue") id res))
    Left e -> return e

createQueue :: HasCallStack => Eff SchedulerIO (Either ProcessExitReason ProcessId)
createQueue = do
    myTId <- lift myThreadId
    runSchedulerState
      (do
        abortNow <- use schedulerShuttingDown
        if abortNow
          then Eff.throwError SchedulerShuttingDown
          else do
            pid               <- nextPid <<+= 1
            channel           <- lift (newTVar Seq.Empty)
            shutdownIndicator <- lift (newTVar False)
            let pinfo = ProcessInfo pid channel shutdownIndicator ProcessIdle
            threadIdTable . at pid .= Just myTId
            processTable . at pid .= Just pinfo
            return pid
      )

destroyQueue :: HasCallStack => Int -> ProcessId -> Eff SchedulerIO ()
destroyQueue retries pid = do
    didWork <- runSchedulerState $ do
      processTable . at pid .= Nothing
      threadIdTable . at pid .= Nothing
    case didWork of
      Right () ->
        logDebug "process message queue destroyed"
      Left e -> do
        logError ("failed to destroy queue: " ++ show e)
        when (retries > 0) $ do
          logDebug "retrying to destroy process message queue"
          lift (threadDelay 100)
          destroyQueue (retries - 1) pid

-- Scheduler Accessor

getLogChannelIO :: SchedulerVar -> IO (LogChannel LogMessage)
getLogChannelIO s = view logChannel <$> readTVarIO (fromSchedulerVar s)

getSchedulerTVar :: HasSchedulerIO r => Eff r (TVar SchedulerState)
getSchedulerTVar = fromSchedulerVar <$> ask

getSchedulerVar :: HasSchedulerIO r => Eff r SchedulerVar
getSchedulerVar = ask

-- State-ful Scheduler Accessors

-- | Execute an action on the process info of the process id, and return it's
-- result. If no such process exists, return 'False'.
overProcessInfoBool
  :: HasCallStack
  => ProcessId
  -> Eff ProcessInfoStateEff Bool
  -> Eff SchedulerStateEff Bool
overProcessInfoBool toPid a =
   Eff.catchError @ProcessExitReason
    (runProcessInfoState toPid a)
    (const (return False))

-- | Execute an action on the process info of the process id, and return it's
-- result. If no such process exists, return 'False'.
overProcessInfo
  :: HasCallStack
  => ProcessId
  -> Eff ProcessInfoStateEff ()
  -> Eff SchedulerStateEff Bool
overProcessInfo toPid a =
  overProcessInfoBool toPid (a >> return True)

-- | Set the process state
setProcessState :: HasCallStack => ProcessState -> Eff ProcessInfoStateEff ()
setProcessState st =
  processState .= st

enqueueMessage ::
  HasCallStack => Dynamic -> Eff ProcessInfoStateEff ()
enqueueMessage reqIn = do
  mq <- use messageQ
  lift (modifyTVar' mq (Just reqIn :<|))

-- | wake receiving process
wakeReceivingProcess :: HasCallStack => Eff ProcessInfoStateEff ()
wakeReceivingProcess = do
  st <- use processState
  when (st == BlockedByReceive || st == BlockedByReceiveSuchThat) $ do
    mq <- use messageQ
    lift (modifyTVar' mq (Nothing :<|))

-- | set shutdown requested flag (wake up the process)
setShutdownRequested :: HasCallStack => Eff ProcessInfoStateEff ()
setShutdownRequested = do
  mq <- use shutdownRequested
  lift (writeTVar mq True)
  wakeReceivingProcess

-- | get and reset the shutdown requested flag
getAndResetShutdownRequested :: HasCallStack => Eff ProcessInfoStateEff Bool
getAndResetShutdownRequested = do
  mq <- use shutdownRequested
  lift $ do
    old <- readTVar mq
    writeTVar mq False
    return old


type SchedulerStateEff = '[Eff.State SchedulerState, Eff.Exc ProcessExitReason, Lift STM]
type ProcessInfoStateEff = '[Eff.State ProcessInfo, Eff.Exc ProcessExitReason, Lift STM]

instance Mtl.MonadState SchedulerState (Eff SchedulerStateEff) where
  get = Eff.get
  put = Eff.put

instance Mtl.MonadState ProcessInfo (Eff ProcessInfoStateEff) where
  get = Eff.get
  put = Eff.put

runProcessInfoState
  :: HasCallStack
  => ProcessId
  -> Eff ProcessInfoStateEff a
  -> Eff SchedulerStateEff a
runProcessInfoState pid stAction =
  do
    res <- use (processTable . at pid)
    case res of
      Nothing    -> Eff.throwError (ProcessNotFound pid)
      Just pinfo -> do
        (x, pinfoOut) <- raise (Eff.runState pinfo stAction)
        processTable . at pid . _Just .= pinfoOut
        return x

runSchedulerState
  :: HasSchedulerIO r
  => Eff SchedulerStateEff a
  -> Eff r (Either ProcessExitReason a)
runSchedulerState stAction =
      either (Left . ProcessCaughtIOException "overScheduler") id
  <$> liftTry
        (do psVar <- getSchedulerTVar
            lift $ STM.atomically $ do
              ps                   <- STM.readTVar psVar
              runLift $ Eff.runError $ do
                (result, psModified) <- Eff.runState ps stAction
                lift $ STM.writeTVar psVar psModified
                return result)
