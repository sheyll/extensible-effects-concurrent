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
  , ProcessExitReason(..)
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

-- | A sum-type with reasons for why a process exists the scheduling loop,
-- this includes errors, that can occur when scheduleing messages.
data ProcessExitReason =
    ProcessNotFound ProcessId
    -- ^ No process info was found for a 'ProcessId' during internal
    -- processing. NOTE: This is **ONLY** caused by internal errors, probably by
    -- an incorrect 'MessagePassing' handler in this module. **Sending a message
    -- to a process ALWAYS succeeds!** Even if the process does not exist.
  | ProcessRaisedError String
    -- ^ A process called 'raiseError'.
  | ProcessExitError String
    -- ^ A process called 'exitWithError'.
  | ProcessCaughtIOException String Exc.SomeException
    -- ^ A process called 'exitWithError'.
  | ProcessShuttingDown
    -- ^ A process exits.
  | ProcessReturned
    -- ^ A process function returned.
  | SchedulerShuttingDown
    -- ^ An action was not performed while the scheduler was exiting.
  deriving (Typeable, Show)

instance Semigroup ProcessExitReason where
   SchedulerShuttingDown <> _ = SchedulerShuttingDown
   _ <> SchedulerShuttingDown = SchedulerShuttingDown
   ProcessReturned <> x = x
   x <> ProcessReturned = x
   ProcessShuttingDown <> x = x
   x <> ProcessShuttingDown = x
   (ProcessRaisedError e1) <> (ProcessRaisedError e2) =
    ProcessRaisedError (e1 ++ " and " ++ e2 )
   e1 <> e2 =
    ProcessExitError (show e1 ++ " and " ++ show e2 )

instance Monoid ProcessExitReason where
  mempty = ProcessShuttingDown
  mappend = (<>)

instance Exc.Exception ProcessExitReason

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
      atomically
        (do
          sch <- readTVar v
          let sch' = sch & schedulerShuttingDown .~ True
          writeTVar v sch'
        )
      yield
      do sch <- readTVarIO v
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

    killProcThread myTId _pid tid =
      when (myTId /= tid) (killThread tid >> yield)

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
      Left  s    -> return (ProcessCaughtIOException  "runProcEff after try" s)
      Right res1 -> return res1
   where
    storePidAndRunProcess pid = do
      lift (atomically (STM.putTMVar pidVar pid))
      logDebug "begin process"
      procAction
      logDebug "process returned"
      return ProcessReturned
  logResult
    :: ProcessExitReason
    -> LogChannel LogMessage
    -> IO ()
  logResult ex lc =
    let lo = (logChannelPutIO lc =<<)
    in lo $ case ex of
        ProcessReturned     -> debugMessageIO "process returned"
        ProcessShuttingDown -> debugMessageIO "process shutdown"
        ProcessExitError m ->
          errorMessageIO ("process exited with error: " ++ show m)
        ProcessCaughtIOException w m ->
          errorMessageIO ("process threw IO exception: "
                          ++ Exc.displayException m ++ " " ++ w)
        ProcessRaisedError m ->
          errorMessageIO ("unhandled process exception: " ++ show m)
        _ -> errorMessageIO ("scheduler error: " ++ show ex)

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
  withNewMessageQueue $  -- SFGX = FX(GX)
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
          (\ !nextRef' -> either return (k nextRef'))
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
      let kArg = either (OnError . show @ProcessExitReason) ResumeWith eres
      k nextRef (Right kArg)

  go nextRef (SendShutdown toPid) k = shutdownOrGo (k nextRef) $ do
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
    let kArg   = either (OnError . show @ProcessExitReason) resume eres
        resume = if toPid == pid then const ShutdownRequested else ResumeWith
    lift Concurrent.yield
    k nextRef (Right kArg)

  go nextRef (Spawn child) k = shutdownOrGo (k nextRef) $ do
    res <- spawnImpl child
    k nextRef (Right (ResumeWith res))

  go nextRef ReceiveMessage k = shutdownOrGo (k nextRef) $ do
    emdynMsg <- overSchedulerXX $ do
      mq <- overProcessInfo pid (use messageQ)
      messages <- lift (readTVar mq)
      case messages of
        r :<| st -> do
          lift (writeTVar mq st)
          return r
        Seq.Empty ->
          lift retry
    k nextRef
      (Right (either (OnError . show @ProcessExitReason)
                     (maybe RetryLastAction ResumeWith)
                     emdynMsg))

  go nextRef (ReceiveMessageSuchThat selectMessage) k =
    shutdownOrGo (k nextRef) $ do
      emq <- overSchedulerXX (overProcessInfo pid (use messageQ))
      case emq of
        Left  e  -> k nextRef (Right (OnError (show @ProcessExitReason e)))
        Right mq -> do
          let
            readTQueueUntilMatches =
              let
                partitionMessages Seq.Empty _acc =
                  Nothing
                partitionMessages (Nothing :<| msgRest) acc =
                  (Just (Nothing, acc >< msgRest))
                partitionMessages (Just m :<| msgRest) acc = maybe
                  (partitionMessages msgRest (acc :|> Just m))
                  (\ !res -> Just (Just res, acc >< msgRest))
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
          emdynMsg <- liftTry (lift (Right <$> atomically readTQueueUntilMatches))
          k
            nextRef
            (Right (either (OnError . show @ProcessExitReason)
                   (maybe RetryLastAction ResumeWith)
                   (join emdynMsg)
            ))

  go nextRef SelfPid k = shutdownOrGo (k nextRef) $ do
    lift Concurrent.yield
    k nextRef (Right (ResumeWith pid))

  go nextRef MakeReference k = shutdownOrGo (k nextRef) $ do
    lift Concurrent.yield
    k (nextRef + 1) (Right (ResumeWith nextRef))

  go nextRef YieldProcess !k = shutdownOrGo (k nextRef) $ do
    lift Concurrent.yield
    k nextRef (Right (ResumeWith ()))

  go nextRef Shutdown k = k nextRef (Left ProcessReturned)

  go nextRef (ExitWithError msg) k =
    k nextRef (Left (ProcessExitError msg))

  go nextRef (RaiseError msg) k =
    k nextRef (Left (ProcessRaisedError msg))

  shutdownOrGo
    :: forall v a
     . HasCallStack
    => (Either ProcessExitReason (ResumeProcess v) -> Eff SchedulerIO a)
    -> Eff SchedulerIO a
    -> Eff SchedulerIO a
  shutdownOrGo !k !ok = do
    psVar          <- getSchedulerTVar
    eHasShutdowReq <- join <$> liftTry (lift
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
      ))
    case eHasShutdowReq of
      Right True  -> logDebug "ShutdownRequested" >> k (Right ShutdownRequested)
      Right False -> ok
      Left  e     -> logDebug ("ShutdownRequest check error: "++show e)
                       >> k (Right (OnError (show e)))

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
                return (either (ProcessCaughtIOException "after detroyQueue") id res))
    Left e -> return e

createQueue :: HasCallStack => Eff SchedulerIO (Either ProcessExitReason ProcessId)
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
            return (Right pid)
      )

destroyQueue :: HasCallStack => Int -> ProcessId -> Eff SchedulerIO ()
destroyQueue retries pid = do
    logDebug "destroy process message queue"
    didWork <- overSchedulerXX $ do
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

-- * State Accessor

type SchedulerStateEff = '[Eff.State SchedulerState, Eff.Exc ProcessExitReason, Lift STM]
type PrcoessInfoStateEff = '[Eff.State ProcessInfo, Eff.Exc ProcessExitReason, Lift STM]

instance Mtl.MonadState SchedulerState (Eff SchedulerStateEff) where
  get = Eff.get
  put = Eff.put

instance Mtl.MonadState ProcessInfo (Eff PrcoessInfoStateEff) where
  get = Eff.get
  put = Eff.put

overProcessInfo
  :: HasCallStack
  => ProcessId
  -> Eff PrcoessInfoStateEff a
  -> Eff SchedulerStateEff a
overProcessInfo pid stAction =
  do
    res <- use (processTable . at pid)
    case res of
      Nothing    -> Eff.throwError (ProcessNotFound pid)
      Just pinfo -> do
        (x, pinfoOut) <- raise (Eff.runState pinfo stAction)
        processTable . at pid . _Just .= pinfoOut
        return x

overSchedulerXX
  :: HasSchedulerIO r
  => Eff SchedulerStateEff a
  -> Eff r (Either ProcessExitReason a)
overSchedulerXX stAction =
      either (Left . ProcessCaughtIOException "overSchedulerXX") id
  <$> liftTry
        (do psVar <- getSchedulerTVar
            lift $ STM.atomically $ do
              ps                   <- STM.readTVar psVar
              runLift $ Eff.runError $ do
                (result, psModified) <- Eff.runState ps stAction
                lift $ STM.writeTVar psVar psModified
                return result
        )

overScheduler
  :: HasSchedulerIO r
  => Mtl.StateT SchedulerState STM.STM (Either ProcessExitReason a)
  -> Eff r (Either ProcessExitReason a)
overScheduler stAction =
  either (Left . ProcessCaughtIOException "overScheduler") id
  <$> liftTry
  (do
    psVar <- getSchedulerTVar
    lift $ STM.atomically $ do
      ps                   <- STM.readTVar psVar
      (result, psModified) <- Mtl.runStateT stAction ps
      STM.writeTVar psVar psModified
      return result
  )


getLogChannelIO :: SchedulerVar -> IO (LogChannel LogMessage)
getLogChannelIO s = view logChannel <$> readTVarIO (fromSchedulerVar s)

getSchedulerTVar :: HasSchedulerIO r => Eff r (TVar SchedulerState)
getSchedulerTVar = fromSchedulerVar <$> ask

getSchedulerVar :: HasSchedulerIO r => Eff r SchedulerVar
getSchedulerVar = ask
