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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
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
import           Data.Kind ()
import qualified Control.Exception             as Exc
import           Control.Concurrent            as Concurrent
import           Control.Concurrent.STM        as STM
import           Control.Eff
import           Control.Eff.Concurrent.Process
-- import           Control.Eff.ExceptionExtra
import           Control.Eff.Lift
import           Control.Eff.Log
import           Control.Eff.Reader.Strict     as Reader
import           Control.Lens
import           Control.Monad                  ( when
                                                , void
                                                , join
                                                )
import qualified Control.Monad.State           as Mtl
import           Data.Dynamic
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.String

-- | Information about a process, needed to implement 'MessagePassing' and
-- 'Process' handlers. The message queue is backed by a 'STM.TQueue' and contains
-- 'MessageQEntry' values.
data ProcessInfo =
                 ProcessInfo { _processId         :: ProcessId
                             , _messageQ          :: STM.TQueue (Maybe Dynamic)
                             , _shutdownRequested :: STM.TVar Bool
                             }

makeLenses ''ProcessInfo

instance Show ProcessInfo where
  show p =  "ProcessInfo: " ++ show (p ^. processId)

-- | Contains all 'ProcessInfo' elements, as well as the state needed to
-- implement inter process communication. It contains also a 'LogChannel' to
-- which the logs of all processes are forwarded to.
data Scheduler =
               Scheduler { _nextPid :: ProcessId
                          , _processTable :: Map ProcessId ProcessInfo
                          , _threadIdTable :: Map ProcessId ThreadId
                          , _schedulerShuttingDown :: Bool
                          , _logChannel :: LogChannel String
                          }

makeLenses ''Scheduler

-- | A newtype wrapper around an 'STM.TVar' holding a 'Scheduler' state.
-- This is needed by 'spawn' and provided by 'runScheduler'.
newtype SchedulerVar = SchedulerVar { fromSchedulerVar :: STM.TVar Scheduler }
  deriving Typeable

-- | A sum-type with errors that can occur when scheduleing messages.
data SchedulerError =
    ProcessNotFound ProcessId
    -- ^ No 'ProcessInfo' was found for a 'ProcessId' during internal
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
                        , Member (Logs String) r
                        , Member (Reader SchedulerVar) r)

-- | The concrete list of 'Eff'ects for this scheduler implementation.
-- See HasSchedulerIO
type SchedulerIO =
              '[ Reader SchedulerVar
               , Logs String
               , Lift IO
               ]

-- | A 'SchedulerProxy' for 'SchedulerIO'
forkIoScheduler :: SchedulerProxy SchedulerIO
forkIoScheduler = SchedulerProxy

-- | This is the main entry point to running a message passing concurrency
-- application. This function takes a 'Process' on top of the 'SchedulerIO'
-- effect and a 'LogChannel' for concurrent logging.
schedule :: Eff (ConsProcess SchedulerIO) () -> LogChannel String -> IO ()
schedule e logC =
  void
  $ withNewSchedulerState
  $ \schedulerStateVar ->
      do pidVar <- newEmptyTMVarIO
         scheduleProcessWithShutdownAction schedulerStateVar pidVar
           $ do mt <- lift myThreadId
                mp <- lift (atomically (readTMVar pidVar))
                logMsg (show mp ++ " main process started in thread " ++ show mt)
                e
                logMsg (show mp ++ " main process returned")
  where
    withNewSchedulerState :: (SchedulerVar -> IO a) -> IO a
    withNewSchedulerState mainProcessAction = do
      myTId <- myThreadId
      Exc.bracket
        (newTVarIO (Scheduler myPid Map.empty Map.empty False logC))
        (tearDownScheduler myTId)
        (mainProcessAction . SchedulerVar)

     where
      myPid = 1
      tearDownScheduler myTId v = do
        logChannelPutIO logC (show myTId ++" begin scheduler tear down")
        sch <-
          (atomically
            (do
              sch <- readTVar v
              let sch' = sch & schedulerShuttingDown .~ True
              writeTVar v sch'
              return sch'
            )
          )
        logChannelPutIO logC (show myTId ++ " killing " ++
                                   let ts = (sch ^.. threadIdTable. traversed)
                                   in if length ts > 100
                                      then show (length ts) ++ " threads"
                                      else show ts )
        imapM_ (killProcThread myTId) (sch ^. threadIdTable)
        Concurrent.yield
        atomically
          (do
              scheduler <- readTVar v
              let allThreadsDead = scheduler^.threadIdTable.to Map.null
                                   && scheduler^.processTable.to Map.null
              STM.check allThreadsDead)
        logChannelPutIO logC "all threads dead"


      killProcThread myTId _pid tid =
        when (myTId /= tid) ( killThread tid)

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMain :: Eff (ConsProcess SchedulerIO) () -> IO ()
defaultMain c =
  runLoggingT
    (logChannelBracket
      128
      (Just "~~~~~~ main process started")
      (Just "====== main process exited")
      (schedule c))
    (print :: String -> IO ())

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMainWithLogChannel :: LogChannel String -> Eff (ConsProcess SchedulerIO) () -> IO ()
defaultMainWithLogChannel logC c =
  closeLogChannelAfter
      (Just (fromString "====== main process exited"))
      logC
      (schedule c logC)

scheduleProcessWithShutdownAction
  :: SchedulerVar
  -> STM.TMVar ProcessId
  -> Eff (ConsProcess SchedulerIO) ()
  -> IO (Either Exc.SomeException ())
scheduleProcessWithShutdownAction schedulerVar pidVar procAction =
  do cleanupVar <- newEmptyTMVarIO
     logC <-  getLogChannelIO schedulerVar
     mTid <- myThreadId
     eeres <- Exc.try (runProcEffects cleanupVar logC)
     let eres = join eeres
     getAndExecCleanup cleanupVar eres logC mTid
     logChannelPutIO logC (show mTid ++ " <~< process cleanup finished")
     return eres
  where

    runProcEffects cleanupVar l =
      Data.Bifunctor.first Exc.SomeException
       <$> runLift
           (logToChannel l
            (runReader
              (scheduleProcessWithCleanup
                shutdownAction
                saveCleanupAndSchedule)
              schedulerVar))
      where
        shutdownAction =
          ShutdownAction
          (\eres ->
             do let ex = either id (const ProcessShuttingDown) eres
                Exc.throw ex)
        saveCleanupAndSchedule cleanUpAction pid =
          do lift (atomically (do STM.putTMVar cleanupVar cleanUpAction
                                  STM.putTMVar pidVar pid))
             mTid <- lift myThreadId
             logMsg (show mTid ++ " >~> begin process " ++ show pid)
             procAction
    getAndExecCleanup cleanupVar eres lc mt =
      do mcleanup <- atomically (STM.tryTakeTMVar cleanupVar)
         traverse_ execCleanup mcleanup
      where
        execCleanup ca =
          do runCleanUpAction ca
             logChannelPutIO lc
               $ show mt ++
               case eres of
                 Left se ->
                   case Exc.fromException se of
                     Nothing ->
                       " process caught exception: "
                       ++ Exc.displayException se
                     Just schedulerErr ->
                       (case schedulerErr of
                         ProcessShuttingDown ->
                           " process shutdown"
                         ProcessExitError m ->
                           " process exited with error: " ++ show m
                         ProcessRaisedError m ->
                           " process raised error: " ++ show m
                         _ ->
                           " scheduler error: " ++ show schedulerErr)
                       ++ " - full exception message: "
                       ++ Exc.displayException se

                 Right _ ->
                   " process function returned"


getLogChannel :: HasSchedulerIO r => Eff r (LogChannel String)
getLogChannel = do
  s <- getSchedulerVar
  lift (getLogChannelIO s)

getLogChannelIO :: SchedulerVar -> IO (LogChannel String)
getLogChannelIO s =
  (view logChannel) <$> atomically (readTVar (fromSchedulerVar s))

overProcessInfo
  :: HasSchedulerIO r
  => ProcessId
  -> Mtl.StateT ProcessInfo STM.STM a
  -> Eff r (Either SchedulerError a)
overProcessInfo pid stAction =
  overScheduler
  (do
    res <- use (processTable . at pid)
    case res of
      Nothing    -> return (Left (ProcessNotFound pid))
      Just pinfo -> do
        (x, pinfoOut) <- Mtl.lift (Mtl.runStateT stAction pinfo)
        processTable . at pid . _Just .= pinfoOut
        return (Right x))

-- ** MessagePassing execution

spawnImpl :: HasCallStack
          => Eff (ConsProcess SchedulerIO) ()
          -> Eff SchedulerIO ProcessId
spawnImpl mfa = do
  schedulerVar <- ask
  pidVar <- lift STM.newEmptyTMVarIO
  void
    $ lift
    $ Concurrent.forkIO
    $ void
    $ scheduleProcessWithShutdownAction
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
scheduleProcessWithCleanup shutdownAction processAction =
  withMessageQueue
  (\cleanUpAction pinfo ->
     handle_relay
       (\x -> return x)
       (go (pinfo ^. processId))
       (processAction cleanUpAction (pinfo ^. processId)))
 where
  shutdownOrGo :: forall v a
     . HasCallStack
    => ProcessId
    -> (ResumeProcess v -> Eff SchedulerIO a)
    -> Eff SchedulerIO a
    -> Eff SchedulerIO a
  shutdownOrGo pid k ok =
    do psVar <- getSchedulerTVar
       eHasShutdowReq <-
         lift
           (do p <- atomically (readTVar psVar)
               let mPinfo = p ^. processTable . at pid
               case mPinfo of
                 Just pinfo ->
                   atomically
                     (do wasRequested <- readTVar (pinfo ^. shutdownRequested)
                         -- reset the shutdwown request flag, in case the shutdown
                         -- is prevented by the process
                         writeTVar (pinfo ^. shutdownRequested) False
                         return (Right wasRequested))
                 Nothing ->
                   return (Left (ProcessNotFound pid)))
       case eHasShutdowReq of
         Right True ->
           k ShutdownRequested
         Right False ->
           ok
         Left e ->
           k (OnError (show e))

  go :: forall v a
     . HasCallStack
    => ProcessId
    -> Process SchedulerIO v
    -> (v -> Eff SchedulerIO a)
    -> Eff SchedulerIO a
  go pid (SendMessage toPid reqIn) k =
      shutdownOrGo pid k $
      do eres <-
           do psVar <- getSchedulerTVar
              lift
                (Right <$>
                  (do p <- atomically (readTVar psVar)
                      let mto = p ^. processTable . at toPid
                      case mto of
                          Just toProc ->
                            do atomically
                                (writeTQueue
                                 (toProc ^. messageQ)
                                 (Just reqIn))
                               return True
                          Nothing ->
                            return False))
         lift Concurrent.yield
         let kArg = either (OnError . show @SchedulerError) ResumeWith eres
         k kArg

  go pid (SendShutdown toPid) k =
    shutdownOrGo pid k
    $ do eres <-
           do psVar <- getSchedulerTVar
              lift
                (Right <$>
                  (do p <- atomically (readTVar psVar)
                      let mto = p ^. processTable . at toPid
                      case mto of
                        Just toProc ->
                          atomically $
                           do writeTVar (toProc ^. shutdownRequested) True
                              writeTQueue (toProc ^. messageQ) Nothing
                              return True
                        Nothing ->
                          return False))
         let kArg = either (OnError . show @SchedulerError) resume eres
             resume = if toPid == pid
                      then const ShutdownRequested
                      else ResumeWith
         lift Concurrent.yield
         k kArg

  go pid (Spawn child) k =
    shutdownOrGo pid k $
    do res <- spawnImpl child
       k (ResumeWith res)

  go pid ReceiveMessage k =
    shutdownOrGo pid k $
      do emq <- overProcessInfo pid (use messageQ)
         case emq of
           Left e ->
             k (OnError (show @SchedulerError e))
           Right mq -> do
             emdynMsg <- lift (Right <$> (atomically (readTQueue mq)))
             k (either
                (OnError . show @SchedulerError)
                (maybe RetryLastAction ResumeWith)
                emdynMsg)

  go pid SelfPid k =
    shutdownOrGo pid k $
    do lift Concurrent.yield
       k (ResumeWith pid)

  go _pid Shutdown _k = do
    invokeShutdownAction shutdownAction (Right ())

  go _pid (ExitWithError msg) _k = do
    invokeShutdownAction shutdownAction (Left (ProcessExitError msg))

  go _pid (RaiseError msg) _k = do
    invokeShutdownAction shutdownAction (Left (ProcessExitError msg))

data ShutdownAction =
  ShutdownAction (forall a . Either SchedulerError () -> IO a)

invokeShutdownAction :: ( HasCallStack
                       , SetMember Lift (Lift IO) r
                       , Member (Logs String) r)
                     => ShutdownAction -> Either SchedulerError () -> Eff r a
invokeShutdownAction (ShutdownAction a) res =
  lift (a res)

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
    Left e ->
      return $ Left e
 where
  createQueue = do
    myTId <- lift myThreadId
    overScheduler
      (do
        abortNow <- use schedulerShuttingDown
        if abortNow
          then return (Left SchedulerShuttingDown)
          else do
            pid     <- nextPid <<+= 1
            channel <- Mtl.lift newTQueue
            shutdownIndicator <- Mtl.lift (newTVar False)
            let pinfo = ProcessInfo pid channel shutdownIndicator
            threadIdTable . at pid .= Just myTId
            processTable . at pid .= Just pinfo
            return (Right pinfo)
      )
  destroyQueue lc pid psVar = do
    didWork <- Exc.try
      (overSchedulerIO
        psVar
        (do os <- processTable . at pid <<.= Nothing
            ot <- threadIdTable . at pid <<.= Nothing
            return (os, isJust os || isJust ot)))
    let getCause =
          Exc.try @Exc.SomeException
              (overSchedulerIO psVar (preuse (processTable . at pid)))
            >>= either
                  (return . (show pid ++) . show)
                  (return . (maybe (show pid) show))

    case didWork of
      Right (_pinfo, True) ->
        return ()
      Right (pinfo, False) ->
        logChannelPutIO lc ("queue already destroyed: " ++ show pinfo)
      Left (e :: Exc.SomeException) ->
        getCause
          >>= logChannelPutIO lc
          .   (("failed to destroy queue: " ++ show e ++ " ") ++)


overScheduler
  :: HasSchedulerIO r
  => Mtl.StateT Scheduler STM.STM (Either SchedulerError a)
  -> Eff r (Either SchedulerError a)
overScheduler stAction = do
  psVar <- getSchedulerTVar
  -- liftCatch (Left . LowLevelIOException) (overSchedulerIO psVar stAction)
  lift (overSchedulerIO psVar stAction)

overSchedulerIO
  :: STM.TVar Scheduler -> Mtl.StateT Scheduler STM.STM a -> IO a
overSchedulerIO psVar stAction =
  do STM.atomically
       (do ps <- STM.readTVar psVar
           (result, psModified) <- Mtl.runStateT stAction ps
           STM.writeTVar psVar psModified
           return result)

getSchedulerTVar :: HasSchedulerIO r => Eff r (TVar Scheduler)
getSchedulerTVar = fromSchedulerVar <$> ask

getSchedulerVar :: HasSchedulerIO r => Eff r SchedulerVar
getSchedulerVar = ask
