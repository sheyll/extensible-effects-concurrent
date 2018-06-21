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
  , SchedulerError(..)
  , SchedulerIO
  , forkIoScheduler
  , HasSchedulerIO
  )
where

import           GHC.Stack
import           Data.Maybe
import           Data.Kind ()
import qualified Control.Exception             as Exc
import           Control.Concurrent            as Concurrent
import           Control.Concurrent.STM        as STM
import           Control.Eff
import           Control.Eff.Concurrent.Process
import           Control.Eff.ExceptionExtra
import           Control.Eff.Lift
import           Control.Eff.Log
import           Control.Eff.Reader.Strict     as Reader
import           Control.Lens
import           Control.Monad                  ( when
                                                , void
                                                )
import qualified Control.Monad.State           as Mtl
import           Data.Dynamic
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

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
  | LowLevelIOException Exc.SomeException
    -- ^ 'Control.Exception.SomeException' was caught while scheduleing
    -- messages.
  | ProcessRaisedError String ProcessId
    -- ^ A process called 'raiseError'.
  | ProcessExitError String ProcessId
    -- ^ A process called 'exitWithError'.
  | ProcessShuttingDown ProcessId
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
      scheduleProcessWithShutdownAction schedulerStateVar
      $ \cleanup ->
          do mt <- lift myThreadId
             mp <- self forkIoScheduler
             logMsg (show mp ++ " main process started in thread " ++ show mt)
             e
             logMsg (show mp ++ " main process exited")
             lift (runCleanUpAction cleanup)
             return (Right ())


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
              return sch
            )
          )
        logChannelPutIO logC (show myTId ++ " killing threads: " ++
                                   show (sch ^.. threadIdTable. traversed))
        imapM_ (killProcThread myTId) (sch ^. threadIdTable)
        atomically
          (do
              scheduler <- readTVar v
              let allThreadsDead = scheduler^.threadIdTable.to Map.null
                                   && scheduler^.processTable.to Map.null
              STM.check allThreadsDead)
        logChannelPutIO logC "all threads dead"


      killProcThread myTId pid tid = when
        (myTId /= tid)
        (  logChannelPutIO logC ("killing thread " ++ show pid)
        >> killThread tid
        )

-- | Start the message passing concurrency system then execute a 'Process' on
-- top of 'SchedulerIO' effect. All logging is sent to standard output.
defaultMain :: Eff (ConsProcess SchedulerIO) () -> IO ()
defaultMain c =
  runLoggingT
    (logChannelBracket
      (Just "~~~~~~ main process started")
      (Just "====== main process exited")
      (schedule c))
    (print :: String -> IO ())

scheduleProcessWithShutdownAction
  :: SchedulerVar
  -> (ShutdownAction -> Eff (ConsProcess SchedulerIO) (Either SchedulerError ()))
  -> IO (Either SchedulerError ())
scheduleProcessWithShutdownAction s mfa = do
  pidVar <- newEmptyTMVarIO
  cleanupVar <- newEmptyTMVarIO
  l <- (view logChannel) <$> atomically (readTVar (fromSchedulerVar s))
  Exc.try
    (runLift
     (forwardLogsToChannel l
      (runReader
       (scheduleProcessWithCleanup (procAction pidVar cleanupVar))
       s)))

  where
    procAction pidVar cleanupVar cleanUpAction =
       do
            lift (atomically (STM.putTMVar cleanupVar cleanUpAction))
            pid <- self forkIoScheduler
            lift (atomically (STM.putTMVar pidVar (Just pid)))
            Right <$> mfa (ShutdownAction (Exc.throw . fmap (const (ProcessShuttingDown pid))))



getLogChannel :: HasSchedulerIO r => Eff r (LogChannel String)
getLogChannel = do
  s <- getSchedulerTVar
  lift ((view logChannel) <$> atomically (readTVar s))

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
          -> Eff SchedulerIO (Either SchedulerError ProcessId)
spawnImpl mfa = do
  schedulerVar <- ask
  pidVar       <- lift newEmptyTMVarIO
  cleanupVar   <- lift newEmptyTMVarIO
  lc           <- getLogChannel
  void
    (lift
      (Concurrent.forkFinally
        (scheduleProcessWithShutdownAction
          schedulerVar
          (\cleanUpAction -> do
            lift (atomically (STM.putTMVar cleanupVar cleanUpAction))
            pid <- self forkIoScheduler
            lift (atomically (STM.putTMVar pidVar (Just pid)))
            Right <$> mfa
          )
        )
        (\eres -> do
          mcleanup <- atomically (STM.tryTakeTMVar cleanupVar)
          void (atomically (tryPutTMVar pidVar Nothing))
          case mcleanup of
            Nothing -> return ()
            Just ca -> do
              runCleanUpAction ca
              mt <- myThreadId
              case eres of
                Left se -> logChannelPutIO
                  lc
                  ("thread " ++ show mt ++ " killed by exception: " ++ show se)
                Right _ ->
                  logChannelPutIO lc ("thread " ++ show mt ++ " exited")
        )
      )
    )
  mPid <- lift (atomically (STM.takeTMVar pidVar))
  return (maybe (Left SchedulerShuttingDown) Right mPid)

newtype CleanUpAction = CleanUpAction { runCleanUpAction :: IO () }

scheduleProcessWithCleanup
  :: HasCallStack
  => (CleanUpAction -> Eff (ConsProcess SchedulerIO) (Either SchedulerError ()))
  -> Eff SchedulerIO (Either SchedulerError ())
scheduleProcessWithCleanup processAction =
  withMessageQueue
  (\cleanUpAction pinfo ->
     do res <- handle_relay
              return
              (go (pinfo ^. processId))
              (processAction cleanUpAction)
        either (\e ->
                   logMsg (show (pinfo^.processId)
                            ++ " exited "
                            ++ show e))
               (const (return ()))
               res
        return res)
  -- lift (runCleanUpAction cleanUpAction)

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
         liftCatch (Left . LowLevelIOException)
         (atomically
           (do p <- readTVar psVar
               let mPinfo = p ^. processTable . at pid
               case mPinfo of
                 Just pinfo ->
                   do wasRequested <- readTVar (pinfo ^. shutdownRequested)
                      -- reset the shutdwown request flag, in case the shutdown
                      -- is prevented by the process
                      writeTVar (pinfo ^. shutdownRequested) False
                      return (Right wasRequested)
                 Nothing ->
                   return (Left (ProcessNotFound pid))))
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
    do resumeRes <- catchError
         (do psVar <- getSchedulerTVar
             res <- liftRethrow LowLevelIOException
                     (atomically
                        (do p <- readTVar psVar
                            let mto = p ^. processTable . at toPid
                            case mto of
                              Just toProc ->
                                do writeTQueue (toProc ^. messageQ) (Just reqIn)
                                   return True
                              Nothing ->
                                return False))
             return (ResumeWith res))
         (return . OnError . show @SchedulerError)
       k resumeRes

  go pid (SendShutdown toPid) k =
    shutdownOrGo pid k $
    do resumeRes <-
         catchError
         (do psVar <- getSchedulerTVar
             res <- liftRethrow LowLevelIOException
                   (atomically
                     (do p <- readTVar psVar
                         let mto = p ^. processTable . at toPid
                         case mto of
                           Just toProc ->
                             do writeTVar (toProc ^. shutdownRequested) True
                                writeTQueue (toProc ^. messageQ) Nothing
                                return True
                           Nothing ->
                             return False))
             return
               (if toPid == pid
                 then ShutdownRequested
                 else ResumeWith res))
         (return . OnError . show @SchedulerError)
       k resumeRes

  go pid (Spawn child) k =
    shutdownOrGo pid k $
    do res <- spawnImpl child
       k (either (OnError . show @SchedulerError) ResumeWith res)

  go pid ReceiveMessage k =
    shutdownOrGo pid k $
      do emdynMsg <- overProcessInfo pid
                        (do mq <- use messageQ
                            Mtl.lift (readTQueue mq))

         k $ either
               (OnError . show @SchedulerError)
               (maybe RetryLastAction ResumeWith)
               emdynMsg

  go pid SelfPid k =
    shutdownOrGo pid k $
    k (ResumeWith pid)

  go pid Shutdown _k = do
    logMsg (show pid ++ " process exited normally.")
    throwError (ProcessShuttingDown pid)

  go pid (ExitWithError msg) _k = do
    logMsg (show pid ++ " process exited with error: " ++ msg)
    throwError (ProcessExitError msg pid)

  go pid (RaiseError msg) _k =
    do logMsg (show pid ++ " error raised: " ++ msg)
       throwError (ProcessRaisedError msg pid)

data ShutdownAction =
  ShutdownAction (forall a . Either SchedulerError () -> IO a)

invokeShutdownAction :: ( HasCallStack
                       , SetMember Lift (Lift IO) r
                       , Member (Logs String) r)
                     => ShutdownAction -> Either SchedulerError () -> Eff r a
invokeShutdownAction (ShutdownAction a) res = lift (a res)

withMessageQueue
  :: HasSchedulerIO r
  => (ShutdownAction -> ProcessInfo -> Eff r a)
  -> Eff r (Either SchedulerError a)
withMessageQueue m = do
  mpinfo <- createQueue
  lc     <- getLogChannel
  case mpinfo of
    Just pinfo -> do
      cleanUpAction <-
        getSchedulerTVar >>= return . CleanUpAction . destroyQueue
          lc
          (pinfo ^. processId)
      m cleanUpAction pinfo
    Nothing ->
      throwError SchedulerShuttingDown
 where
  createQueue = do
    myTId <- lift myThreadId
    overScheduler
      (do
        abortNow <- use schedulerShuttingDown
        if abortNow
          then return Nothing
          else do
            pid     <- nextPid <<+= 1
            channel <- Mtl.lift newTQueue
            shutdownIndicator <- Mtl.lift (newTVar False)
            let pinfo = ProcessInfo pid channel shutdownIndicator
            threadIdTable . at pid .= Just myTId
            processTable . at pid .= Just pinfo
            return (Just pinfo)
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
      Right (pinfo, True) ->
        logChannelPutIO lc ("destroying queue: " ++ show pinfo)
      Right (pinfo, False) ->
        logChannelPutIO lc ("queue already destroyed: " ++ show pinfo)
      Left (e :: Exc.SomeException) ->
        getCause
          >>= logChannelPutIO lc
          .   (("failed to destroy queue: " ++ show e ++ " ") ++)


overScheduler
  :: HasSchedulerIO r => Mtl.StateT Scheduler STM.STM a -> Eff r a
overScheduler stAction = do
  psVar <- getSchedulerTVar
  liftRethrow LowLevelIOException (overSchedulerIO psVar stAction)

overSchedulerIO
  :: STM.TVar Scheduler -> Mtl.StateT Scheduler STM.STM a -> IO a
overSchedulerIO psVar stAction = STM.atomically
  (do
    ps                   <- STM.readTVar psVar
    (result, psModified) <- Mtl.runStateT stAction ps
    STM.writeTVar psVar psModified
    return result
  )

getSchedulerTVar :: HasSchedulerIO r => Eff r (TVar Scheduler)
getSchedulerTVar = fromSchedulerVar <$> ask
