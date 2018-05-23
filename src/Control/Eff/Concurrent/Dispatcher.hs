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
{-# LANGUAGE GADTs #-}
module Control.Eff.Concurrent.Dispatcher
  ( ProcIO
  , ProcessException(..)
--  , runProcIO
  , runProcIOWithScheduler
  , SchedulerVar
  , withScheduler
  , HasScheduler
  , Scheduler
  -- , nextPid, processTable
  , ProcessInfo
  , processId
  -- , messageQ
  , getProcessInfo
  , getLogChannel
  , spawn
  , dispatchMessages
  -- , withMessageQueue
  )
where

import           GHC.Stack
import           Data.Maybe
import           Data.Kind
import qualified Control.Exception             as Exc
import           Control.Concurrent            as Concurrent
import           Control.Concurrent.STM        as STM
import           Control.Eff
import           Control.Eff.Concurrent.MessagePassing
import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Eff.Log
import           Control.Eff.Reader.Strict     as Reader
import           Control.Lens
import           Control.Monad                 (when, void)
import qualified Control.Monad.State           as Mtl
import           Control.Monad.Trans.Control
import           Data.Dynamic
import           Data.Typeable                  ( typeRep )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

-- * MessagePassing Scheduling

data ProcessInfo =
                 ProcessInfo { _processId       :: ProcessId
                             , _messageQ        :: STM.TQueue Dynamic
                             , _exitOnShutdown  :: Bool
                             }

makeLenses ''ProcessInfo

instance Show ProcessInfo where
  show p =
    "ProcessInfo: " ++ show (p ^. processId) ++ " trapExit: "
                      ++ show (not (p^.exitOnShutdown))

data Scheduler =
               Scheduler { _nextPid :: ProcessId
                         , _processTable :: Map ProcessId ProcessInfo
                         , _threadIdTable :: Map ProcessId ThreadId
                         , _schedulerShuttingDown :: Bool
                         , _logChannel :: LogChannel String
                         }

makeLenses ''Scheduler

newtype SchedulerVar = SchedulerVar { fromSchedulerVar :: STM.TVar Scheduler }
  deriving Typeable

data ProcessException =
  ProcessShutdown String ProcessId
  | ProcessException String ProcessId
  | SchedulerShuttingDown
  | LowLevelIOException Exc.SomeException
  deriving (Typeable, Show)

type family Members (es :: [* -> *])  (r :: [* -> *]) :: Constraint where
  Members '[] r = ()
  Members (e ': es) r = (Member e r, Members es r)

type HasScheduler r = ( HasCallStack
                      , SetMember Lift (Lift IO) r
                      , Member (Exc ProcessException) r
                      , Member (Reader SchedulerVar) r)

type ProcIO = '[ MessagePassing
               , Process
               , Exc ProcessException
               , Reader SchedulerVar
               , Logs String
               , Lift IO
               ]

instance MonadLog String (Eff ProcIO) where
  logMessageFree = logMessageFreeEff

runProcIOWithScheduler :: Eff ProcIO a
                       -> LogChannel String
                       -> IO (Either ProcessException a)
runProcIOWithScheduler e = -- TODO dispatchmessages should not allow unhandled (process-)exceptions
 withScheduler
   (runError
    (dispatchMessages
     (\cleanup ->
         do mt <- lift myThreadId
            mp <- self
            logMessage (show mp ++ " main process started in thread " ++ show mt)
            res <- catchError (Right <$> e) (return . Left)
            case res of
              Left ex ->
                 do logMessage (show mp ++ " main process exception: " ++
                               ((show :: ProcessException -> String) ex))
                    lift (runCleanUpAction cleanup)
                >> throwError ex
              Right rres ->
                do logMessage (show mp ++ " main process exited")
                   lift (runCleanUpAction cleanup)
                   return rres)))

runProcIO :: SchedulerVar
          -> (CleanUpAction -> Eff ProcIO a)
          -> IO (Either ProcessException a)
runProcIO s procAction =
  do l <- (view logChannel) <$> atomically (readTVar (fromSchedulerVar s))
     runLift
         (forwardLogsToChannel l
         (runReader
           (runError
             (dispatchMessages procAction))
           s))


getLogChannel :: HasScheduler r => Eff r (LogChannel String)
getLogChannel =
  do s <- getSchedulerTVar
     lift ((view logChannel) <$> atomically (readTVar s))

withScheduler :: Eff '[Reader SchedulerVar, Logs String, Lift IO] a
              -> LogChannel String
              -> IO a
withScheduler mainProcessAction logC =
  do myTId <- myThreadId
     Exc.bracket
       (newTVarIO (Scheduler 1 Map.empty Map.empty False logC))
       (tearDownScheduler myTId)
       (\sch ->
          runLift
            (forwardLogsToChannel logC
               (runReader mainProcessAction (SchedulerVar sch))))
  where
      tearDownScheduler myTId v =
        do sch <- (atomically
                   (do sch <- readTVar v
                       let sch' = sch & schedulerShuttingDown .~ True
                                      & processTable .~ Map.empty
                                      & threadIdTable .~ Map.empty
                       writeTVar v sch'
                       return sch))
           imapM_ (killProcThread (sch^.logChannel) myTId)
                  (sch ^. threadIdTable)
      killProcThread logC myTId pid tid =
        when (myTId /= tid)
             (logChannelPutIO logC ("killing thread of process " ++ show pid)
              >> killThread tid)

getProcessInfo :: HasScheduler r => ProcessId -> Eff r ProcessInfo
getProcessInfo pid = do
  res <- getProcessInfoSafe pid
  case res of
      Nothing ->
        throwError (ProcessShutdown "process table not found" pid)
      Just i ->
        return i

overProcessInfo
  :: HasScheduler r
  => ProcessId
  -> Mtl.StateT ProcessInfo STM.STM a
  -> Eff r a
overProcessInfo pid stAction =
  overScheduler
    (do res <- use (processTable . at pid )
        case res of
          Nothing ->
            return (Left (ProcessShutdown "process table not found" pid))
          Just pinfo ->
            do
            (x, pinfoOut) <- Mtl.lift (Mtl.runStateT stAction pinfo)
            processTable . at pid . _Just .= pinfoOut
            return (Right x))
    >>= liftEither

getProcessInfoSafe :: HasScheduler r => ProcessId -> Eff r (Maybe ProcessInfo)
getProcessInfoSafe pid = do
  p <- getScheduler
  return (p ^. processTable . at pid)

-- ** MessagePassing execution

spawn :: HasScheduler r => Eff ProcIO () -> Eff r ProcessId
spawn mfa = do
  schedulerVar <- ask
  pidVar       <- lift newEmptyTMVarIO
  cleanupVar   <- lift newEmptyTMVarIO
  lc           <- getLogChannel
  void
    (lift
    (Concurrent.forkFinally
       (runProcIO
        schedulerVar
        (\ cleanUpAction -> do
            lift (atomically (STM.putTMVar cleanupVar cleanUpAction))
            pid <- self
            lift (atomically (STM.putTMVar pidVar (Just pid)))
            catchError
              mfa
              (logMessage . ("process exception: " ++)
                . (show :: ProcessException -> String))))
      (\eres -> do
          mcleanup <- atomically (STM.tryTakeTMVar cleanupVar)
          void (atomically (tryPutTMVar pidVar Nothing))
          case mcleanup of
            Nothing -> return ()
            Just ca ->
              do runCleanUpAction ca
                 mt <- myThreadId
                 case eres of
                   Left se ->
                     logChannelPutIO lc ("thread " ++ show mt ++ " killed by exception: " ++ show se)
                   Right _ ->
                     logChannelPutIO lc ("thread " ++ show mt ++ " exited"))))
  mPid <- lift (atomically (STM.takeTMVar pidVar))
  maybe (throwError SchedulerShuttingDown) return mPid

newtype CleanUpAction = CleanUpAction { runCleanUpAction :: IO () }

dispatchMessages
  :: forall r a
   . (HasScheduler r, HasCallStack)
  => (CleanUpAction -> Eff (MessagePassing ': Process ': r) a)
  -> Eff r a
dispatchMessages processAction =
  withMessageQueue
    (\cleanUpAction pinfo ->
       handle_relay return (goProc (pinfo^.processId))
         (handle_relay return (go (pinfo^.processId)) (processAction cleanUpAction)))
 where
  go
    :: forall v
     . HasCallStack
    => ProcessId
    -> MessagePassing v
    -> (v -> Eff (Process ': r) a)
    -> Eff (Process ': r) a
  go _pid (SendMessage toPid reqIn) k = do
    psVar <- getSchedulerTVar
    lift
        (atomically
          (do
            p <- readTVar psVar
            let mto = p ^. processTable . at toPid
            case mto of
              Just toProc ->
                let dReq = toDyn reqIn
                in  do
                      writeTQueue (toProc ^. messageQ) dReq
                      return True
              Nothing -> return False
          )
        )
      >>= k
  go pid (ReceiveMessage onMsg) k = do
    (mDynMsg, isExitOnShutdown) <-
       (overProcessInfo pid
        (do mq <- use messageQ
            isExitOnShutdown <- use exitOnShutdown
            msg <- Mtl.lift (readTQueue mq)
            return (msg, isExitOnShutdown)))
    case fromDynamic mDynMsg of
      Just (Message req) ->
        let result = onMsg req in k (Message result)
      Just nix@Shutdown ->
        if isExitOnShutdown then
          let msg = "shutdown message received: " ++ show (typeRep nix)
          in throwError (ProcessShutdown msg pid)
        else
           k Shutdown
      nix@Nothing ->
        let msg = "unexpected message: "
                    ++ show mDynMsg
                    ++ " expected: "
                    ++ show (typeRep nix)
          in throwError (ProcessShutdown msg pid)
  goProc
    :: forall v x . HasCallStack
    => ProcessId
    -> Process v
    -> (v -> Eff r x)
    -> Eff r x
  goProc pid SelfPid k = k pid
  goProc pid (TrapExit s) k =
    overProcessInfo pid (exitOnShutdown <<.= (not s)) >>= k . not
  goProc pid (RaiseError msg) _k =
    throwError (ProcessException msg pid)

withMessageQueue :: HasScheduler r => (CleanUpAction -> ProcessInfo -> Eff r a) -> Eff r a
withMessageQueue m = do
  mpinfo <- createQueue
  lc <- getLogChannel
  case mpinfo of
    Just pinfo ->
      do cleanUpAction <-
           getSchedulerTVar >>= return . CleanUpAction . destroyQueue lc pinfo
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
                  let pinfo = ProcessInfo pid channel True
                  threadIdTable . at pid .= Just myTId
                  processTable . at pid .= Just pinfo
                  return (Just pinfo)
       )
  destroyQueue lc pinfo psVar =
    do didWork <- overSchedulerIO psVar
                 (do abortNow <- use schedulerShuttingDown
                     if abortNow
                       then
                         return False
                       else
                         do os <- processTable . at (pinfo ^. processId) <<.= Nothing
                            ot <- threadIdTable . at (pinfo ^. processId) <<.= Nothing
                            return (isJust os || isJust ot))
       case didWork of
         Right True ->
            logChannelPutIO lc ("destroying queue: " ++ show pinfo)
         Right False ->
            logChannelPutIO lc ("queue already destroyed: " ++ show pinfo)
         Left e ->
            logChannelPutIO lc ("failed to destroy queue: " ++ show pinfo ++ " " ++ show e)


overScheduler :: HasScheduler r => Mtl.StateT Scheduler STM.STM a -> Eff r a
overScheduler stAction = do
  psVar <- getSchedulerTVar
  lift (overSchedulerIO psVar stAction) >>= either (throwError . LowLevelIOException) return

overSchedulerIO :: STM.TVar Scheduler -> Mtl.StateT Scheduler STM.STM a -> IO (Either Exc.SomeException a)
overSchedulerIO psVar stAction =
  Exc.try
       (STM.atomically
          (do ps                   <- STM.readTVar psVar
              (result, psModified) <- Mtl.runStateT stAction ps
              STM.writeTVar psVar psModified
              return result))

getSchedulerTVar :: HasScheduler r => Eff r (TVar Scheduler)
getSchedulerTVar = fromSchedulerVar <$> ask

getScheduler :: HasScheduler r => Eff r Scheduler
getScheduler = do
  processesVar <- getSchedulerTVar
  lift (atomically (readTVar processesVar))
