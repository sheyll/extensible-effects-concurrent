{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , withSchedulerVar
  , HasScheduler
  , Scheduler
  -- , nextPid, processTable
  , ProcessInfo
  , processId
  -- , messageQ
  , getProcessInfo
  , spawn
  , dispatchMessages
  -- , withMessageQueue
  )
where

import           GHC.Stack
import           Data.Kind
import           Control.Exception             (bracket)
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
import           Control.Monad.IO.Class
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

runProcIOWithScheduler :: Eff ProcIO a
                       -> LoggingT String IO (Either ProcessException a)
runProcIOWithScheduler  e = -- TODO dispatchmessages should not allow unhandled (process-)exceptions
 withSchedulerVar
   (runError
    (dispatchMessages
     (\cleanup ->
         do mt <- lift myThreadId
            mp <- self
            logMessage (show mp ++ " main process started in thread " ++ show mt)
            res <- catchError (Right <$> e) (return . Left)
            case res of
              Left e ->
                 do logMessage (show mp ++ " main process exception: " ++
                               ((show :: ProcessException -> String) e))
                    lift (runCleanUpAction cleanup)
                >> throwError e
              Right res ->
                do logMessage (show mp ++ " main process exited")
                   lift (runCleanUpAction cleanup)
                   return res)))

runProcIO :: SchedulerVar
          -> (CleanUpAction -> Eff ProcIO a)
          -> IO (Either ProcessException a)
runProcIO s procAction =
  do l <- (view logChannel) <$> atomically (readTVar (fromSchedulerVar s))
     runLift
       (forwardLogsToChannel
         l
         (runReader
           (runError
             (dispatchMessages procAction))
           s))

withSchedulerVar :: Eff '[Reader SchedulerVar, Logs String, Lift IO] a
                 -> LoggingT String IO a
withSchedulerVar mainProcessAction =
  control
  (\ runInIO ->
      do myTId <- myThreadId
         let logHandler = void . runInIO . logMessage
         bracket
           (forkLogChannel
             logHandler
             (Just "scheduler log channel starting"))
           (joinLogChannel
            (Just "scheduler log channel shutting down"))
           (\logC ->
              bracket
              (newTVarIO (Scheduler 1 Map.empty Map.empty False logC))
              (tearDownScheduer myTId)
              (\sch ->
                 runLift
                   (forwardLogsToChannel logC
                    (runReader mainProcessAction (SchedulerVar sch))))))
  where
      tearDownScheduer myTId v =
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
             (logChannelPutIO logC ("killing thread of " ++ show pid)
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
            return (Left (ProcessShutdown "ProcessTable not found!" pid))
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
  lift
    (Concurrent.forkFinally
      (printErrors =<<
       (runProcIO
        schedulerVar
        (\ cleanUpAction -> do
            lift (atomically (STM.putTMVar cleanupVar cleanUpAction))
            pid <- self
            lift (atomically (STM.putTMVar pidVar (Just pid)))
            catchError
              mfa
              (logMessage . ("exception: " ++)
                . (show :: ProcessException -> String)))))
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
                     cleanUpLogger ca ("thread " ++ show mt ++ " killed by exception: " ++ show se)
                   Right _ ->
                     cleanUpLogger ca ("thread " ++ show mt ++ " exited")))
  mPid <- lift (atomically (STM.takeTMVar pidVar))
  maybe (throwError SchedulerShuttingDown) return mPid

  where
    printErrors :: Show a => Either a b -> IO ()
    printErrors (Left e) = putStrLn ("TODO fix logging!! " ++ show e)
    printErrors _        = return ()

data CleanUpAction = CleanUpAction {  cleanUpLogger :: String -> IO (), runCleanUpAction :: IO ()    }

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
    (mDynMsg, isExitOnShutdown) <- overProcessInfo pid
        (do mq <- use messageQ
            isExitOnShutdown <- use exitOnShutdown
            msg <- Mtl.lift (readTQueue mq)
            return (msg, isExitOnShutdown))
    case fromDynamic mDynMsg of
      Just (Message req) ->
        let result = onMsg req in k (Message result)
      Just nix@Shutdown ->
        if isExitOnShutdown then
          let msg = "Shutdown Message received: " ++ show (typeRep nix)
          in throwError (ProcessShutdown msg pid)
        else
           k Shutdown
      nix@Nothing ->
        let msg = ( "Unexpected message: "
                    ++ show mDynMsg
                    ++ " expected: "
                    ++ show (typeRep nix)
                  )
          in throwError (ProcessShutdown msg pid)
  goProc
    :: forall v x
     . HasCallStack
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
  case mpinfo of
    Just pinfo ->
      do cleanUpAction <-
           getSchedulerTVar >>= return . CleanUpAction (putStrLn . (++ " !! TODO fix log")) . destroyQueue pinfo
         res <- catchError (Right <$> m cleanUpAction pinfo)
                   (return . (Left :: ProcessException -> Either ProcessException a))
         either throwError return res
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
  destroyQueue pinfo psVar =
    do didWork <- overSchedulerIO psVar
                 (do abortNow <- use schedulerShuttingDown
                     if abortNow
                       then
                         return False
                       else
                         do processTable . at (pinfo ^. processId) .= Nothing
                            threadIdTable . at (pinfo ^. processId) .= Nothing
                            return True)
       when didWork (putStrLn ("detroying queue: " ++ show pinfo))


overScheduler :: HasScheduler r => Mtl.StateT Scheduler STM.STM a -> Eff r a
overScheduler stAction = do
  psVar <- getSchedulerTVar
  lift (overSchedulerIO psVar stAction)

overSchedulerIO :: STM.TVar Scheduler -> Mtl.StateT Scheduler STM.STM a -> IO a
overSchedulerIO psVar stAction =
  STM.atomically
  (do
      ps                   <- STM.readTVar psVar
      (result, psModified) <- Mtl.runStateT stAction ps
      STM.writeTVar psVar psModified
      return result)

getSchedulerTVar :: HasScheduler r => Eff r (TVar Scheduler)
getSchedulerTVar = fromSchedulerVar <$> ask

getScheduler :: HasScheduler r => Eff r Scheduler
getScheduler = do
  processesVar <- getSchedulerTVar
  lift (atomically (readTVar processesVar))
