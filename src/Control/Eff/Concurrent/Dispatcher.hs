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
  , runProcIO
  , runProcIOWithScheduler
  , SchedulerVar
  , withSchedulerVar
  , newSchedulerVar
  , HasProcesses
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
import           Control.Concurrent            as Concurrent
import           Control.Concurrent.STM        as STM
import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Eff.Concurrent.MessagePassing
import           Control.Eff.Reader.Strict     as Reader
import           Control.Lens
import           Control.Monad                 (void)
import qualified Control.Monad.State           as Mtl
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
                         }
                 deriving Show
makeLenses ''Scheduler

newtype SchedulerVar = SchedulerVar { fromSchedulerVar :: STM.TVar Scheduler }
  deriving Typeable

data ProcessException =
  ProcessShutdown String ProcessId
  | ProcessException String ProcessId
  deriving (Typeable, Show)

type family Members (es :: [* -> *])  (r :: [* -> *]) :: Constraint where
  Members '[] r = ()
  Members (e ': es) r = (Member e r, Members es r)

type HasProcesses r = ( HasCallStack
                      , SetMember Lift (Lift IO) r
                      , Member (Exc ProcessException) r
                      , Member (Reader SchedulerVar) r)

type ProcIO = '[ MessagePassing
               , Process
               , Exc ProcessException
               , Reader SchedulerVar
               , Lift IO
               ]

runProcIOWithScheduler :: Eff ProcIO a -> IO (Either ProcessException a)
runProcIOWithScheduler e =
  runLift newSchedulerVar >>= flip runProcIO e

runProcIO :: SchedulerVar -> Eff ProcIO a -> IO (Either ProcessException a)
runProcIO s = runLift . runError . flip runReader s . dispatchMessages

withSchedulerVar
  :: (Member (Exc ProcessException) r, HasCallStack)
  => SchedulerVar
  -> Eff (Reader SchedulerVar ':r) a
  -> Eff r a
withSchedulerVar =


newSchedulerVar
  :: (SetMember Lift (Lift IO) r, HasCallStack)
  => Eff r SchedulerVar
newSchedulerVar =
  SchedulerVar <$> lift (newTVarIO (Scheduler 1 Map.empty))

getProcessInfo :: HasProcesses r => ProcessId -> Eff r ProcessInfo
getProcessInfo pid = do
  res <- getProcessInfoSafe pid
  case res of
      Nothing ->
        throwError (ProcessShutdown "ProcessTable not found!" pid)
      Just i ->
        return i

overProcessInfo
  :: HasProcesses r
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

getProcessInfoSafe :: HasProcesses r => ProcessId -> Eff r (Maybe ProcessInfo)
getProcessInfoSafe pid = do
  p <- getScheduler
  return (p ^. processTable . at pid)

-- ** MessagePassing execution

spawn :: HasProcesses r => Eff ProcIO () -> Eff r ProcessId
spawn mfa = do
  schedulerVar <- ask
  pidVar    <- lift newEmptyTMVarIO
  _threadId <- lift
    (Concurrent.forkIO
      (printErrors =<<
       (runProcIO schedulerVar
        (do
            pid <- self
            lift (atomically (STM.putTMVar pidVar pid))
            catchError
              mfa
              (lift
               . putStrLn
                . ("Got Exception: " ++)
                . (show :: ProcessException -> String))
        ))))
  lift (atomically (STM.takeTMVar pidVar))
  where
    printErrors :: Show a => Either a b -> IO ()
    printErrors (Left e) = putStrLn (show e)
    printErrors _        = return ()

dispatchMessages
  :: forall r a
   . (HasProcesses r, HasCallStack)
  => Eff (MessagePassing ': Process ': r) a
  -> Eff r a
dispatchMessages processAction =
  withMessageQueue
    (\pinfo ->
       handle_relay return (goProc (pinfo^.processId))
       (handle_relay return (go (pinfo^.processId)) processAction))
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

withMessageQueue :: HasProcesses r => (ProcessInfo -> Eff r a) -> Eff r a
withMessageQueue m = do
  pinfo <- createQueue
  res   <- catchError (Right <$> m pinfo)
                     (return . (Left :: ProcessException -> Either ProcessException a))
  destroyQueue pinfo
  either throwError return res
 where
  createQueue = overScheduler
    (do
      pid     <- nextPid <<+= 1
      channel <- Mtl.lift newTQueue
      let pinfo = ProcessInfo pid channel True
      processTable . at pid .= Just pinfo
      return pinfo
    )
  destroyQueue pinfo =
    do lift (putStrLn ("detroying queue: " ++ show pinfo))
       overScheduler (processTable . at (pinfo ^. processId) .= Nothing)


overScheduler :: HasProcesses r => Mtl.StateT Scheduler STM.STM a -> Eff r a
overScheduler stAction = do
  psVar <- getSchedulerTVar
  lift
    (STM.atomically
      (do
        ps                   <- STM.readTVar psVar
        (result, psModified) <- Mtl.runStateT stAction ps
        STM.writeTVar psVar psModified
        return result
      )
    )

getSchedulerTVar :: HasProcesses r => Eff r (TVar Scheduler)
getSchedulerTVar = fromSchedulerVar <$> ask

getScheduler :: HasProcesses r => Eff r Scheduler
getScheduler = do
  processesVar <- getSchedulerTVar
  lift (atomically (readTVar processesVar))
