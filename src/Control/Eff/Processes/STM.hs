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
module Control.Eff.Processes.STM
  ( ProcessIOEffects
  , HasScheduler
  , Scheduler
  -- , nextPid, processTable
  , ProcessInfo
  , processId
  -- , messageQ
  , runScheduler
  , getProcessInfo
  , spawn
  , dispatchMessages
  -- , withMessageQueue
  ) where

import           GHC.Stack
import           Data.Kind
import           Control.Concurrent            as Concurrent
import           Control.Concurrent.STM        as STM
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Processes
import           Control.Eff.Reader.Strict     as Reader
import           Control.Lens
import qualified Control.Monad.State           as Mtl
import           Data.Dynamic
import           Data.Typeable                  ( typeRep )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

-- * Process Scheduling

data ProcessInfo =
                 ProcessInfo { _processId       :: ProcessId
                             , _messageQ        :: STM.TQueue Dynamic
                             }

makeLenses ''ProcessInfo

instance Show ProcessInfo where
  show p =
    "Process: " ++ show (p ^. processId)

data Scheduler =
               Scheduler { _nextPid :: ProcessId
                         , _processTable :: Map ProcessId ProcessInfo
                         }
                 deriving Show
makeLenses ''Scheduler

type family Members (es :: [* -> *])  (r :: [* -> *]) :: Constraint where
  Members '[] r = ()
  Members (e ': es) r = (Member e r, Members es r)

type HasScheduler r = ( HasCallStack
                      , SetMember Lift (Lift IO) r
                      , Member (Reader (STM.TVar Scheduler)) r)

type ProcessIOEffects = '[Process, Reader (STM.TVar Scheduler), Lift IO]

runScheduler
  :: (SetMember Lift (Lift IO) r, HasCallStack)
  => Eff (Reader (STM.TVar Scheduler) ': r) ()
  -> Eff r ()
runScheduler e = do
  v <- lift (newTVarIO (Scheduler 1 Map.empty))
  runReader e v

getProcessInfo :: HasScheduler r => ProcessId -> Eff r (Maybe ProcessInfo)
getProcessInfo pid = do
  p <- getScheduler
  return (p ^. processTable . at pid)

-- ** Process execution

spawn :: HasScheduler r => Eff ProcessIOEffects () -> Eff r ProcessId
spawn mfa = do
  processes <- ask
  pidVar    <- lift newEmptyTMVarIO
  _threadId <- lift
    (Concurrent.forkIO
      (runLift
        (runReader
         (dispatchMessages
           (do pid <- self
               lift (atomically (STM.putTMVar pidVar pid))
               mfa)
         ) processes))
    )
  lift (atomically (STM.takeTMVar pidVar))

dispatchMessages
  :: forall r a . (HasScheduler r, HasCallStack)
  => Eff (Process ': r) a
  -> Eff r a
dispatchMessages processAction =
  withMessageQueue
    (\pinfo ->
       handle_relay return (go pinfo) processAction)
 where
  go :: forall v . HasCallStack => ProcessInfo -> Process v -> (v -> Eff r a) -> Eff r a
  go (ProcessInfo selfPidInt _) SelfPid k = k selfPidInt
  go _ (SendMessage toPid reqIn) k = do
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
  go (ProcessInfo selfPidInt channel) (ReceiveMessage onMsg) k = do
    mDynMsg <- lift (atomically (readTQueue channel))
    case fromDynamic mDynMsg of
      Just req -> let result = onMsg req in k (Just result)
      nix      -> do
        lift
          (putStrLn
            (  show selfPidInt
            ++ " got unexpected msg: "
            ++ show mDynMsg
            ++ " expected: "
            ++ show (typeRep nix)
            )
          )
        k Nothing

withMessageQueue
  :: HasScheduler r
  => (ProcessInfo -> Eff r a)
  -> Eff r a
withMessageQueue m =
  do pinfo <- createQueue
     res <- m pinfo
     destroyQueue pinfo
     return res
  where
    createQueue =
      overScheduler
       (do
         pid     <- nextPid <<+= 1
         channel <- Mtl.lift newTQueue
         let pinfo = ProcessInfo pid channel
         processTable . at pid .= Just pinfo
         return pinfo)
    destroyQueue pinfo =
      overScheduler
         (processTable . at (pinfo^.processId) .= Nothing)


overScheduler :: HasScheduler r => Mtl.StateT Scheduler STM.STM a -> Eff r a
overScheduler stAction = do
  psVar <- ask
  lift
    (STM.atomically
      (do
        ps                   <- STM.readTVar psVar
        (result, psModified) <- Mtl.runStateT stAction ps
        STM.writeTVar psVar psModified
        return result
      )
    )

getSchedulerTVar :: HasScheduler r => Eff r (TVar Scheduler)
getSchedulerTVar = ask

getScheduler :: HasScheduler r => Eff r Scheduler
getScheduler = do
  processesVar <- getSchedulerTVar
  lift (atomically (readTVar processesVar))
