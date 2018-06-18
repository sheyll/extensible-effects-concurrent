-- | Implement Erlang style message passing concurrency.
--
-- This handles the 'MessagePassing' and 'Process' effects, using
-- 'STM.TQueue's and 'forkIO'.
--
-- This aims to be a pragmatic implementation, so even logging is
-- supported.
--
-- At the core is a /main process/ that enters 'runMainProcess'
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
module Control.Eff.Concurrent.Dispatcher
  ( runMainProcess
  , defaultMain
  , spawn
  , DispatcherError(..)
  , DispatcherIO
  , HasDispatcherIO
  , ProcIO
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
import           Data.Typeable                  ( typeRep )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

-- | Information about a process, needed to implement 'MessagePassing' and
-- 'Process' handlers. The message queue is backed by a 'STM.TQueue'.
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

-- | Contains all 'ProcessInfo' elements, as well as the state needed to
-- implement inter process communication. It contains also a 'LogChannel' to
-- which the logs of all processes are forwarded to.
data Dispatcher =
               Dispatcher { _nextPid :: ProcessId
                          , _processTable :: Map ProcessId ProcessInfo
                          , _threadIdTable :: Map ProcessId ThreadId
                          , _schedulerShuttingDown :: Bool
                          , _logChannel :: LogChannel String
                          }

makeLenses ''Dispatcher

-- | A newtype wrapper around an 'STM.TVar' holding a 'Dispatcher' state.
-- This is needed by 'spawn' and provided by 'runDispatcher'.
newtype DispatcherVar = DispatcherVar { fromDispatcherVar :: STM.TVar Dispatcher }
  deriving Typeable

-- | A sum-type with errors that can occur when dispatching messages.
data DispatcherError =
   UnhandledMessageReceived Dynamic ProcessId
   -- ^ A process message queue contained a bad message and the 'Dynamic' value
   -- could not be converted to the expected value using 'fromDynamic'.
  | ProcessNotFound ProcessId
    -- ^ No 'ProcessInfo' was found for a 'ProcessId' during internal
    -- processing. NOTE: This is **ONLY** caused by internal errors, probably by
    -- an incorrect 'MessagePassing' handler in this module. **Sending a message
    -- to a process ALWAYS succeeds!** Even if the process does not exist.
  | ProcessException String ProcessId
    -- ^ A process called 'raiseError'.
  | DispatcherShuttingDown
    -- ^ An action was not performed while the dispatcher was exiting.
  | LowLevelIOException Exc.SomeException
    -- ^ 'Control.Exception.SomeException' was caught while dispatching
    -- messages.
  deriving (Typeable, Show)

instance Exc.Exception DispatcherError

-- | An alias for the constraints for the effects essential to this dispatcher
-- implementation, i.e. these effects allow 'spawn'ing new 'Process'es.
-- @see DispatcherIO
type HasDispatcherIO r = ( HasCallStack
                        , SetMember Lift (Lift IO) r
                        , Member (Exc DispatcherError) r
                        , Member (Logs String) r
                        , Member (Reader DispatcherVar) r)

-- | The concrete list of 'Eff'ects for this scheduler implementation.
-- @see HasDispatcherIO
type DispatcherIO =
              '[ Exc DispatcherError
               , Reader DispatcherVar
               , Logs String
               , Lift IO
               ]

-- | The concrete list of 'Eff'ects that provide 'MessagePassing' and
-- 'Process'es ontop of 'DispatcherIO'
type ProcIO = ConsProcIO DispatcherIO

-- | /Cons/ 'ProcIO' onto a list of effects.
type ConsProcIO r = MessagePassing ': Process ': r

instance MonadLog String (Eff ProcIO) where
  logMessageFree = logMessageFreeEff

-- | This is the main entry point to running a message passing concurrency
-- application. This function takes a 'ProcIO' effect and a 'LogChannel' for
-- concurrent logging.
runMainProcess :: Eff ProcIO a -> LogChannel String -> IO a
runMainProcess e logC = withDispatcher
  (dispatchMessages
    (\cleanup -> do
      mt <- lift myThreadId
      mp <- self
      logMessage (show mp ++ " main process started in thread " ++ show mt)
      res <- try e
      case res of
        Left ex ->
          do
              logMessage
                (  show mp
                ++ " main process exception: "
                ++ ((show :: DispatcherError -> String) ex)
                )
              lift (runCleanUpAction cleanup)
            >> throwError ex
        Right rres -> do
          logMessage (show mp ++ " main process exited")
          lift (runCleanUpAction cleanup)
          return rres
    )
  )
  where
    withDispatcher :: Eff DispatcherIO a -> IO a
    withDispatcher mainProcessAction = do
      myTId <- myThreadId
      Exc.bracket
        (newTVarIO (Dispatcher myPid Map.empty Map.empty False logC))
        (tearDownDispatcher myTId)
        (\sch -> runLift
          (forwardLogsToChannel
            logC
            (runReader (runErrorRethrowIO mainProcessAction) (DispatcherVar sch))
          )
        )
     where
      myPid = 1
      tearDownDispatcher myTId v = do
        logChannelPutIO logC (show myTId ++" begin dispatcher tear down")
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
              dispatcher <- readTVar v
              let allThreadsDead = dispatcher^.threadIdTable.to Map.null
                                   && dispatcher^.processTable.to Map.null
              STM.check allThreadsDead)
        logChannelPutIO logC "all threads dead"


      killProcThread myTId pid tid = when
        (myTId /= tid)
        (  logChannelPutIO logC ("killing thread " ++ show pid)
        >> killThread tid
        )


-- | Start the message passing concurrency system then execute a 'ProcIO' effect.
-- All logging is sent to standard output.
defaultMain :: Eff ProcIO a -> IO a
defaultMain c =
  runLoggingT
    (logChannelBracket
      (Just "~~~~~~ main process started")
      (Just "====== main process exited")
      (runMainProcess c))
    (print :: String -> IO ())


runChildProcess
  :: DispatcherVar
  -> (CleanUpAction -> Eff ProcIO a)
  -> IO (Either DispatcherError a)
runChildProcess s procAction = do
  l <- (view logChannel) <$> atomically (readTVar (fromDispatcherVar s))
  runLift
    (forwardLogsToChannel
      l
      (runReader (runError (dispatchMessages procAction)) s)
    )

getLogChannel :: HasDispatcherIO r => Eff r (LogChannel String)
getLogChannel = do
  s <- getDispatcherTVar
  lift ((view logChannel) <$> atomically (readTVar s))


overProcessInfo
  :: HasDispatcherIO r
  => ProcessId
  -> Mtl.StateT ProcessInfo STM.STM a
  -> Eff r a
overProcessInfo pid stAction = liftEither =<< overDispatcher
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

spawn :: HasDispatcherIO r => Eff ProcIO () -> Eff r ProcessId
spawn mfa = do
  schedulerVar <- ask
  pidVar       <- lift newEmptyTMVarIO
  cleanupVar   <- lift newEmptyTMVarIO
  lc           <- getLogChannel
  void
    (lift
      (Concurrent.forkFinally
        (runChildProcess
          schedulerVar
          (\cleanUpAction -> do
            lift (atomically (STM.putTMVar cleanupVar cleanUpAction))
            pid <- self
            lift (atomically (STM.putTMVar pidVar (Just pid)))
            catchError
              mfa
              ( logMessage
              . ("process exception: " ++)
              . (show :: DispatcherError -> String)
              )
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
  maybe (throwError DispatcherShuttingDown) return mPid

newtype CleanUpAction = CleanUpAction { runCleanUpAction :: IO () }

dispatchMessages
  :: forall r a
   . (HasDispatcherIO r, HasCallStack)
  => (CleanUpAction -> Eff (ConsProcIO r) a)
  -> Eff r a
dispatchMessages processAction = withMessageQueue
  (\cleanUpAction pinfo ->
     try
     (handle_relay
      return
      (goProc (pinfo ^. processId))
       (handle_relay return (go (pinfo ^. processId))
        (processAction cleanUpAction)
       ))
     >>=
     either
     (\(e :: DispatcherError) ->
        do logMsg (show (pinfo^.processId)
                    ++ " cleanup on exception: "
                    ++ show e)
           lift (runCleanUpAction cleanUpAction)
           throwError e)
     return
  )
 where
  go
    :: forall v
     . HasCallStack
    => ProcessId
    -> MessagePassing v
    -> (v -> Eff (Process ': r) a)
    -> Eff (Process ': r) a
  go _pid (SendMessage toPid reqIn) k = do
    psVar <- getDispatcherTVar
    liftRethrow
        LowLevelIOException
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
    catchError
      (do
        mDynMsg <- overProcessInfo pid
          (do
            mq <- use messageQ
            Mtl.lift (readTQueue mq))
        case onMsg mDynMsg of
          Just result -> k (Message result)
          nix@Nothing ->
            let
              msg = "unexpected message: "
                  ++ show mDynMsg
                  ++ " expected: "
                  ++ show (typeRep nix)
            in do isExitOnShutdown <- overProcessInfo pid (use exitOnShutdown)
                  if isExitOnShutdown
                    then throwError (UnhandledMessageReceived mDynMsg pid)
                    else k (ProcessControlMessage msg)
      )
      (\(se :: DispatcherError) -> do
        isExitOnShutdown <- overProcessInfo pid (use exitOnShutdown)
        if isExitOnShutdown
          then throwError se
          else k (ProcessControlMessage (show se))
      )

  goProc
    :: forall v x
     . HasCallStack
    => ProcessId
    -> Process v
    -> (v -> Eff r x)
    -> Eff r x
  goProc pid SelfPid k = k pid
  goProc pid (TrapExit s) k =
    overProcessInfo pid (exitOnShutdown .= (not s)) >>= k
  goProc pid GetTrapExit k =
    overProcessInfo pid (use exitOnShutdown) >>= k . not
  goProc pid (RaiseError msg) _k = do
    logMsg (show pid ++ " error raised: " ++ msg)
    throwError (ProcessException msg pid)

withMessageQueue
  :: HasDispatcherIO r => (CleanUpAction -> ProcessInfo -> Eff r a) -> Eff r a
withMessageQueue m = do
  mpinfo <- createQueue
  lc     <- getLogChannel
  case mpinfo of
    Just pinfo -> do
      cleanUpAction <-
        getDispatcherTVar >>= return . CleanUpAction . destroyQueue
          lc
          (pinfo ^. processId)
      m cleanUpAction pinfo
    Nothing -> throwError DispatcherShuttingDown
 where
  createQueue = do
    myTId <- lift myThreadId
    overDispatcher
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
  destroyQueue lc pid psVar = do
    didWork <- Exc.try
      (overDispatcherIO
        psVar
        (do
          abortNow <- use schedulerShuttingDown
          if abortNow
            then return (Nothing, False)
            else do
              os <- processTable . at pid <<.= Nothing
              ot <- threadIdTable . at pid <<.= Nothing
              return (os, isJust os || isJust ot)
        )
      )
    let getCause =
          Exc.try @Exc.SomeException
              (overDispatcherIO psVar (preuse (processTable . at pid)))
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


overDispatcher
  :: HasDispatcherIO r => Mtl.StateT Dispatcher STM.STM a -> Eff r a
overDispatcher stAction = do
  psVar <- getDispatcherTVar
  liftRethrow LowLevelIOException (overDispatcherIO psVar stAction)

overDispatcherIO
  :: STM.TVar Dispatcher -> Mtl.StateT Dispatcher STM.STM a -> IO a
overDispatcherIO psVar stAction = STM.atomically
  (do
    ps                   <- STM.readTVar psVar
    (result, psModified) <- Mtl.runStateT stAction ps
    STM.writeTVar psVar psModified
    return result
  )

getDispatcherTVar :: HasDispatcherIO r => Eff r (TVar Dispatcher)
getDispatcherTVar = fromDispatcherVar <$> ask

getDispatcher :: HasDispatcherIO r => Eff r Dispatcher
getDispatcher = do
  processesVar <- getDispatcherTVar
  lift (atomically (readTVar processesVar))
