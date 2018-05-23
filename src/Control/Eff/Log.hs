{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
-- | An extensible effect that wraps 'Control.Monad.Log.MonadLog' into an extensible effect.
module Control.Eff.Log
  ( handleLogsWith
  , Logs(..)
  , logMessageFreeEff
  , logMsg
  , module ExtLog
  , LogChannel()
  , logChannelPutIO
  , forwardLogsToChannel
  , forkLogChannel
  , joinLogChannel
  , logChannelBracket
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Eff as Eff
import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.Log as ExtLog hiding ()
import Control.Monad.Trans.Control
import Data.Kind
import qualified Control.Eff.Lift as Eff
import qualified Control.Monad.Log as Log

-- | The 'Eff'ect type to wrap 'ExtLog.MonadLog'.
-- This is a
data Logs message a where
  LogMessageFree :: (forall n . Monoid n => (message -> n) -> n) -> Logs message ()

-- | Effectful version of the /strange/ 'ExtLog.logMessageFree' function.
logMessageFreeEff :: Member (Logs message) r => (forall n . Monoid n => (message -> n) -> n) -> Eff r ()
logMessageFreeEff foldMapish = send (LogMessageFree foldMapish)

-- | Effectful version of the 'ExtLog.logMessage' function.
logMsg :: Member (Logs m) r => m -> Eff r ()
logMsg msg = logMessageFreeEff ($ msg)

-- | Handle 'Logs' effects using 'Log.LoggingT' 'Log.Handler's.
handleLogsWith :: forall m r message a .
                 (Monad m, SetMember Eff.Lift (Eff.Lift m) r)
               => Eff (Logs message ': r) a
               -> (forall b . (Log.Handler m message -> m b) -> m b)
               -> Eff r a
handleLogsWith actionThatLogs foldHandler =
  Eff.handle_relay return go actionThatLogs
  where
    go :: Logs message b -> (b -> Eff r c) -> Eff r c
    go (LogMessageFree foldMapish) k =
      Eff.lift
      (foldHandler
        (Log.runLoggingT
          (Log.logMessageFree foldMapish)))
      >>= k

-- * Concurrent Logging

-- | Input queue for a concurrent logger.
data LogChannel message =
  LogChannel { fromLogChannel :: TQueue message
             , logChannelOpen :: TVar Bool
             , logChannelThread :: ThreadId
             }

-- | Send the log messages to a 'LogChannel'.
forwardLogsToChannel :: forall r message a
                       . (SetMember Eff.Lift (Eff.Lift IO) r)
                     => LogChannel message
                     -> Eff (Logs message ': r) a
                     -> Eff r a
forwardLogsToChannel logChan actionThatLogs = do
  handleLogsWith actionThatLogs
    (\withHandler -> withHandler (logChannelPutIO logChan))

-- | Enqueue a log message into a log channel
logChannelPutIO :: LogChannel message -> message -> IO ()
logChannelPutIO c m =
  atomically (do isOpen <- readTVar (logChannelOpen c)
                 when isOpen (writeTQueue (fromLogChannel c) m))

-- | Fork 'LogChannel' backed by a process that repeatedly receives log messages
-- sent by 'forwardLogstochannel' or 'logChannelPutIO'. The process logs by
-- invoken the given IO action. To stop and kill a 'LogChannel' invoke
-- 'joinLogChannel'.
forkLogChannel :: forall message
                 . (message -> IO ())
               -> Maybe message
               -> IO (LogChannel message)
forkLogChannel handle mFirstMsg =
  do (msgQ,isOpenV) <-
       atomically (do tq <- newTQueue
                      v <- newTVar True
                      mapM_ (writeTQueue tq) mFirstMsg
                      return (tq, v))
     thread <- forkFinally
              (logLoop msgQ isOpenV)
              (const (cleanUp msgQ isOpenV))
     return (LogChannel msgQ isOpenV thread)
  where
    cleanUp :: TQueue message -> TVar Bool -> IO ()
    cleanUp tq isOpenVar =
      atomically (do writeTVar isOpenVar False
                     flushTQueue tq)
      >>= mapM_ handle
    logLoop :: TQueue message -> TVar Bool -> IO ()
    logLoop tq isOpenVar =
      do mMsg <-
           atomically (do isOpen <- readTVar isOpenVar
                          if isOpen
                            then Just <$> readTQueue tq
                            else return Nothing)
         case mMsg of
           Just msg ->
             do handle msg
                logLoop tq isOpenVar
           Nothing ->
             return ()

-- | Close a log channel. Subsequent loggin requests will no be handled any
-- more.
joinLogChannel :: Maybe message -> LogChannel message -> IO ()
joinLogChannel closeLogMessage (LogChannel tq isOpenVar thread) =
  do wasOpen <-
        atomically
        (do isOpen <- readTVar isOpenVar
            if isOpen
              then
                do writeTVar isOpenVar False
                   mapM_ (writeTQueue tq) closeLogMessage
                   return True
              else return False)
     when wasOpen (killThread thread)


-- | Wrap 'LogChannel' creation and destruction around a monad action in
-- 'bracket'y manner.
logChannelBracket :: Maybe message
                  -> Maybe message
                  -> (LogChannel message -> IO a)
                  -> LoggingT message IO a
logChannelBracket mWelcome mGoodbye f =
  control
  (\ runInIO ->
      do myTId <- myThreadId
         let logHandler = void . runInIO . logMessage
         bracket
           (forkLogChannel
             logHandler
             mWelcome)
           (joinLogChannel
            mGoodbye)
           f)
