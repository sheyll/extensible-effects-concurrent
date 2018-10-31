-- | Erlang style processes with message passing concurrency based on
-- (more) @extensible-effects@.
module Control.Eff.Concurrent
  (
    -- * Concurrent Processes with Message Passing Concurrency
    module Control.Eff.Concurrent.Process
  ,
    -- * Data Types and Functions for APIs (aka Protocols)
    module Control.Eff.Concurrent.Api
  ,
    -- ** /Client/ Functions for Consuming APIs
    module Control.Eff.Concurrent.Api.Client
  ,
    -- ** /Server/ Functions for Providing APIs
    module Control.Eff.Concurrent.Api.Server
  ,
    -- ** /Observer/ Functions for Events and Event Listener
    module Control.Eff.Concurrent.Api.Observer
  ,
    -- *** Capture /Observation/ in a FIFO Queue
    module Control.Eff.Concurrent.Api.Observer.Queue
  ,
    -- * /Scheduler/ Process Effect Handler
    -- ** Concurrent Scheduler
    module Control.Eff.Concurrent.Process.ForkIOScheduler
  ,
    -- ** Single Threaded Scheduler
    module Control.Eff.Concurrent.Process.SingleThreadedScheduler
  ,
    -- * Utilities
    -- ** Logging Effect
    module Control.Eff.Log
  ,
    -- ** Preventing Space Leaks
    module Control.Eff.Loop
  )
where

import           Control.Eff.Concurrent.Process ( ProcessId(..)
                                                , fromProcessId
                                                , Process(..)
                                                , ConsProcess
                                                , ResumeProcess(..)
                                                , SchedulerProxy(..)
                                                , MessageSelector(..)
                                                , thisSchedulerProxy
                                                , executeAndCatch
                                                , executeAndResume
                                                , yieldProcess
                                                , sendMessage
                                                , sendMessageAs
                                                , sendMessageChecked
                                                , spawn
                                                , spawn_
                                                , receiveMessage
                                                , receiveMessageAs
                                                , receiveMessageSuchThat
                                                , receiveLoop
                                                , receiveLoopAs
                                                , receiveLoopSuchThat
                                                , self
                                                , sendShutdown
                                                , sendShutdownChecked
                                                , makeReference
                                                , exitWithError
                                                , exitNormally
                                                , raiseError
                                                , catchRaisedError
                                                , ignoreProcessError
                                                )
import           Control.Eff.Concurrent.Api     ( Api
                                                , Synchronicity(..)
                                                , Server(..)
                                                , fromServer
                                                , proxyAsServer
                                                , asServer
                                                )
import           Control.Eff.Concurrent.Api.Client
                                                ( cast
                                                , castChecked
                                                , call
                                                , castRegistered
                                                , callRegistered
                                                , callRegisteredA
                                                , ServesApi
                                                , registerServer
                                                , whereIsServer
                                                , ServerReader
                                                )
import           Control.Eff.Concurrent.Api.Server
                                                ( serve
                                                , spawnServer
                                                , spawnServerWithEffects
                                                , ApiHandler(..)
                                                , castCallback
                                                , callCallback
                                                , terminateCallback
                                                , apiHandler
                                                , apiHandlerForever
                                                , castHandler
                                                , castHandlerForever
                                                , callHandler
                                                , callHandlerForever
                                                , castAndCallHandler
                                                , castAndCallHandlerForever
                                                , ApiServerCmd(..)
                                                , unhandledCallError
                                                , unhandledCastError
                                                , defaultTermination
                                                , Servable(..)
                                                , ServerCallback(..)
                                                , requestHandlerSelector
                                                , terminationHandler
                                                )
import           Control.Eff.Concurrent.Api.Observer
                                                ( Observer(..)
                                                , Observable(..)
                                                , ObserverState
                                                , notifyObserver
                                                , registerObserver
                                                , forgetObserver
                                                , SomeObserver(..)
                                                , notifySomeObserver
                                                , Observers()
                                                , manageObservers
                                                , addObserver
                                                , removeObserver
                                                , notifyObservers
                                                , CallbackObserver
                                                , spawnCallbackObserver
                                                , spawnLoggingObserver
                                                )
import           Control.Eff.Concurrent.Api.Observer.Queue
                                                ( ObservationQueue()
                                                , ObservationQueueReader
                                                , readObservationQueue
                                                , tryReadObservationQueue
                                                , flushObservationQueue
                                                , enqueueObservationsRegistered
                                                , enqueueObservations
                                                )
import           Control.Eff.Concurrent.Process.ForkIOScheduler
                                                ( schedule
                                                , defaultMain
                                                , defaultMainWithLogChannel
                                                , SchedulerError(..)
                                                , SchedulerIO
                                                , forkIoScheduler
                                                , HasSchedulerIO
                                                )

import           Control.Eff.Concurrent.Process.SingleThreadedScheduler
                                                ( schedulePure )
import           Control.Eff.Log
import           Control.Eff.Loop
