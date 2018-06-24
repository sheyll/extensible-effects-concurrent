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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

-- | Type safe /server/ API processes

module Control.Eff.Concurrent.Api.Server
  ( ApiHandler (..)
  , serve
  , unhandledCallError
  , unhandledCastError
  , defaultTermination
  , serveBoth
  , serve3
  , tryApiHandler
  , UnhandledRequest()
  , catchUnhandled
  , ensureAllHandled
  , castMessage
  , exitUnhandled
  )
where

import           Control.Eff
import           Control.Eff.InternalExtra
import qualified Control.Eff.Exception as Exc
import           Control.Eff.Concurrent.Api
import           Control.Eff.Concurrent.Api.Internal
import           Control.Eff.Concurrent.Process
import           Data.Proxy
import           Data.Typeable (Typeable, typeRep)
import           Data.Dynamic
import           GHC.Stack

-- | Receive messages until the process exits and invoke the callback on each
-- message.
serve
  :: forall r q p
   . (Typeable p, SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> ApiHandler p r
  -> Eff r ()
serve px handlers =
  receiveLoop px
   $ \ mReq ->
       case mReq of
        Left Nothing ->
          applyApiHandler px handlers (Terminate Nothing)
        Left (Just reason) ->
          applyApiHandler px handlers (Terminate (Just reason))
        Right dyn ->
          ensureAllHandled px
            (do msg <- castMessage dyn
                raise (applyApiHandler px handlers msg))

-- | A record of callbacks requests to a /server/, serving a specific 'Api' family instance.
data ApiHandler p r where
  ApiHandler ::
     { -- | A cast will not return a result directly. This is used for async
       -- methods.
       _handleCast
         :: HasCallStack
         => Api p 'Asynchronous -> Eff r ()
      -- | A call is a blocking operation, the caller is blocked until this
      -- handler calls the reply continuation.
     , _handleCall
         :: forall x . HasCallStack
         => Api p ('Synchronous x) -> (x -> Eff r Bool) -> Eff r ()
     -- | This callback is called with @Nothing@ if the process exits
     -- peacefully, or @Just "error message..."@ if the process exits with an
     -- error. This function is responsible to exit the process if necessary.
     -- The default behavior is defined in 'defaultTermination'.
     , _handleTerminate
         :: HasCallStack
         => Maybe String -> Eff r ()
     } -> ApiHandler p r

-- | Apply either the '_handleCall', '_handleCast' or the '_handleTerminate'
-- callback to an incoming 'Request'.
applyApiHandler :: forall r q p
                . ( Typeable p
                  , SetMember Process (Process q) r
                  , HasCallStack)
              => SchedulerProxy q
              -> ApiHandler p r
              -> Request p
              -> Eff r ()
applyApiHandler _px handlers (Terminate e) =
  _handleTerminate handlers e
applyApiHandler _ handlers (Cast request        ) =
  _handleCast handlers request
applyApiHandler px handlers (Call fromPid request) =
  _handleCall handlers request sendReply
      where
       sendReply :: Typeable x => x -> Eff r Bool
       sendReply reply =
         sendMessage px fromPid (toDyn (Response (Proxy @p) reply))

-- * ApiHandler default callbacks

-- | A default handler to use in '_handleCall' in 'ApiHandler'. It will call
-- 'raiseError' with a nice error message.
unhandledCallError
  :: forall p x r q .
    ( Show (Api p ( 'Synchronous x))
    , Typeable p
    , HasCallStack
    , SetMember Process (Process q) r
    )
  => SchedulerProxy q
  -> Api p ( 'Synchronous x)
  -> (x -> Eff r Bool)
  -> Eff r ()
unhandledCallError px api _ = raiseError px
  ("Unhandled call: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep (Proxy @p)) ++ ")")

-- | A default handler to use in '_handleCast' in 'ApiHandler'. It will call
-- 'raiseError' with a nice error message.
unhandledCastError
  :: forall p r q .
    ( Show (Api p 'Asynchronous)
    , Typeable p
    , HasCallStack
    , SetMember Process (Process q) r
    )
  => SchedulerProxy q
  -> Api p 'Asynchronous
  -> Eff r ()
unhandledCastError px api = raiseError px
  ("Unhandled cast: ("
    ++ show api
    ++ " :: "
    ++ show (typeRep (Proxy @p)) ++ ")")

-- | Exit the process either normally of the error message is @Nothing@
-- or with 'exitWithError' otherwise.
defaultTermination :: forall q r .
                     ( HasCallStack, SetMember Process (Process q) r )
                   => SchedulerProxy q
                   -> Maybe String
                   -> Eff r ()
defaultTermination px e =
   maybe (exitNormally px) (exitWithError px) e

-- * Multiple ApiHandler combined

-- | 'serve' two 'ApiHandler's at once. The first handler is used for
-- termination handling.
serveBoth :: forall r q p1 p2
          . ( Typeable p1, Typeable p2
            , SetMember Process (Process q) r
            , HasCallStack)
       => SchedulerProxy q
       -> ApiHandler p1 r -> ApiHandler p2 r
       -> Eff r ()
serveBoth px h1 h2 =
  receiveLoop px
   $ \ mReq ->
       case mReq of
        Left Nothing ->
          applyApiHandler px h1 (Terminate Nothing)
        Left (Just reason) ->
          applyApiHandler px h1 (Terminate (Just reason))
        Right dyn ->
          ensureAllHandled px
            (tryApiHandler px h1 dyn
              `catchUnhandled`
              tryApiHandler px h2)

-- | 'serve' three 'ApiHandler's at once. The first handler is used for
-- termination handling.
serve3 :: forall r q p1 p2 p3
          . ( Typeable p1, Typeable p2, Typeable p3
            , SetMember Process (Process q) r
            , HasCallStack)
       => SchedulerProxy q
       -> ApiHandler p1 r -> ApiHandler p2 r -> ApiHandler p3 r
       -> Eff r ()
serve3 px h1 h2 h3 =
  receiveLoop px
   $ \ mReq ->
       case mReq of
        Left Nothing ->
          applyApiHandler px h1 (Terminate Nothing)
        Left (Just reason) ->
          applyApiHandler px h1 (Terminate (Just reason))
        Right dyn ->
          ensureAllHandled px
            (tryApiHandler px h1 dyn
              `catchUnhandled`
              tryApiHandler px h2
              `catchUnhandled`
              tryApiHandler px h3)

-- | The basic building block of the combination of 'ApiHandler's is this
-- function, which can not only be passed to 'receiveLoop', but also throws an
-- 'UnhandledRequest' exception if 'castMessage' failed, such that multiple
-- invokation of this function for different 'ApiHandler's can be tried, by using 'catchUnhandled'.
--
-- > tryApiHandler px handlers1 message
-- >   `catchUnhandled`
-- > tryApiHandler px handlers2
-- >   `catchUnhandled`
-- > tryApiHandler px handlers3
--
tryApiHandler  :: forall r q p
                . ( Typeable p
                  , SetMember Process (Process q) r
                  , HasCallStack)
              => SchedulerProxy q
              -> ApiHandler p r
              -> Dynamic
              -> Eff (Exc.Exc UnhandledRequest ': r) ()
tryApiHandler px handlers message =
  do request <- castMessage message
     raise (applyApiHandler px handlers request)

-- | An exception that is used by the mechanism that chains together multiple
-- different 'ApiHandler' allowing a single process to implement multiple
-- 'Api's. This exception is thrown by 'castMessage'. This
-- exception can be caught with 'catchUnhandled', this way, several
-- distinct 'ApiHandler' can be tried until one /fits/ or until the
-- 'exitUnhandled' is invoked.
newtype UnhandledRequest = UnhandledRequest { fromUnhandledRequest :: Dynamic }

-- | If 'castMessage' failes to cast the message to a 'Request' for a
-- certain 'ApiHandler' it throws an 'UnhandledRequest' exception. That
-- exception is caught by this function and the raw message is given to the
-- handler function. This is the basis for chaining 'ApiHandler's.
catchUnhandled
  :: forall r a .
    ( Member (Exc.Exc UnhandledRequest) r
    , HasCallStack)
  => Eff r a
  -> (Dynamic -> Eff r a)
  -> Eff r a
catchUnhandled effect handler =
  effect `Exc.catchError` (handler . fromUnhandledRequest)

-- | Catch 'UnhandledRequest's and terminate the process with an error, if necessary.
ensureAllHandled
  :: forall r q .
    (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff (Exc.Exc UnhandledRequest ': r) ()
  -> Eff r ()
ensureAllHandled px effect =
  do result <- Exc.runError effect
     either (exitUnhandled px . fromUnhandledRequest) return result

-- | Cast a 'Dynamic' value, and if that fails, throw an 'UnhandledRequest'
-- error.
castMessage :: forall r a .
              ( HasCallStack
              , Typeable a
              , Member (Exc.Exc UnhandledRequest) r)
            => Dynamic -> Eff r a
castMessage message =
  maybe
  (Exc.throwError (UnhandledRequest message))
  return
  (fromDynamic message)

-- | If an incoming message could not be casted to a 'Request' corresponding to
-- an 'ApiHandler' (e.g. with 'castMessage') one should use this function to
-- exit the process with a corresponding error.
exitUnhandled
  :: forall r q . (SetMember Process (Process q) r, HasCallStack)
  => SchedulerProxy q
  -> Dynamic
  -> Eff r ()
exitUnhandled px message =
  do let reason = "unhandled message: " ++ show message
     exitWithError px reason
