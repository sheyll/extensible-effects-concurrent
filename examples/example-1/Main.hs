-- | A complete example for the library
module Main where

import           GHC.Stack
import           Control.Eff
import           Control.Eff.Lift
import           Control.Monad
import           Data.Dynamic
import           Control.Eff.Concurrent
import qualified Control.Exception             as Exc

data TestApi
  deriving Typeable

data instance Api TestApi x where
  SayHello :: String -> Api TestApi ('Synchronous Bool)
  Shout :: String -> Api TestApi 'Asynchronous
  Terminate :: Api TestApi ('Synchronous ())
  TerminateError :: String -> Api TestApi ('Synchronous ())
  deriving (Typeable)

data MyException = MyException
    deriving Show

instance Exc.Exception MyException

deriving instance Show (Api TestApi x)

main :: IO ()
main = defaultMain (example forkIoScheduler)

mainProcessSpawnsAChildAndReturns
  :: (HasCallStack, SetMember Process (Process q) r)
  => SchedulerProxy q
  -> Eff r ()
mainProcessSpawnsAChildAndReturns px = void (spawn (void (receiveMessage px)))

example
  :: ( HasCallStack
     , SetMember Process (Process q) r
     , HasLogging IO r
     , HasLogging IO q
     )
  => SchedulerProxy q
  -> Eff r ()
example px = do
  me <- self px
  logInfo ("I am " ++ show me)
  server <- testServerLoop px
  logInfo ("Started server " ++ show server)
  let go = do
        lift (putStr "Enter something: ")
        x <- lift getLine
        case x of
          ('K' : rest) -> do
            callRegistered px (TerminateError rest)
            go
          ('S' : _) -> do
            callRegistered px Terminate
            go
          ('C' : _) -> do
            castRegistered px (Shout x)
            go
          ('R' : rest) -> do
            replicateM_ (read rest) (castRegistered px (Shout x))
            go
          ('q' : _) -> logInfo "Done."
          _         -> do
            res <- ignoreProcessError px (callRegistered px (SayHello x))
            logInfo ("Result: " ++ show res)
            go
  registerServer server go

testServerLoop
  :: forall r q
   . (HasCallStack, SetMember Process (Process q) r, HasLogging IO q)
  => SchedulerProxy q
  -> Eff r (Server TestApi)
testServerLoop px = spawnServer px
  $ apiHandler handleCastTest handleCallTest handleTerminateTest
 where
  handleCastTest
    :: Api TestApi 'Asynchronous -> Eff (Process q ': q) ApiServerCmd
  handleCastTest (Shout x) = do
    me <- self px
    logInfo (show me ++ " Shouting: " ++ x)
    return HandleNextRequest
  handleCallTest
    :: Api TestApi ( 'Synchronous x)
    -> (x -> Eff (Process q ': q) ())
    -> Eff (Process q ': q) ApiServerCmd
  handleCallTest (SayHello "e1") _reply = do
    me <- self px
    logInfo (show me ++ " raising an error")
    raiseError px "No body loves me... :,("
  handleCallTest (SayHello "e2") _reply = do
    me <- self px
    logInfo (show me ++ " throwing a MyException ")
    lift (Exc.throw MyException)
  handleCallTest (SayHello "self") reply = do
    me <- self px
    logInfo (show me ++ " casting to self")
    cast px (asServer @TestApi me) (Shout "from me")
    void (reply False)
    return HandleNextRequest
  handleCallTest (SayHello "stop") reply = do
    me <- self px
    logInfo (show me ++ " stopping me")
    void (reply False)
    return (StopApiServer Nothing)
  handleCallTest (SayHello "xxx") reply = do
    me <- self px
    logInfo (show me ++ " stopping me with xxx")
    void (reply False)
    return (StopApiServer (Just "xxx"))
  handleCallTest (SayHello "die") reply = do
    me <- self px
    logInfo (show me ++ " throwing and catching ")
    catchRaisedError
      px
      (\er -> logInfo ("WOW: " ++ show er ++ " - No. This is wrong!"))
      (raiseError px "No body loves me... :,(")
    void (reply True)
    return HandleNextRequest
  handleCallTest (SayHello x) reply = do
    me <- self px
    logInfo (show me ++ " Got Hello: " ++ x)
    void (reply (length x > 3))
    return HandleNextRequest
  handleCallTest Terminate reply = do
    me <- self px
    logInfo (show me ++ " exiting")
    void (reply ())
    exitNormally px
  handleCallTest (TerminateError msg) reply = do
    me <- self px
    logInfo (show me ++ " exiting with error: " ++ msg)
    void (reply ())
    exitWithError px msg
  handleTerminateTest msg = do
    me <- self px
    logInfo (show me ++ " is exiting: " ++ show msg)
    maybe (exitNormally px) (exitWithError px) msg
