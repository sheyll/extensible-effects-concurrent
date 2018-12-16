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
main = defaultMain example

mainProcessSpawnsAChildAndReturns
  :: (HasCallStack, SetMember Process (Process q) r, Member Interrupts r)
  => Eff r ()
mainProcessSpawnsAChildAndReturns = void (spawn (void receiveAnyMessage))

example
  :: ( HasCallStack
     , SetMember Process (Process q) r
     , Member Interrupts r
     , HasLogging IO r
     , HasLogging IO q
     )
  => Eff r ()
example = do
  me <- self
  logInfo ("I am " ++ show me)
  server <- testServerLoop
  logInfo ("Started server " ++ show server)
  let go = do
        lift (putStr "Enter something: ")
        x <- lift getLine
        case x of
          ('K' : rest) -> do
            callRegistered (TerminateError rest)
            go
          ('S' : _) -> do
            callRegistered Terminate
            go
          ('C' : _) -> do
            castRegistered (Shout x)
            go
          ('R' : rest) -> do
            replicateM_ (read rest) (castRegistered (Shout x))
            go
          ('q' : _) -> logInfo "Done."
          _         -> do
            res <- callRegistered (SayHello x)
            logInfo ("Result: " ++ show res)
            go
  registerServer server go

testServerLoop
  :: forall r q
   . ( HasCallStack
     , SetMember Process (Process q) r
     , HasLogging IO q
     , Member Interrupts r
     )
  => Eff r (Server TestApi)
testServerLoop = spawnServer
  $ apiHandler handleCastTest handleCallTest handleTerminateTest
 where
  handleCastTest
    :: Api TestApi 'Asynchronous -> Eff (InterruptableProcess q) ApiServerCmd
  handleCastTest (Shout x) = do
    me <- self
    logInfo (show me ++ " Shouting: " ++ x)
    return HandleNextRequest
  handleCallTest
    :: Api TestApi ( 'Synchronous x)
    -> (x -> Eff (InterruptableProcess q) ())
    -> Eff (InterruptableProcess q) ApiServerCmd
  handleCallTest (SayHello "e1") _reply = do
    me <- self
    logInfo (show me ++ " raising an error")
    interrupt (ProcessError "No body loves me... :,(")
  handleCallTest (SayHello "e2") _reply = do
    me <- self
    logInfo (show me ++ " throwing a MyException ")
    lift (Exc.throw MyException)
  handleCallTest (SayHello "self") reply = do
    me <- self
    logInfo (show me ++ " casting to self")
    cast (asServer @TestApi me) (Shout "from me")
    void (reply False)
    return HandleNextRequest
  handleCallTest (SayHello "stop") reply = do
    me <- self
    logInfo (show me ++ " stopping me")
    void (reply False)
    return (StopApiServer (ProcessError "test error"))
  handleCallTest (SayHello x) reply = do
    me <- self
    logInfo (show me ++ " Got Hello: " ++ x)
    void (reply (length x > 3))
    return HandleNextRequest
  handleCallTest Terminate reply = do
    me <- self
    logInfo (show me ++ " exiting")
    void (reply ())
    exitNormally
  handleCallTest (TerminateError msg) reply = do
    me <- self
    logInfo (show me ++ " exiting with error: " ++ msg)
    void (reply ())
    exitWithError msg
  handleTerminateTest msg = do
    me <- self
    logInfo (show me ++ " is exiting: " ++ show msg)
    logProcessExit msg
