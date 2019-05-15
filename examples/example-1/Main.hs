-- | A complete example for the library
module Main where

import           GHC.Stack
import           Control.Eff
import           Control.Monad
import           Data.Dynamic
import           Control.Eff.Concurrent
import qualified Control.Exception             as Exc
import qualified Data.Text as T
import           Control.DeepSeq
import           Data.Type.Pretty

data TestApi
  deriving Typeable

type instance ToPretty TestApi = PutStr "test"

data instance Api TestApi x where
  SayHello :: String -> Api TestApi ('Synchronous Bool)
  Shout :: String -> Api TestApi 'Asynchronous
  Terminate :: Api TestApi ('Synchronous ())
  TerminateError :: String -> Api TestApi ('Synchronous ())
  deriving (Typeable)

instance NFData (Api TestApi x) where
  rnf (SayHello s) = rnf s
  rnf (Shout s) = rnf s
  rnf Terminate = ()
  rnf (TerminateError s) = rnf s


data MyException = MyException
    deriving Show

instance Exc.Exception MyException

deriving instance Show (Api TestApi x)

main :: IO ()
main = defaultMain example

mainProcessSpawnsAChildAndReturns :: HasCallStack => Eff (InterruptableProcess q) ()
mainProcessSpawnsAChildAndReturns = void (spawn (void receiveAnyMessage))

example:: ( HasCallStack, Member Logs q, Lifted IO q) => Eff (InterruptableProcess q) ()
example = do
  me <- self
  logInfo (T.pack ("I am " ++ show me))
  server <- testServerLoop
  logInfo (T.pack ("Started server " ++ show server))
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
            logInfo (T.pack ("Result: " ++ show res))
            go
  registerServer server go

testServerLoop
  :: forall q
   . ( HasCallStack
     , Member Logs q
     , Lifted IO q
     )
  => Eff (InterruptableProcess q) (Server TestApi)
testServerLoop = spawnApiServer
  (handleCastTest <> handleCalls handleCallTest) handleTerminateTest
 where
  handleCastTest = handleCasts $ \(Shout x) -> do
    me <- self
    logInfo (T.pack (show me ++ " Shouting: " ++ x))
    return AwaitNext
  handleCallTest :: Api TestApi ('Synchronous r) -> (Eff (InterruptableProcess q) (Maybe r, CallbackResult 'Recoverable) -> xxx) -> xxx
  handleCallTest (SayHello "e1") k = k $ do
    me <- self
    logInfo (T.pack (show me ++ " raising an error"))
    interrupt (ErrorInterrupt "No body loves me... :,(")
  handleCallTest (SayHello "e2") k = k $ do
    me <- self
    logInfo (T.pack (show me ++ " throwing a MyException "))
    void (lift (Exc.throw MyException))
    pure (Nothing, AwaitNext)
  handleCallTest (SayHello "self") k = k $ do
    me <- self
    logInfo (T.pack (show me ++ " casting to self"))
    cast (asServer @TestApi me) (Shout "from me")
    return (Just False, AwaitNext)
  handleCallTest (SayHello "stop") k = k $ do
    me <- self
    logInfo (T.pack (show me ++ " stopping me"))
    return (Just False, StopServer (ErrorInterrupt "test error"))
  handleCallTest (SayHello x) k = k $ do
    me <- self
    logInfo (T.pack (show me ++ " Got Hello: " ++ x))
    return (Just (length x > 3), AwaitNext)
  handleCallTest Terminate k = k $ do
    me <- self
    logInfo (T.pack (show me ++ " exiting"))
    pure (Just (), StopServer NormalExitRequested)
  handleCallTest (TerminateError msg) k = k $ do
    me <- self
    logInfo (T.pack (show me ++ " exiting with error: " ++ msg))
    pure (Just (), StopServer (ErrorInterrupt msg))
  handleTerminateTest = InterruptCallback $ \msg -> do
    me <- self
    logInfo (T.pack (show me ++ " is exiting: " ++ show msg))
    logProcessExit msg
    pure (StopServer (interruptToExit msg))
