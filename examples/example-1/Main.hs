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

data TestProtocol
  deriving Typeable

type instance ToPretty TestProtocol = PutStr "test"

data instance Pdu TestProtocol x where
  SayHello :: String -> Protocol TestProtocol ('Synchronous Bool)
  Shout :: String -> Protocol TestProtocol 'Asynchronous
  Terminate :: Pdu TestProtocol ('Synchronous ())
  TerminateError :: String -> Pdu TestProtocol ('Synchronous ())
  deriving (Typeable)

instance NFData (Pdu TestProtocol x) where
  rnf (SayHello s) = rnf s
  rnf (Shout s) = rnf s
  rnf Terminate = ()
  rnf (TerminateError s) = rnf s


data MyException = MyException
    deriving Show

instance Exc.Exception MyException

deriving instance Show (Pdu TestProtocol x)

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
            callEndpointReader (TerminateError rest)
            go
          ('S' : _) -> do
            callEndpointReader Terminate
            go
          ('C' : _) -> do
            castEndpointReader (Shout x)
            go
          ('R' : rest) -> do
            replicateM_ (read rest) (castEndpointReader (Shout x))
            go
          ('q' : _) -> logInfo "Done."
          _         -> do
            res <- callEndpointReader (SayHello x)
            logInfo (T.pack ("Result: " ++ show res))
            go
  registerEndpoint server go

testServerLoop
  :: forall q
   . ( HasCallStack
     , Member Logs q
     , Lifted IO q
     )
  => Eff (InterruptableProcess q) (Endpoint TestProtocol)
testServerLoop = spawnProtocolServer
  (handleCastTest <> handleCalls handleCallTest) handleTerminateTest
 where
  handleCastTest = handleCasts $ \(Shout x) -> do
    me <- self
    logInfo (T.pack (show me ++ " Shouting: " ++ x))
    return AwaitNext
  handleCallTest :: Pdu TestProtocol ('Synchronous r) -> (Eff (InterruptableProcess q) (Maybe r, ServerLoopCommand 'Recoverable) -> xxx) -> xxx
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
    cast (asEndpoint @TestProtocol me) (Shout "from me")
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
