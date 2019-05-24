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
  SayHello :: String -> Pdu TestProtocol ('Synchronous Bool)
  Shout :: String -> Pdu TestProtocol 'Asynchronous
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

mainProcessSpawnsAChildAndReturns :: HasCallStack => Eff InterruptableProcEff ()
mainProcessSpawnsAChildAndReturns = void (spawn (void receiveAnyMessage))

example:: HasCallStack => Eff InterruptableProcEff ()
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
            call server (TerminateError rest)
            go
          ('S' : _) -> do
            call server Terminate
            go
          ('C' : _) -> do
            cast server (Shout x)
            go
          ('R' : rest) -> do
            replicateM_ (read rest) (cast server (Shout x))
            go
          ('q' : _) -> logInfo "Done."
          _         -> do
            res <- call server (SayHello x)
            logInfo (T.pack ("Result: " ++ show res))
            go
  go

testServerLoop :: Eff InterruptableProcEff (Endpoint TestProtocol)
testServerLoop = spawnProtocolServer
  (simpleGenServer  (handleReq @(GenServerId TestProtocol)) "test-server-1")
 where
  handleReq :: GenServerId TestProtocol -> Event TestProtocol -> Eff
  handleReq _myId (OnRequest (Call orig Terminate)) = do
    me <- self
    logInfo (T.pack (show me ++ " exiting"))
    sendReply orig ()
    interrupt NormalExitRequested

  handleReq _myId (OnRequest (Call orig (TerminateError e))) = do
    me <- self
    logInfo (T.pack (show me ++ " exiting with error: " ++ e))
    sendReply orig ()
    interrupt (ErrorInterrupt e)

  handleReq _myId (OnRequest (Call orig (SayHello mx))) =
    case mx of
      "e1" -> do
        me <- self
        logInfo (T.pack (show me ++ " raising an error"))
        interrupt (ErrorInterrupt "No body loves me... :,(")

      "e2" -> do
        me <- self
        logInfo (T.pack (show me ++ " throwing a MyException "))
        void (lift (Exc.throw MyException))

      "self" -> do
        me <- self
        logInfo (T.pack (show me ++ " casting to self"))
        cast (asEndpoint @TestProtocol me) (Shout "from me")
        sendReply orig False

      "stop" -> do
        me <- self
        logInfo (T.pack (show me ++ " stopping me"))
        sendReply orig False
        interrupt (ErrorInterrupt "test error")

      x -> do
        me <- self
        logInfo (T.pack (show me ++ " Got Hello: " ++ x))
        sendReply orig (length x > 3)

  handleReq _myId (OnRequest (Cast (Shout x))) = do
    me <- self
    logInfo (T.pack (show me ++ " Shouting: " ++ x))

  handleReq _myId (OnInterrupt msg) = do
    me <- self
    logInfo (T.pack (show me ++ " is exiting: " ++ show msg))
    logProcessExit msg
    interrupt msg

  handleReq _myId wtf = do
    me <- self
    logCritical (T.pack (show me ++ " WTF: " ++ show wtf))

