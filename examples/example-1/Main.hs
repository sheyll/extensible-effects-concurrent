-- | A complete example for the library
module Main where

import           GHC.Stack
import           Control.Eff
import           Control.Monad
import           Data.Dynamic
import           Control.Eff.Concurrent
import qualified Control.Eff.Concurrent.Protocol.CallbackServer as Callback
import           Control.Eff.Concurrent.Protocol.EffectfulServer as Server
import qualified Control.Exception             as Exc
import           Control.DeepSeq
import           Data.Type.Pretty

data TestProtocol
  deriving Typeable

instance ToTypeLogMsg TestProtocol

type instance ToPretty TestProtocol = PutStr "test"

instance HasPdu TestProtocol where
  data instance Pdu TestProtocol x where
    SayHello :: String -> Pdu TestProtocol ('Synchronous Bool)
    Shout :: String -> Pdu TestProtocol 'Asynchronous
    Terminate :: Pdu TestProtocol ('Synchronous ())
    TerminateError :: LogMsg -> Pdu TestProtocol ('Synchronous ())
    deriving (Typeable)

instance ToLogMsg (Pdu TestProtocol r) where
  toLogMsg = \case
    SayHello x -> packLogMsg "Hello " <> packLogMsg x
    Shout x -> packLogMsg "HEEELLLOOOO " <> packLogMsg x
    Terminate -> packLogMsg "terminate normally"
    TerminateError x -> packLogMsg "terminate with error: " <> x

instance NFData (Pdu TestProtocol x) where
  rnf (SayHello s) = rnf s
  rnf (Shout s) = rnf s
  rnf Terminate = ()
  rnf (TerminateError s) = rnf s


data MyException = MyException
    deriving Show

instance Exc.Exception MyException

main :: IO ()
main = defaultMain example

mainProcessSpawnsAChildAndReturns :: HasCallStack => Eff Effects ()
mainProcessSpawnsAChildAndReturns = void (spawn "some child" (void receiveAnyMessage))

example:: HasCallStack => Eff Effects ()
example = do
  me <- self
  logInfo (MkLogMsg "I am ") me
  server <- testServerLoop
  logInfo (MkLogMsg "Started server ") server
  let go = do
        lift (putStr "Enter something: ")
        x <- lift getLine
        case x of
          ('K' : rest) -> do
            call server (TerminateError (packLogMsg rest))
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
          ('q' : _) -> logInfo (MkLogMsg "Done.")
          _         -> do
            res <- call server (SayHello x)
            logInfo (MkLogMsg "Result: ") res
            go
  go

testServerLoop :: Eff Effects (Endpoint TestProtocol)
testServerLoop = Callback.startLink (Callback.callbacks handleReq "test-server-1")
 where
  handleReq :: Endpoint TestProtocol -> Event TestProtocol -> Eff Effects ()
  handleReq me (OnCall rt cm) =
    case cm of
      Terminate -> do
        logInfo me (MkLogMsg " exiting")
        sendReply rt ()
        interrupt NormalExitRequested

      TerminateError e -> do
        logInfo me (MkLogMsg " exiting with error: ") e
        sendReply rt ()
        interrupt (ErrorInterrupt e)

      SayHello mx ->
        case mx of
          "e1" -> do
            logInfo me (MkLogMsg " raising an error")
            interrupt (ErrorInterrupt "No body loves me... :,(")

          "e2" -> do
            logInfo me (MkLogMsg " throwing a MyException ")
            void (lift (Exc.throw MyException))

          "self" -> do
            logInfo me (MkLogMsg " casting to self")
            cast me (Shout "from me")
            sendReply rt False

          "stop" -> do
            logInfo me (MkLogMsg " stopping me")
            sendReply rt False
            interrupt (ErrorInterrupt "test error")

          x -> do
            logInfo me (MkLogMsg " Got Hello: ") x
            sendReply rt (length x > 3)

  handleReq me (OnCast (Shout x)) = do
    logInfo me (MkLogMsg " Shouting: ") x

  handleReq me (OnInterrupt msg) = do
    logInfo me (MkLogMsg " is exiting: ") msg
    logProcessExit msg
    interrupt msg

  handleReq me wtf =
    logCritical me (MkLogMsg " WTF: ") wtf
