-- | This helps to setup logging to /standard ouput/.
module Control.Eff.LogWriter.UDP
  ( withUDPLogWriter,
    withUDPLogging,
  )
where

import Control.Eff as Eff
import Control.Eff.Log
import Control.Eff.LogWriter.Rich
import qualified Control.Exception.Safe as Safe
import Control.Monad (void)
import qualified Control.Monad.Catch as Catch
import Control.Monad.Trans.Control
  ( MonadBaseControl,
    liftBaseOp,
  )
import Data.Text as T
import Data.Text.Encoding as T
import GHC.Stack
import Network.Socket hiding (sendTo)
import Network.Socket.ByteString

-- | Enable logging to a remote host via __UDP__, with some 'LogEvent' fields preset
-- as in 'withRichLogging'.
--
-- See 'Control.Eff.Log.Examples.exampleUdpRFC3164Logging'
withUDPLogging ::
  (HasCallStack, MonadBaseControl IO (Eff e), Lifted IO e) =>
  -- | 'LogEvent' rendering function
  (LogEvent -> Text) ->
  -- | Hostname or IP
  String ->
  -- | Port e.g. @"514"@
  String ->
  -- | The default application name to put into the 'logEventAppName' field.
  String ->
  -- | The default RFC-5424 facility to put into the 'logEventFacility' field.
  Facility ->
  -- | The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  LogPredicate ->
  Eff (Logs : LogWriterReader : e) a ->
  Eff e a
withUDPLogging render hostname port a f p e =
  liftBaseOp
    (withUDPSocket render hostname port)
    (\lw -> withRichLogging lw a f p e)

-- | Enable logging to a (remote-) host via UDP.
--
-- See 'Control.Eff.Log.Examples.exampleUdpRFC3164Logging'
withUDPLogWriter ::
  (IoLogging e, MonadBaseControl IO (Eff e), HasCallStack) =>
  -- | 'LogEvent' rendering function
  (LogEvent -> Text) ->
  -- | Hostname or IP
  String ->
  -- | Port e.g. @"514"@
  String ->
  Eff e b ->
  Eff e b
withUDPLogWriter render hostname port e =
  liftBaseOp (withUDPSocket render hostname port) (`addLogWriter` e)

withUDPSocket ::
  HasCallStack =>
  -- | 'LogEvent' rendering function
  (LogEvent -> Text) ->
  -- | Hostname or IP
  String ->
  -- | Port e.g. @"514"@
  String ->
  (LogWriter -> IO a) ->
  IO a
withUDPSocket render hostname port ioE =
  Safe.bracket
    ( do
        let hints = defaultHints {addrSocketType = Datagram}
        addr : _ <- getAddrInfo (Just hints) (Just hostname) (Just port)
        s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        return (addr, s)
    )
    (Safe.try @IO @Catch.SomeException . close . snd)
    ( \(a, s) ->
        let addr = addrAddress a
         in ioE
              ( MkLogWriter
                  ( \lmStr ->
                      void $ sendTo s (T.encodeUtf8 (render lmStr)) addr
                  )
              )
    )
