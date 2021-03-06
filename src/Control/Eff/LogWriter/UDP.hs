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
import qualified Network.Socket as Network
import Network.Socket.ByteString as Network

-- | Enable logging to a remote host via __UDP__, with some 'LogEvent' fields preset
-- as in 'withRichLogging'.
--
-- See 'Control.Eff.Log.Examples.exampleUdpRFC3164Logging'
withUDPLogging ::
  (MonadBaseControl IO (Eff e), Lifted IO e) =>
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
  (IoLogging e, MonadBaseControl IO (Eff e)) =>
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
        let hints = Network.defaultHints {Network.addrSocketType = Network.Datagram}
        addr : _ <- Network.getAddrInfo (Just hints) (Just hostname) (Just port)
        s <-
          Network.socket
            (Network.addrFamily addr)
            (Network.addrSocketType addr)
            (Network.addrProtocol addr)
        return (addr, s)
    )
    (Safe.try @IO @Catch.SomeException . Network.close . snd)
    ( \(a, s) ->
        let addr = Network.addrAddress a
         in ioE
              ( MkLogWriter
                  ( \lmStr ->
                      void $ sendTo s (T.encodeUtf8 (render lmStr)) addr
                  )
              )
    )
