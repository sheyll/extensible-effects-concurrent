-- | This helps to setup logging to /standard ouput/.
module Control.Eff.LogWriter.UnixSocket
  ( withUnixSocketLogWriter,
    withUnixSocketLogging,
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
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as Net

-- | Enable logging to a /unix domain socket/, with some 'LogEvent' fields preset
-- as in 'withRichLogging'.
--
-- See 'Control.Eff.Log.Examples.exampleDevLogSyslogLogging'
withUnixSocketLogging ::
  (MonadBaseControl IO (Eff e), Lifted IO e) =>
  -- | 'LogEvent' rendering function
  LogEventReader Text ->
  -- | Path to the socket file
  FilePath ->
  -- | The default application name to put into the 'logEventAppName' field.
  String ->
  -- | The default RFC-5424 facility to put into the 'logEventFacility' field.
  Facility ->
  -- | The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  LogPredicate ->
  Eff (Logs : LogWriterReader : e) a ->
  Eff e a
withUnixSocketLogging render socketPath a f p e =
  liftBaseOp
    (withUnixSocketSocket render socketPath)
    (\lw -> withRichLogging lw a f p e)

-- | Enable logging to a (remote-) host via UnixSocket.
--
-- See 'Control.Eff.Log.Examples.exampleDevLogSyslogLogging'
withUnixSocketLogWriter ::
  (IoLogging e, MonadBaseControl IO (Eff e)) =>
  -- | 'LogEvent' rendering function
  LogEventReader Text ->
  -- | Path to the socket file
  FilePath ->
  Eff e b ->
  Eff e b
withUnixSocketLogWriter render socketPath e =
  liftBaseOp (withUnixSocketSocket render socketPath) (`addLogWriter` e)

withUnixSocketSocket ::
  -- | 'LogEvent' rendering function
  LogEventReader Text ->
  -- | Path to the socket file
  FilePath ->
  (LogWriter -> IO a) ->
  IO a
withUnixSocketSocket render socketPath ioE =
  Safe.bracket
    (Net.socket Net.AF_UNIX Net.Datagram Net.defaultProtocol)
    (Safe.try @IO @Catch.SomeException . Net.close)
    ( \s ->
        let addr = Net.SockAddrUnix socketPath
         in ioE
              ( MkLogWriter
                  ( \lmStr ->
                      void $ Net.sendTo s (T.encodeUtf8 (render lmStr)) addr
                  )
              )
    )
