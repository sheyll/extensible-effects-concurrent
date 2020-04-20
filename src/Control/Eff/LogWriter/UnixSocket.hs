-- | This helps to setup logging to /standard ouput/.
module Control.Eff.LogWriter.UnixSocket
  ( withUnixSocketLogWriter
  , withUnixSocketLogging
  )
where

import           Control.Eff                   as Eff
import           Control.Eff.Log
import           Control.Eff.LogWriter.Rich
import           Data.Text                     as T
import           Data.Text.Encoding            as T
import qualified Control.Exception.Safe        as Safe
import           Control.Monad                  ( void )
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Trans.Control    ( MonadBaseControl
                                                , liftBaseOp
                                                )
import           GHC.Stack
import           Network.Socket          hiding ( sendTo )
import           Network.Socket.ByteString

-- | Enable logging to a /unix domain socket/, with some 'LogEvent' fields preset
-- as in 'withRichLogging'.
--
-- See 'Control.Eff.Log.Examples.exampleDevLogSyslogLogging'
withUnixSocketLogging
  :: (HasCallStack, MonadBaseControl IO (Eff e), Lifted IO e)
  => LogEventReader Text -- ^ 'LogEvent' rendering function
  -> FilePath -- ^ Path to the socket file
  -> String -- ^ The default application name to put into the 'logEventAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'logEventFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader : e) a
  -> Eff e a
withUnixSocketLogging render socketPath a f p e = liftBaseOp
  (withUnixSocketSocket render socketPath)
  (\lw -> withRichLogging lw a f p e)

-- | Enable logging to a (remote-) host via UnixSocket.
--
-- See 'Control.Eff.Log.Examples.exampleDevLogSyslogLogging'
withUnixSocketLogWriter
  :: (IoLogging e, MonadBaseControl IO (Eff e), HasCallStack)
  => LogEventReader Text -- ^ 'LogEvent' rendering function
  -> FilePath -- ^ Path to the socket file
  -> Eff e b
  -> Eff e b
withUnixSocketLogWriter render socketPath e =
  liftBaseOp (withUnixSocketSocket render socketPath) (`addLogWriter` e)

withUnixSocketSocket
  :: HasCallStack
  => LogEventReader Text -- ^ 'LogEvent' rendering function
  -> FilePath -- ^ Path to the socket file
  -> (LogWriter -> IO a)
  -> IO a
withUnixSocketSocket render socketPath ioE = Safe.bracket
  (socket AF_UNIX Datagram defaultProtocol)
  (Safe.try @IO @Catch.SomeException . close)
  (\s ->
      let addr = SockAddrUnix socketPath
      in
        ioE
          (MkLogWriter
            (\lmStr ->
              void $ sendTo s (T.encodeUtf8 (render lmStr)) addr
            )
          )
  )
