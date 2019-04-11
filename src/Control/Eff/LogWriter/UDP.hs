-- | This helps to setup logging to /standard ouput/.
module Control.Eff.LogWriter.UDP
  ( withUDPLogWriter
  , withUDPLogging
  )
where

import           Control.Eff                   as Eff
import           Control.Eff.Log
import           Control.Eff.LogWriter.Console
import           Control.Eff.LogWriter.IO
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Data.Text.Encoding            as T
import qualified Control.Exception.Safe        as Safe
import           Control.Monad                  ( void )
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Trans.Control    ( MonadBaseControl
                                                , liftBaseOp
                                                )
import           GHC.Stack
import qualified System.Socket                 as Socket
import           System.Socket.Type.Datagram   as Socket
import           System.Socket.Family.Inet     as Socket
import qualified System.Socket.Protocol.UDP    as Socket

-- | Enable logging to a remote host via __UDP__, with some 'LogMessage' fields preset
-- as in 'withIoLogging'.
--
-- See 'Control.Eff.Log.Examples.exampleUdpRFC3164Logging'
withUDPLogging
  :: (HasCallStack, MonadBaseControl IO (Eff e), Lifted IO e)
  => (LogMessage -> Text) -- ^ 'LogMessage' rendering function
  -> Text -- ^ Hostname or IP
  -> Text -- ^ Port e.g. @"514"@
  -> Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader IO : e) a
  -> Eff e a
withUDPLogging render hostname port a f p e = liftBaseOp
  (withUDPSocket render hostname port)
  (\lw -> withIoLogging lw a f p e)


-- | Enable logging to a (remote-) host via UDP.
--
-- See 'Control.Eff.Log.Examples.exampleUdpRFC3164Logging'
withUDPLogWriter
  :: (Lifted IO e, LogsTo IO e, MonadBaseControl IO (Eff e), HasCallStack)
  => (LogMessage -> Text) -- ^ 'LogMessage' rendering function
  -> Text -- ^ Hostname or IP
  -> Text -- ^ Port e.g. @"514"@
  -> Eff e b
  -> Eff e b
withUDPLogWriter render hostname port e =
  liftBaseOp (withUDPSocket render hostname port) (`addLogWriter` e)


withUDPSocket
  :: HasCallStack
  => (LogMessage -> Text) -- ^ 'LogMessage' rendering function
  -> Text -- ^ Hostname or IP
  -> Text -- ^ Port e.g. @"514"@
  -> (LogWriter IO -> IO a)
  -> IO a
withUDPSocket render hostname port ioE = Safe.bracket
  (Socket.socket :: IO (Socket.Socket Inet Datagram Socket.UDP))
  (Safe.try @IO @Catch.SomeException . Socket.close)
  (\s -> do
    ai <- Socket.getAddressInfo (Just (T.encodeUtf8 hostname))
                                (Just (T.encodeUtf8 port))
                                mempty
    case ai :: [Socket.AddressInfo Inet Datagram Socket.UDP] of
      (a : _) -> do
        let addr = Socket.socketAddress a
        ioE
          (mkLogWriterIO
            (\lmStr -> void
              $ Socket.sendTo s (T.encodeUtf8 (render lmStr)) Socket.msgNoSignal addr
            )
          )

      [] -> do
        T.putStrLn ("could not resolve UDP syslog server: " <> hostname)
        ioE consoleLogWriter
  )
