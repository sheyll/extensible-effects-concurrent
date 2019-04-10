-- | This helps to setup logging to /standard ouput/.
module Control.Eff.LogWriter.UnixSocket
  ( withUnixSocketLogWriter
  , withUnixSocketLogging
  )
where

import           Control.Eff                   as Eff
import           Control.Eff.Log
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
import           System.Socket.Type.Datagram    as Socket
import           System.Socket.Family.Unix      as Socket
import           System.Socket.Family.Inet      as Socket
import qualified System.Socket.Protocol.Default as Socket
import qualified System.Socket.Protocol.UDP     as Socket

-- | Enable logging to a /unix domain socket/, with some 'LogMessage' fields preset
-- as in 'withIoLogging'.
--
-- Example: Log to "/dev/log"
--
-- > exampleWithUnixSocketLogging :: IO ()
-- > exampleWithUnixSocketLogging =
-- >     runLift
-- >   $ withUnixSocketLogging "/dev/log "my-app" local7 allLogMessages
-- >   $ logInfo "Oh, hi there"
withUnixSocketLogging
  :: (HasCallStack, MonadBaseControl IO (Eff e), Lifted IO e)
  => (LogMessage -> Text) -- ^ 'LogMessage' rendering function
  -> Text -- ^ Hostname or IP
  -> Text -- ^ Port e.g. @"514"@
  -> Text -- ^ The default application name to put into the 'lmAppName' field.
  -> Facility -- ^ The default RFC-5424 facility to put into the 'lmFacility' field.
  -> LogPredicate -- ^ The inital predicate for log messages, there are some pre-defined in "Control.Eff.Log.Message#PredefinedPredicates"
  -> Eff (Logs : LogWriterReader IO : e) a
  -> Eff e a
withUnixSocketLogging render hostname port a f p e = liftBaseOp
  (withUnixSocketSocket render hostname port)
  (\lw -> withIoLogging lw a f p e)


-- | Enable logging to a (remote-) host via UnixSocket.
--
-- Example:
--
-- > exampleWithUnixSocketLogWriter :: IO ()
-- > exampleWithUnixSocketLogWriter =
-- >     runLift
-- >   $ withSomeLogging @IO
-- >   $ withUnixSocketLogWriter
-- >   $ logInfo "Oh, hi there"
withUnixSocketLogWriter
  :: (Lifted IO e, LogsTo IO e, MonadBaseControl IO (Eff e), HasCallStack)
  => (LogMessage -> Text) -- ^ 'LogMessage' rendering function
  -> FilePath -- ^ Path to the socket file
  -> Text -- ^ Hostname or IP
  -> Text -- ^ Port e.g. @"514"@
  -> Eff e b
  -> Eff e b
withUnixSocketLogWriter render hostname port e =
  liftBaseOp (withUnixSocketSocket render hostname port) (`addLogWriter` e)


withUnixSocketSocket
  :: HasCallStack
  => (LogMessage -> Text) -- ^ 'LogMessage' rendering function
  -> FilePath -- ^ Path to the socket file
  -> Text -- ^ Hostname or IP
  -> Text -- ^ Port e.g. @"514"@
  -> (LogWriter IO -> IO a)
  -> IO a
withUnixSocketSocket render hostname port ioE = Safe.bracket
  (Socket.socket :: IO (Socket.Socket Inet Datagram Socket.UnixSocket))
  (Safe.try @IO @Catch.SomeException . Socket.close)
  (\s -> do
    ai <- Socket.getAddressInfo (Just (T.encodeUtf8 hostname))
                                (Just (T.encodeUtf8 port))
                                mempty
    case ai :: [Socket.AddressInfo Inet Datagram Socket.UnixSocket] of
      (a : _) -> do
        let addr = Socket.socketAddress a
        Socket.connect s addr
        ioE
          (mkLogWriterIO
            (\lmStr -> void
              $ Socket.send s (T.encodeUtf8 (render lmStr)) Socket.msgNoSignal
            )
          )

      [] -> do
        T.putStrLn ("could not resolve UnixSocket syslog server: " <> hostname)
        ioE consoleLogWriter
  )


withUnixSocketSocket
  :: HasCallStack
  => LogMessageRender

  -> IO a
withUnixSocketSocket ioE =
  Safe.bracket
      (
        Socket.socket :: IO (Socket.Socket Unix Datagram Socket.Default)
      )
      (Safe.try @IO @Catch.SomeException . Socket.close)
      (\s ->
            case socketAddressUnixPath "/dev/log" of
              Just addr -> do
                Socket.connect s addr
                ioE (mkLogWriterIO (\lmStr ->
                  void $
                  Socket.send
                    s
                    (T.encodeUtf8 (render  lmStr))
                    Socket.msgNoSignal))

              Nothing -> do
                putStrLn "could not open /dev/log"
                ioE consoleLogWriter
        )

-- | Open a file and add the 'LogWriter' in the 'LogWriterReader' tha appends the log messages to it.
withRFC5424UnixDomainSocket
  :: ( Lifted IO e
     , LogsTo IO e
     , MonadBaseControl IO (Eff e)
     )
  => Eff e b
  -> Eff e b
withRFC5424UnixDomainSocket = withUnixDomainSocketLogWriter renderRFC5424

-- | Open a file and add the 'LogWriter' in the 'LogWriterReader' tha appends the log messages to it.
withRFC3164UnixDomainSocketWriter
  :: ( Lifted IO e
     , LogsTo IO e
     , MonadBaseControl IO (Eff e)
     )
  => Eff e b
  -> Eff e b
withRFC3164UnixDomainSocketWriter = withUnixDomainSocketLogWriter renderRFC3164
