-- | This helps to setup logging to /standard ouput/.
module Control.Eff.LogWriter.UDP
  ( withUDPLogWriter
  , withUDPLogging
  )
where

import           Control.Eff                   as Eff
import           Control.Eff.Log
import           Control.Eff.LogWriter.IO
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

-- | Enable logging to a remote host via __UDP__, with some 'LogMessage' fields preset
-- as in 'withIoLogging'.
--
-- See 'Control.Eff.Log.Examples.exampleUdpRFC3164Logging'
withUDPLogging
  :: (HasCallStack, MonadBaseControl IO (Eff e), Lifted IO e)
  => (LogMessage -> Text) -- ^ 'LogMessage' rendering function
  -> String -- ^ Hostname or IP
  -> String -- ^ Port e.g. @"514"@
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
  -> String -- ^ Hostname or IP
  -> String -- ^ Port e.g. @"514"@
  -> Eff e b
  -> Eff e b
withUDPLogWriter render hostname port e =
  liftBaseOp (withUDPSocket render hostname port) (`addLogWriter` e)


withUDPSocket
  :: HasCallStack
  => (LogMessage -> Text) -- ^ 'LogMessage' rendering function
  -> String -- ^ Hostname or IP
  -> String -- ^ Port e.g. @"514"@
  -> (LogWriter IO -> IO a)
  -> IO a
withUDPSocket render hostname port ioE = Safe.bracket
  (do
    let hints = defaultHints { addrSocketType = Datagram }
    addr : _ <- getAddrInfo (Just hints) (Just hostname) (Just port)
    s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    return (addr, s)
  )
  (Safe.try @IO @Catch.SomeException . close . snd)
  (\(a, s) ->
      let addr = addrAddress a
      in
        ioE
          (mkLogWriterIO
            (\lmStr ->
              void $ sendTo s (T.encodeUtf8 (render lmStr)) addr
            )
          )
  )
