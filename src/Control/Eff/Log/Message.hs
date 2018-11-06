-- | An RFC 5434 inspired log message and convenience functions for
-- logging them.
module Control.Eff.Log.Message
  ( LogMessage(..)
  , HasLogging
  , renderRFC5424
  , printLogMessage
  , ioLogMessageHandler
  , ioLogMessageWriter
  , traceLogMessageWriter
  , renderLogMessage
  , increaseLogMessageDistance
  , dropDistantLogMessages
  , logWithSeverity
  , logEmergency
  , logAlert
  , logCritical
  , logError
  , logWarning
  , logNotice
  , logInfo
  , logDebug
  , errorMessage
  , infoMessage
  , debugMessage
  , errorMessageIO
  , infoMessageIO
  , debugMessageIO
  , Severity(fromSeverity)
  , emergencySeverity
  , alertSeverity
  , criticalSeverity
  , errorSeverity
  , warningSeverity
  , noticeSeverity
  , informationalSeverity
  , debugSeverity
  , Facility(fromFacility)
  , kernelMessages
  , userLevelMessages
  , mailSystem
  , systemDaemons
  , securityAuthorizationMessages4
  , linePrinterSubsystem
  , networkNewsSubsystem
  , uucpSubsystem
  , clockDaemon
  , securityAuthorizationMessages10
  , ftpDaemon
  , ntpSubsystem
  , logAuditFacility
  , logAlertFacility
  , clockDaemon2
  , local0
  , local1
  , local2
  , local3
  , local4
  , local5
  , local6
  , local7
  , lmFacility
  , lmSeverity
  , lmTimestamp
  , lmHostname
  , lmAppname
  , lmProcessId
  , lmMessageId
  , lmStructuredData
  , lmSrcLoc
  , lmThreadId
  , lmMessage
  , lmDistance
  , setCallStack
  , setLogMessageTimestamp
  , setLogMessageThreadId
  , StructuredDataElement(..)
  , sdElementId
  , sdElementParameters
  )
where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Log.Handler
import           Control.Lens
import           Control.Monad                  ( (>=>) )
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Foldable
import           Data.Maybe
import           Data.String
import           Data.Time.Clock
import           Data.Time.Format
import           Debug.Trace
import           GHC.Generics
import           GHC.Stack
import           System.FilePath.Posix
import           Text.Printf

-- | A message data type inspired by the RFC-5424 Syslog Protocol
data LogMessage =
  LogMessage { _lmFacility :: Facility
             , _lmSeverity :: Severity
             , _lmTimestamp :: Maybe UTCTime
             , _lmHostname :: Maybe String
             , _lmAppname :: Maybe String
             , _lmProcessId :: Maybe String
             , _lmMessageId :: Maybe String
             , _lmStructuredData :: [StructuredDataElement]
             , _lmThreadId :: Maybe ThreadId
             , _lmSrcLoc :: Maybe SrcLoc
             , _lmMessage :: String
             , _lmDistance :: Int }
  deriving (Eq, Generic)

-- | A convenient alias for the constraints that enable logging of 'LogMessage's
-- in the monad, which is 'Lift'ed into a given @Eff@ effect list.
type HasLogging writerM effect = (HasLogWriter LogMessage writerM effect)

showLmMessage :: LogMessage -> [String]
showLmMessage (LogMessage _f _s _ts _hn _an _pid _mi _sd ti loc msg _dist) =
  if null msg
    then []
    else
      maybe "" (printf "[%s]" . show) ti
      : (msg ++ replicate (max 0 (60 - length msg)) ' ')
      : maybe
          []
          (\sl -> pure
            (printf "% 30s line %i"
                    (takeFileName (srcLocFile sl))
                    (srcLocStartLine sl)
            )
          )
          loc

-- | A 'LogWriter' that applys 'renderLogMessage' to the log message and then
-- traces it using 'traceM'.
traceLogMessageWriter :: Monad m => LogWriter LogMessage m
traceLogMessageWriter =
  foldingLogWriter (traverse_ (traceM . renderLogMessage))


-- | Render a 'LogMessage' human readable.
renderLogMessage :: LogMessage -> String
renderLogMessage l@(LogMessage _f s ts hn an pid mi sd _ _ _ _) =
  unwords $ filter
    (not . null)
    ( maybe
        ""
        (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")))
        ts
    : fromMaybe "" hn
    : show s
    : fromMaybe "" an
    : fromMaybe "" pid
    : fromMaybe "" mi
    : (if null sd then "" else show sd)
    : showLmMessage l
    )

-- | Render a 'LogMessage' according to the rules in the given RFC, except for
-- the rules concerning unicode and ascii
renderRFC5424 :: LogMessage -> String
renderRFC5424 l@(LogMessage f s ts hn an pid mi sd _ _ _ _) = unwords
  ( ("<" ++ show (fromSeverity s + fromFacility f * 8) ++ ">" ++ "1")
  : maybe
      "-"
      (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")))
      ts
  : fromMaybe "-" hn
  : fromMaybe "-" an
  : fromMaybe "-" pid
  : fromMaybe "-" mi
  : (if null sd then "-" else show sd)
  : showLmMessage l
  )


instance NFData LogMessage

-- | RFC-5424 defines how structured data can be included in a log message.
data StructuredDataElement =
  SdElement { _sdElementId :: String
            , _sdElementParameters :: [SdParameter]}
  deriving (Eq, Ord, Generic)

instance Show StructuredDataElement where
  show (SdElement sdid params) =
    "[" ++ sdName sdid ++ if null params then "" else " " ++ unwords (show <$> params) ++ "]"

instance NFData StructuredDataElement

-- | Component of a 'StructuredDataElement'
data SdParameter =
  SdParameter String String
  deriving (Eq, Ord, Generic)

instance Show SdParameter where
  show (SdParameter k v) = sdName k ++ "=\"" ++ sdParamValue v ++ "\""

sdName :: String -> String
sdName = take 32 . filter (\c -> c == '=' || c == ']' || c == ' ' || c == '"')

sdParamValue :: String -> String
sdParamValue e = e >>= \case
  '"'  -> "\\\""
  '\\' -> "\\\\"
  ']'  -> "\\]"
  x    -> [x]

instance NFData SdParameter

-- | An rfc 5424 severity
newtype Severity =
  Severity {fromSeverity :: Int}
  deriving (Eq, Ord, Generic, NFData)

instance Show Severity where
  show (Severity 1)             = "ALERT    "
  show (Severity 2)             = "CRITICAL "
  show (Severity 3)             = "ERROR    "
  show (Severity 4)             = "WARNING  "
  show (Severity 5)             = "NOTICE   "
  show (Severity 6)             = "INFO     "
  show (Severity x) |  x <= 0   = "EMERGENCY"
                    | otherwise = "DEBUG    "

-- | An rfc 5424 facility
newtype Facility = Facility {fromFacility :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)


makeLenses ''StructuredDataElement
makeLenses ''LogMessage

-- | Put the source location of the given callstack in 'lmSrcLoc'
setCallStack :: CallStack -> LogMessage -> LogMessage
setCallStack cs m = case getCallStack cs of
  []              -> m
  (_, srcLoc) : _ -> m & lmSrcLoc ?~ srcLoc

instance Default LogMessage where
  def = LogMessage def def def def def def def def def def "" 0

instance IsString LogMessage where
  fromString = infoMessage

-- | Render a 'LogMessage' but set the timestamp and thread id fields.
printLogMessage :: LogMessage -> IO ()
printLogMessage = setLogMessageTimestamp >=> putStrLn . renderLogMessage

-- | Set a timestamp (if not set), the thread id (if not set) using IO actions
-- then /write/ the log message using the 'IO' and 'String' based 'LogWriter'.
ioLogMessageWriter
  :: HasCallStack => LogWriter String IO -> LogWriter LogMessage IO
ioLogMessageWriter delegatee = foldingLogWriter
  (   traverse setLogMessageTimestamp
  >=> traverse setLogMessageThreadId
  >=> (writeAllLogMessages delegatee . fmap renderLogMessage)
  )

-- | Use 'ioLogMessageWriter' to /handle/ logging using 'handleLogs'.
ioLogMessageHandler
  :: (HasCallStack, Lifted IO e)
  => LogWriter String IO
  -> Eff (Logs LogMessage ': LogWriterReader LogMessage IO ': e) a
  -> Eff e a
ioLogMessageHandler delegatee =
  writeLogs (ioLogMessageWriter delegatee)

-- | An IO action that sets the current UTC time (see 'enableLogMessageTimestamps')
-- in 'lmTimestamp'.
setLogMessageTimestamp :: MonadIO m => LogMessage -> m LogMessage
setLogMessageTimestamp m = if isNothing (m ^. lmTimestamp)
  then do
    now <- liftIO getCurrentTime
    return (m & lmTimestamp ?~ now)
  else return m

-- | An IO action appends the the 'ThreadId' of the calling process (see 'myThreadId')
-- to 'lmMessage'.
setLogMessageThreadId :: MonadIO m => LogMessage -> m LogMessage
setLogMessageThreadId m = if isNothing (m ^. lmThreadId)
  then do
    t <- liftIO myThreadId
    return (m & lmThreadId ?~ t)
  else return m

-- | Increase the /distance/ of log messages by one.
-- Logs can be filtered by their distance with 'dropDistantLogMessages'
increaseLogMessageDistance
  :: (HasCallStack, HasLogWriter LogMessage h e) => Eff e a -> Eff e a
increaseLogMessageDistance = mapLogMessages (over lmDistance (+ 1))

-- | Drop all log messages with an 'lmDistance' greater than the given
-- value.
dropDistantLogMessages :: (HasLogging m r) => Int -> Eff r a -> Eff r a
dropDistantLogMessages maxDistance =
  filterLogMessages (\lm -> lm ^. lmDistance <= maxDistance)

-- | Log a 'String' as 'LogMessage' with a given 'Severity'.
logWithSeverity
  :: ( HasCallStack
     , Member (Logs LogMessage) e
     )
  => Severity
  -> String
  -> Eff e ()
logWithSeverity !s =
  withFrozenCallStack
    $ logMsg
    . setCallStack callStack
    . set lmSeverity s
    . flip (set lmMessage) def

-- | Log a 'String' as 'emergencySeverity'.
logEmergency
  :: ( HasCallStack
     , Member (Logs LogMessage) e
     )
  => String
  -> Eff e ()
logEmergency = withFrozenCallStack (logWithSeverity emergencySeverity)

-- | Log a message with 'alertSeverity'.
logAlert
  :: ( HasCallStack
     , Member (Logs LogMessage) e
     )
  => String
  -> Eff e ()
logAlert = withFrozenCallStack (logWithSeverity alertSeverity)

-- | Log a 'criticalSeverity' message.
logCritical
  :: ( HasCallStack
     , Member (Logs LogMessage) e
     )
  => String
  -> Eff e ()
logCritical = withFrozenCallStack (logWithSeverity criticalSeverity)

-- | Log a 'errorSeverity' message.
logError
  :: ( HasCallStack
     , Member (Logs LogMessage) e
     )
  => String
  -> Eff e ()
logError = withFrozenCallStack (logWithSeverity errorSeverity)

-- | Log a 'warningSeverity' message.
logWarning
  :: ( HasCallStack
     , Member (Logs LogMessage) e
     )
  => String
  -> Eff e ()
logWarning = withFrozenCallStack (logWithSeverity warningSeverity)

-- | Log a 'noticeSeverity' message.
logNotice
  :: ( HasCallStack
     , Member (Logs LogMessage) e
     )
  => String
  -> Eff e ()
logNotice = withFrozenCallStack (logWithSeverity noticeSeverity)

-- | Log a 'informationalSeverity' message.
logInfo
  :: ( HasCallStack
     , Member (Logs LogMessage) e
     )
  => String
  -> Eff e ()
logInfo = withFrozenCallStack (logWithSeverity informationalSeverity)

-- | Log a 'debugSeverity' message.
logDebug
  :: ( HasCallStack
     , Member (Logs LogMessage) e
     )
  => String
  -> Eff e ()
logDebug = withFrozenCallStack (logWithSeverity debugSeverity)

-- | Construct a 'LogMessage' with 'errorSeverity'
errorMessage :: HasCallStack => String -> LogMessage
errorMessage m = withFrozenCallStack
  (def & lmSeverity .~ errorSeverity & lmMessage .~ m & setCallStack callStack)

-- | Construct a 'LogMessage' with 'informationalSeverity'
infoMessage :: HasCallStack => String -> LogMessage
infoMessage m = withFrozenCallStack
  (  def
  &  lmSeverity
  .~ informationalSeverity
  &  lmMessage
  .~ m
  &  setCallStack callStack
  )

-- | Construct a 'LogMessage' with 'debugSeverity'
debugMessage :: HasCallStack => String -> LogMessage
debugMessage m = withFrozenCallStack
  (def & lmSeverity .~ debugSeverity & lmMessage .~ m & setCallStack callStack)

-- | Construct a 'LogMessage' with 'errorSeverity'
errorMessageIO :: (HasCallStack, MonadIO m) => String -> m LogMessage
errorMessageIO =
  withFrozenCallStack
    $ (setLogMessageThreadId >=> setLogMessageTimestamp)
    . errorMessage

-- | Construct a 'LogMessage' with 'informationalSeverity'
infoMessageIO :: (HasCallStack, MonadIO m) => String -> m LogMessage
infoMessageIO =
  withFrozenCallStack
    $ (setLogMessageThreadId >=> setLogMessageTimestamp)
    . infoMessage

-- | Construct a 'LogMessage' with 'debugSeverity'
debugMessageIO :: (HasCallStack, MonadIO m) => String -> m LogMessage
debugMessageIO =
  withFrozenCallStack
    $ (setLogMessageThreadId >=> setLogMessageTimestamp)
    . debugMessage

emergencySeverity :: Severity
emergencySeverity = Severity 0

alertSeverity :: Severity
alertSeverity = Severity 1

criticalSeverity :: Severity
criticalSeverity = Severity 2

errorSeverity :: Severity
errorSeverity = Severity 3

warningSeverity :: Severity
warningSeverity = Severity 4

noticeSeverity :: Severity
noticeSeverity = Severity 5

informationalSeverity :: Severity
informationalSeverity = Severity 6

debugSeverity :: Severity
debugSeverity = Severity 7

instance Default Severity where
  def = debugSeverity

kernelMessages :: Facility
kernelMessages = Facility 0

userLevelMessages :: Facility
userLevelMessages = Facility 1

mailSystem :: Facility
mailSystem = Facility 2

systemDaemons :: Facility
systemDaemons = Facility 3

securityAuthorizationMessages4 :: Facility
securityAuthorizationMessages4 = Facility 4

linePrinterSubsystem :: Facility
linePrinterSubsystem = Facility 6

networkNewsSubsystem :: Facility
networkNewsSubsystem = Facility 7

uucpSubsystem :: Facility
uucpSubsystem = Facility 8

clockDaemon :: Facility
clockDaemon = Facility 9

securityAuthorizationMessages10 :: Facility
securityAuthorizationMessages10 = Facility 10

ftpDaemon :: Facility
ftpDaemon = Facility 11

ntpSubsystem :: Facility
ntpSubsystem = Facility 12

logAuditFacility :: Facility
logAuditFacility = Facility 13

logAlertFacility :: Facility
logAlertFacility = Facility 14

clockDaemon2 :: Facility
clockDaemon2 = Facility 15

local0 :: Facility
local0 = Facility 16

local1 :: Facility
local1 = Facility 17

local2 :: Facility
local2 = Facility 18

local3 :: Facility
local3 = Facility 19

local4 :: Facility
local4 = Facility 20

local5 :: Facility
local5 = Facility 21

local6 :: Facility
local6 = Facility 22

local7 :: Facility
local7 = Facility 23

instance Default Facility where
  def = local7
