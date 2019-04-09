module LogMessageIdeaTest where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Lens
import           Data.Default
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Time.Clock
import           Data.Time.Format
import           GHC.Generics            hiding ( to )
import           GHC.Stack
import           System.FilePath.Posix
import           Text.Printf
import           Control.Eff
import           Control.Eff.Reader.Lazy


-- | A message data type inspired by the RFC-5424 Syslog Protocol
data LogMessage =
  MkLogMessage { _lmFacility :: !Facility
               , _lmSeverity :: !Severity
               , _lmTimestamp :: (Maybe UTCTime)
               , _lmHostname :: (Maybe T.Text)
               , _lmAppName :: (Maybe T.Text)
               , _lmProcessId :: (Maybe T.Text)
               , _lmMessageId :: (Maybe T.Text)
               , _lmStructuredData :: [StructuredDataElement]
               , _lmThreadId :: (Maybe ThreadId)
               , _lmSrcLoc :: (Maybe SrcLoc)
               , _lmMessage :: T.Text}
  deriving (Eq, Generic)

instance Default LogMessage where
  def = MkLogMessage def def def def def def def def def def ""

instance NFData LogMessage

-- | RFC-5424 defines how structured data can be included in a log message.
data StructuredDataElement =
  SdElement { _sdElementId :: !T.Text
            , _sdElementParameters :: ![SdParameter]}
  deriving (Eq, Ord, Generic, Show)


renderSdElement :: StructuredDataElement -> T.Text
renderSdElement (SdElement sdId params) = "[" <> sdName sdId <> if null params
  then ""
  else " " <> T.unwords (renderSdParameter <$> params) <> "]"

instance NFData StructuredDataElement

-- | Component of an RFC-5424 'StructuredDataElement'
data SdParameter =
  MkSdParameter !T.Text !T.Text
  deriving (Eq, Ord, Generic, Show)

renderSdParameter :: SdParameter -> T.Text
renderSdParameter (MkSdParameter k v) =
  sdName k <> "=\"" <> sdParamValue v <> "\""

-- | Extract the name of an 'SdParameter' the length is cropped to 32 according to RFC 5424.
sdName :: T.Text -> T.Text
sdName =
  T.take 32 . T.filter (\c -> c == '=' || c == ']' || c == ' ' || c == '"')

-- | Extract the value of an 'SdParameter'.
sdParamValue :: T.Text -> T.Text
sdParamValue = T.concatMap $ \case
  '"'  -> "\\\""
  '\\' -> "\\\\"
  ']'  -> "\\]"
  x    -> T.singleton x

instance NFData SdParameter

-- | An rfc 5424 severity
newtype Severity =
  Severity {fromSeverity :: Int}
  deriving (Eq, Ord, Generic, NFData)

instance Show Severity where
  show (Severity 1) = "ALERT    "
  show (Severity 2) = "CRITICAL "
  show (Severity 3) = "ERROR    "
  show (Severity 4) = "WARNING  "
  show (Severity 5) = "NOTICE   "
  show (Severity 6) = "INFO     "
  show (Severity x) | x <= 0    = "EMERGENCY"
                    | otherwise = "DEBUG    "
--  *** Severities

-- | Smart constructor for the RFC-5424 __emergency__ 'LogMessage' 'Severity'.
-- This corresponds to the severity value __0__.
-- See 'lmSeverity'.
emergencySeverity :: Severity
emergencySeverity = Severity 0

-- | Smart constructor for the RFC-5424 __alert__ 'LogMessage' 'Severity'.
-- This corresponds to the severity value __1__.
-- See 'lmSeverity'.
alertSeverity :: Severity
alertSeverity = Severity 1

-- | Smart constructor for the RFC-5424 __critical__ 'LogMessage' 'Severity'.
-- This corresponds to the severity value __2__.
-- See 'lmSeverity'.
criticalSeverity :: Severity
criticalSeverity = Severity 2

-- | Smart constructor for the RFC-5424 __error__ 'LogMessage' 'Severity'.
-- This corresponds to the severity value __3__.
-- See 'lmSeverity'.
errorSeverity :: Severity
errorSeverity = Severity 3

-- | Smart constructor for the RFC-5424 __warning__ 'LogMessage' 'Severity'.
-- This corresponds to the severity value __4__.
-- See 'lmSeverity'.
warningSeverity :: Severity
warningSeverity = Severity 4

-- | Smart constructor for the RFC-5424 __notice__ 'LogMessage' 'Severity'.
-- This corresponds to the severity value __5__.
-- See 'lmSeverity'.
noticeSeverity :: Severity
noticeSeverity = Severity 5

-- | Smart constructor for the RFC-5424 __informational__ 'LogMessage' 'Severity'.
-- This corresponds to the severity value __6__.
-- See 'lmSeverity'.
informationalSeverity :: Severity
informationalSeverity = Severity 6

-- | Smart constructor for the RFC-5424 __debug__ 'LogMessage' 'Severity'.
-- This corresponds to the severity value __7__.
-- See 'lmSeverity'.
debugSeverity :: Severity
debugSeverity = Severity 7

instance Default Severity where
  def = debugSeverity


-- | An rfc 5424 facility
newtype Facility = Facility {fromFacility :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Smart constructor for the RFC-5424 'LogMessage' facility @kernelMessages@.
-- See 'lmFacility'.
kernelMessages :: Facility
kernelMessages = Facility 0

-- | Smart constructor for the RFC-5424 'LogMessage' facility @userLevelMessages@.
-- See 'lmFacility'.
userLevelMessages :: Facility
userLevelMessages = Facility 1

-- | Smart constructor for the RFC-5424 'LogMessage' facility @mailSystem@.
-- See 'lmFacility'.
mailSystem :: Facility
mailSystem = Facility 2

-- | Smart constructor for the RFC-5424 'LogMessage' facility @systemDaemons@.
-- See 'lmFacility'.
systemDaemons :: Facility
systemDaemons = Facility 3

-- | Smart constructor for the RFC-5424 'LogMessage' facility @securityAuthorizationMessages4@.
-- See 'lmFacility'.
securityAuthorizationMessages4 :: Facility
securityAuthorizationMessages4 = Facility 4

-- | Smart constructor for the RFC-5424 'LogMessage' facility @linePrinterSubsystem@.
-- See 'lmFacility'.
linePrinterSubsystem :: Facility
linePrinterSubsystem = Facility 6

-- | Smart constructor for the RFC-5424 'LogMessage' facility @networkNewsSubsystem@.
-- See 'lmFacility'.
networkNewsSubsystem :: Facility
networkNewsSubsystem = Facility 7

-- | Smart constructor for the RFC-5424 'LogMessage' facility @uucpSubsystem@.
-- See 'lmFacility'.
uucpSubsystem :: Facility
uucpSubsystem = Facility 8

-- | Smart constructor for the RFC-5424 'LogMessage' facility @clockDaemon@.
-- See 'lmFacility'.
clockDaemon :: Facility
clockDaemon = Facility 9

-- | Smart constructor for the RFC-5424 'LogMessage' facility @securityAuthorizationMessages10@.
-- See 'lmFacility'.
securityAuthorizationMessages10 :: Facility
securityAuthorizationMessages10 = Facility 10

-- | Smart constructor for the RFC-5424 'LogMessage' facility @ftpDaemon@.
-- See 'lmFacility'.
ftpDaemon :: Facility
ftpDaemon = Facility 11

-- | Smart constructor for the RFC-5424 'LogMessage' facility @ntpSubsystem@.
-- See 'lmFacility'.
ntpSubsystem :: Facility
ntpSubsystem = Facility 12

-- | Smart constructor for the RFC-5424 'LogMessage' facility @logAuditFacility@.
-- See 'lmFacility'.
logAuditFacility :: Facility
logAuditFacility = Facility 13

-- | Smart constructor for the RFC-5424 'LogMessage' facility @logAlertFacility@.
-- See 'lmFacility'.
logAlertFacility :: Facility
logAlertFacility = Facility 14

-- | Smart constructor for the RFC-5424 'LogMessage' facility @clockDaemon2@.
-- See 'lmFacility'.
clockDaemon2 :: Facility
clockDaemon2 = Facility 15

-- | Smart constructor for the RFC-5424 'LogMessage' facility @local0@.
-- See 'lmFacility'.
local0 :: Facility
local0 = Facility 16

-- | Smart constructor for the RFC-5424 'LogMessage' facility @local1@.
-- See 'lmFacility'.
local1 :: Facility
local1 = Facility 17

-- | Smart constructor for the RFC-5424 'LogMessage' facility @local2@.
-- See 'lmFacility'.
local2 :: Facility
local2 = Facility 18

-- | Smart constructor for the RFC-5424 'LogMessage' facility @local3@.
-- See 'lmFacility'.
local3 :: Facility
local3 = Facility 19

-- | Smart constructor for the RFC-5424 'LogMessage' facility @local4@.
-- See 'lmFacility'.
local4 :: Facility
local4 = Facility 20

-- | Smart constructor for the RFC-5424 'LogMessage' facility @local5@.
-- See 'lmFacility'.
local5 :: Facility
local5 = Facility 21

-- | Smart constructor for the RFC-5424 'LogMessage' facility @local6@.
-- See 'lmFacility'.
local6 :: Facility
local6 = Facility 22

-- | Smart constructor for the RFC-5424 'LogMessage' facility @local7@.
-- See 'lmFacility'.
local7 :: Facility
local7 = Facility 23

instance Default Facility where
  def = local7

makeLensesWith (lensRules & generateSignatures .~ False) ''StructuredDataElement

-- | A lens for 'SdParameter's
sdElementParameters
  :: Functor f
  => ([SdParameter] -> f [SdParameter])
  -> StructuredDataElement
  -> f StructuredDataElement

-- | A lens for the key or ID of a group of RFC 5424 key-value pairs.
sdElementId
  :: Functor f
  => (T.Text -> f T.Text)
  -> StructuredDataElement
  -> f StructuredDataElement

makeLensesWith (lensRules & generateSignatures .~ False) ''LogMessage

-- | A lens for the UTC time of a 'LogMessage'
-- The function 'setLogMessageTimestamp' can be used to set the field.
lmTimestamp
  :: Functor f
  => (Maybe UTCTime -> f (Maybe UTCTime))
  -> LogMessage
  -> f LogMessage

-- | A lens for the 'ThreadId' of a 'LogMessage'
-- The function 'setLogMessageThreadId' can be used to set the field.
lmThreadId
  :: Functor f
  => (Maybe ThreadId -> f (Maybe ThreadId))
  -> LogMessage
  -> f LogMessage

-- | A lens for the 'StructuredDataElement' of a 'LogMessage'
lmStructuredData
  :: Functor f
  => ([StructuredDataElement] -> f [StructuredDataElement])
  -> LogMessage
  -> f LogMessage

-- | A lens for the 'SrcLoc' of a 'LogMessage'
lmSrcLoc
  :: Functor f
  => (Maybe SrcLoc -> f (Maybe SrcLoc))
  -> LogMessage
  -> f LogMessage

-- | A lens for the 'Severity' of a 'LogMessage'
lmSeverity
  :: Functor f => (Severity -> f Severity) -> LogMessage -> f LogMessage

-- | A lens for a user defined of /process/ id of a 'LogMessage'
lmProcessId
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogMessage
  -> f LogMessage

-- | A lens for a user defined /message id/ of a 'LogMessage'
lmMessageId
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogMessage
  -> f LogMessage

-- | A lens for the user defined textual message of a 'LogMessage'
lmMessage :: Functor f => (T.Text -> f T.Text) -> LogMessage -> f LogMessage

-- | A lens for the hostname of a 'LogMessage'
-- The function 'setLogMessageHostname' can be used to set the field.
lmHostname
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogMessage
  -> f LogMessage

-- | A lens for the 'Facility' of a 'LogMessage'
lmFacility
  :: Functor f => (Facility -> f Facility) -> LogMessage -> f LogMessage

-- | A lens for the RFC 5424 /application/ name of a 'LogMessage'
--
-- One useful pattern for using this field, is to implement log filters that allow
-- info and debug message from the application itself while only allowing warning and error
-- messages from third party libraries:
--
-- > debugLogsForAppName myAppName lm =
-- >   view lmAppName lm == Just myAppName || lmSeverityIsAtLeast warningSeverity lm
--
-- This concept is also implemented in 'discriminateByAppName'.
lmAppName
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogMessage
  -> f LogMessage

instance Show LogMessage where
  show = T.unpack . T.unlines . renderLogMessageBodyTabbed


type LogRenderer a = LogMessage -> a

withRenderer :: LogRenderer a -> Eff (Reader (LogRenderer a) ': e) b -> Eff e b
withRenderer = runReader

newtype SeverityText = MkSeverityText { fromSeverityText :: T.Text }
  deriving (Semigroup)

mkSyslogSeverityText :: LogRenderer SeverityText
mkSyslogSeverityText (MkLogMessage !f !s _ _ _ _ _ _ _ _ _)
   = MkSeverityText $ "<" <> T.pack (show (fromSeverity s + fromFacility f * 8)) <> ">"

newtype FacilityText = MkFacilityText { fromFacilityText :: T.Text }
  deriving (Semigroup)

mkSyslogFacilityText :: LogRenderer FacilityText
mkSyslogFacilityText _ = MkFacilityText ""

newtype TimestampText = MkTimestampText { fromTimestampText :: T.Text }
  deriving (Semigroup)

mkFormattedTimestampText :: LogTimestampFormat -> LogRenderer (Maybe TimestampText)
mkFormattedTimestampText f (MkLogMessage _ _ ts _ _ _ _ _ _ _ _) =
  MkTimestampText . formatLogTimestamp f <$> ts

newtype MessageText = MkMessageText { fromMessageText :: T.Text }
  deriving (Semigroup)

mkMessageText :: LogRenderer MessageText
mkMessageText = MkMessageText . renderLogMessageBody

renderDevLogMessage :: LogRenderer T.Text
renderDevLogMessage =
  run
    $ withRenderer mkSyslogSeverityText
    $ withRenderer mkSyslogFacilityText
    $ withRenderer mkMessageText
    $ withRenderer (fromMaybe (MkTimestampText "-") <$> mkFormattedTimestampText rfc5424NoZTimestamp)
    $ mkDevLogMessage

mkDevLogMessage ::
  ( '[ Reader (LogRenderer SeverityText)
     , Reader (LogRenderer FacilityText)
     , Reader (LogRenderer TimestampText)
     , Reader (LogRenderer MessageText)
     ]
    <:: e
  )
  => Eff e (LogRenderer T.Text)
mkDevLogMessage =
  (\s ts m -> s <> pure " " <> ts <> pure " " <> m)
    <$> (fmap fromSeverityText  <$> ask)
    <*> (fmap fromTimestampText <$> ask)
    <*> (fmap fromMessageText   <$> ask)


-- | A time stamp formatting function
newtype LogTimestampFormat =
  MkLogTimestampFormat { formatLogTimestamp :: UTCTime -> T.Text }

-- | Make a  'LogTimestampFormat' using 'formatTime' in the 'defaultLocale'.
mkLogTimestampFormat
  :: String -- ^ The format string that is passed to 'formatTime'
  -> LogTimestampFormat
mkLogTimestampFormat s = MkLogTimestampFormat (T.pack . formatTime defaultTimeLocale s)

-- | Don't render the time stamp
suppressTimestamp :: LogTimestampFormat
suppressTimestamp = MkLogTimestampFormat (const "")

-- | Render the time stamp using @"%h %d %H:%M:%S"@
rfc3164Timestamp :: LogTimestampFormat
rfc3164Timestamp = mkLogTimestampFormat "%h %d %H:%M:%S"

-- | Render the time stamp to @'iso8601DateFormat' (Just "%H:%M:%S%6QZ")@
rfc5424Timestamp :: LogTimestampFormat
rfc5424Timestamp = mkLogTimestampFormat (iso8601DateFormat (Just "%H:%M:%S%6QZ"))

-- | Render the time stamp like 'rfc5424Timestamp' does, but omit the terminal @Z@ character.
rfc5424NoZTimestamp :: LogTimestampFormat
rfc5424NoZTimestamp = mkLogTimestampFormat (iso8601DateFormat (Just "%H:%M:%S%6Q"))



-- | Print the /body/ of a 'LogMessage'
renderLogMessageBodyTabbed :: LogMessage -> [T.Text]
renderLogMessageBodyTabbed (MkLogMessage _f _s _ts _hn _an _pid _mi _sd ti loc msg) =
  if T.null msg
    then []
    else
      maybe "" ((<> " -") . T.pack . show) ti
      : (msg <> T.replicate (max 0 (60 - T.length msg)) " ")
      : maybe
          []
          (\sl -> pure
            (T.pack $ printf "% 30s line %i"
                             (takeFileName (srcLocFile sl))
                             (srcLocStartLine sl)
            )
          )
          loc

-- | Print the /body/ of a 'LogMessage' without any /tab-stops/
renderLogMessageBody :: LogMessage -> T.Text
renderLogMessageBody (MkLogMessage _f _s _ts _hn _an _pid _mi _sd ti loc msg) =
     maybe "" (\tis -> T.pack (show tis) <> " ") ti
  <> msg
  <> maybe
          ""
          (\sl ->
             T.pack $ printf " at %s:%i"
                             (takeFileName (srcLocFile sl))
                             (srcLocStartLine sl)
          )
          loc
