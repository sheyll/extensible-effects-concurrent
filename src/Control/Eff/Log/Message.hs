{-# LANGUAGE QuantifiedConstraints #-}
-- | This modules contains RFC 5434 inspired logging data types for log events.
module Control.Eff.Log.Message
  ( -- * Log Event Data Type
    LogEvent(..)
   -- ** Field Accessors
  , lmFacility
  , lmSeverity
  , lmTimestamp
  , lmHostname
  , lmAppName
  , lmProcessId
  , lmMessageId
  , lmStructuredData
  , lmSrcLoc
  , lmThreadId
  , lmMessage

  -- *** IO Based 'LogEvent' Modification
  , setCallStack
  , prefixLogEventsWith
  , setLogEventsTimestamp
  , setLogEventsThreadId
  , setLogEventsHostname

  -- ** Log Message Construction
  , errorMessage
  , infoMessage
  , debugMessage

  -- *** Type Class for Conversion to 'LogEvent'
  , ToLogEntry(..)

  -- *** IO Based Constructor
  , errorMessageIO
  , infoMessageIO
  , debugMessageIO

  -- * 'LogEvent' Predicates #PredefinedPredicates#
  -- $PredefinedPredicates
  , LogPredicate
  , allLogMessages
  , noLogMessages
  , lmSeverityIs
  , lmSeverityIsAtLeast
  , lmMessageStartsWith
  , discriminateByAppName

  -- ** RFC-5424 Structured Data
  , StructuredDataElement(..)
  , SdParameter(..)
  , sdElementId
  , sdElementParameters

  -- * RFC 5424 Severity
  , Severity(fromSeverity)
  , emergencySeverity
  , alertSeverity
  , criticalSeverity
  , errorSeverity
  , warningSeverity
  , noticeSeverity
  , informationalSeverity
  , debugSeverity

  -- * RFC 5424 Facility
  , Facility (..)
  -- ** Facility Constructors
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
  )
where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad                  ( (>=>) )
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Maybe
import           Data.String                    (IsString(..))
import qualified Data.Text                     as T
import           Data.Time.Clock
import           GHC.Generics            hiding ( to )
import           GHC.Stack
import           Network.HostName              as Network

-- | A data type describing a complete logging event, usually consisting of
-- e.g. a log message, a timestamp and a severity.
-- The fields are modelled to ressamble all fields mentioned for the
-- RFC-5424 Syslog Protocol.
data LogEvent =
  MkLogEvent { _lmFacility :: !Facility
             , _lmSeverity :: !Severity
             , _lmTimestamp :: (Maybe UTCTime)
             , _lmHostname :: (Maybe T.Text)
             , _lmAppName :: (Maybe T.Text)
             , _lmProcessId :: (Maybe T.Text)
             , _lmMessageId :: (Maybe T.Text)
             , _lmStructuredData :: [StructuredDataElement]
             , _lmThreadId :: (Maybe ThreadId)
             , _lmSrcLoc :: (Maybe SrcLoc)
             , _lmMessage :: T.Text
             }
  deriving (Eq, Generic)

instance Default LogEvent where
  def = MkLogEvent def def def def def def def def def def ""

-- | This instance is __only__ supposed to be used for unit tests and debugging.
instance Show LogEvent where
  show = T.unpack . _lmMessage

instance NFData LogEvent

-- | RFC-5424 defines how structured data can be included in a log message.
data StructuredDataElement =
  SdElement { _sdElementId :: !T.Text
            , _sdElementParameters :: ![SdParameter]}
  deriving (Eq, Ord, Generic, Show)

instance NFData StructuredDataElement

-- | Component of an RFC-5424 'StructuredDataElement'
data SdParameter =
  MkSdParameter !T.Text !T.Text
  deriving (Eq, Ord, Generic, Show)

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

-- | Smart constructor for the RFC-5424 __emergency__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __0__.
-- See 'lmSeverity'.
emergencySeverity :: Severity
emergencySeverity = Severity 0

-- | Smart constructor for the RFC-5424 __alert__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __1__.
-- See 'lmSeverity'.
alertSeverity :: Severity
alertSeverity = Severity 1

-- | Smart constructor for the RFC-5424 __critical__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __2__.
-- See 'lmSeverity'.
criticalSeverity :: Severity
criticalSeverity = Severity 2

-- | Smart constructor for the RFC-5424 __error__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __3__.
-- See 'lmSeverity'.
errorSeverity :: Severity
errorSeverity = Severity 3

-- | Smart constructor for the RFC-5424 __warning__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __4__.
-- See 'lmSeverity'.
warningSeverity :: Severity
warningSeverity = Severity 4

-- | Smart constructor for the RFC-5424 __notice__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __5__.
-- See 'lmSeverity'.
noticeSeverity :: Severity
noticeSeverity = Severity 5

-- | Smart constructor for the RFC-5424 __informational__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __6__.
-- See 'lmSeverity'.
informationalSeverity :: Severity
informationalSeverity = Severity 6

-- | Smart constructor for the RFC-5424 __debug__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __7__.
-- See 'lmSeverity'.
debugSeverity :: Severity
debugSeverity = Severity 7

instance Default Severity where
  def = debugSeverity


-- | An rfc 5424 facility
newtype Facility = Facility {fromFacility :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Smart constructor for the RFC-5424 'LogEvent' facility @kernelMessages@.
-- See 'lmFacility'.
kernelMessages :: Facility
kernelMessages = Facility 0

-- | Smart constructor for the RFC-5424 'LogEvent' facility @userLevelMessages@.
-- See 'lmFacility'.
userLevelMessages :: Facility
userLevelMessages = Facility 1

-- | Smart constructor for the RFC-5424 'LogEvent' facility @mailSystem@.
-- See 'lmFacility'.
mailSystem :: Facility
mailSystem = Facility 2

-- | Smart constructor for the RFC-5424 'LogEvent' facility @systemDaemons@.
-- See 'lmFacility'.
systemDaemons :: Facility
systemDaemons = Facility 3

-- | Smart constructor for the RFC-5424 'LogEvent' facility @securityAuthorizationMessages4@.
-- See 'lmFacility'.
securityAuthorizationMessages4 :: Facility
securityAuthorizationMessages4 = Facility 4

-- | Smart constructor for the RFC-5424 'LogEvent' facility @linePrinterSubsystem@.
-- See 'lmFacility'.
linePrinterSubsystem :: Facility
linePrinterSubsystem = Facility 6

-- | Smart constructor for the RFC-5424 'LogEvent' facility @networkNewsSubsystem@.
-- See 'lmFacility'.
networkNewsSubsystem :: Facility
networkNewsSubsystem = Facility 7

-- | Smart constructor for the RFC-5424 'LogEvent' facility @uucpSubsystem@.
-- See 'lmFacility'.
uucpSubsystem :: Facility
uucpSubsystem = Facility 8

-- | Smart constructor for the RFC-5424 'LogEvent' facility @clockDaemon@.
-- See 'lmFacility'.
clockDaemon :: Facility
clockDaemon = Facility 9

-- | Smart constructor for the RFC-5424 'LogEvent' facility @securityAuthorizationMessages10@.
-- See 'lmFacility'.
securityAuthorizationMessages10 :: Facility
securityAuthorizationMessages10 = Facility 10

-- | Smart constructor for the RFC-5424 'LogEvent' facility @ftpDaemon@.
-- See 'lmFacility'.
ftpDaemon :: Facility
ftpDaemon = Facility 11

-- | Smart constructor for the RFC-5424 'LogEvent' facility @ntpSubsystem@.
-- See 'lmFacility'.
ntpSubsystem :: Facility
ntpSubsystem = Facility 12

-- | Smart constructor for the RFC-5424 'LogEvent' facility @logAuditFacility@.
-- See 'lmFacility'.
logAuditFacility :: Facility
logAuditFacility = Facility 13

-- | Smart constructor for the RFC-5424 'LogEvent' facility @logAlertFacility@.
-- See 'lmFacility'.
logAlertFacility :: Facility
logAlertFacility = Facility 14

-- | Smart constructor for the RFC-5424 'LogEvent' facility @clockDaemon2@.
-- See 'lmFacility'.
clockDaemon2 :: Facility
clockDaemon2 = Facility 15

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local0@.
-- See 'lmFacility'.
local0 :: Facility
local0 = Facility 16

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local1@.
-- See 'lmFacility'.
local1 :: Facility
local1 = Facility 17

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local2@.
-- See 'lmFacility'.
local2 :: Facility
local2 = Facility 18

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local3@.
-- See 'lmFacility'.
local3 :: Facility
local3 = Facility 19

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local4@.
-- See 'lmFacility'.
local4 :: Facility
local4 = Facility 20

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local5@.
-- See 'lmFacility'.
local5 :: Facility
local5 = Facility 21

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local6@.
-- See 'lmFacility'.
local6 :: Facility
local6 = Facility 22

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local7@.
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

makeLensesWith (lensRules & generateSignatures .~ False) ''LogEvent

-- | A lens for the UTC time of a 'LogEvent'
-- The function 'setLogEventsTimestamp' can be used to set the field.
lmTimestamp
  :: Functor f
  => (Maybe UTCTime -> f (Maybe UTCTime))
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'ThreadId' of a 'LogEvent'
-- The function 'setLogEventsThreadId' can be used to set the field.
lmThreadId
  :: Functor f
  => (Maybe ThreadId -> f (Maybe ThreadId))
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'StructuredDataElement' of a 'LogEvent'
lmStructuredData
  :: Functor f
  => ([StructuredDataElement] -> f [StructuredDataElement])
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'SrcLoc' of a 'LogEvent'
lmSrcLoc
  :: Functor f
  => (Maybe SrcLoc -> f (Maybe SrcLoc))
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'Severity' of a 'LogEvent'
lmSeverity
  :: Functor f => (Severity -> f Severity) -> LogEvent -> f LogEvent

-- | A lens for a user defined of /process/ id of a 'LogEvent'
lmProcessId
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogEvent
  -> f LogEvent

-- | A lens for a user defined /message id/ of a 'LogEvent'
lmMessageId
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogEvent
  -> f LogEvent

-- | A lens for the user defined textual message of a 'LogEvent'
lmMessage :: Functor f => (T.Text -> f T.Text) -> LogEvent -> f LogEvent

-- | A lens for the hostname of a 'LogEvent'
-- The function 'setLogEventsHostname' can be used to set the field.
lmHostname
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'Facility' of a 'LogEvent'
lmFacility
  :: Functor f => (Facility -> f Facility) -> LogEvent -> f LogEvent

-- | A lens for the RFC 5424 /application/ name of a 'LogEvent'
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
  -> LogEvent
  -> f LogEvent

-- | Put the source location of the given callstack in 'lmSrcLoc'
setCallStack :: CallStack -> LogEvent -> LogEvent
setCallStack cs m = case getCallStack cs of
  []              -> m
  (_, srcLoc) : _ -> m & lmSrcLoc ?~ srcLoc

-- | Prefix the 'lmMessage'.
prefixLogEventsWith :: T.Text -> LogEvent -> LogEvent
prefixLogEventsWith = over lmMessage . (<>)

-- | An IO action that sets the current UTC time in 'lmTimestamp'.
setLogEventsTimestamp :: LogEvent -> IO LogEvent
setLogEventsTimestamp m = if isNothing (m ^. lmTimestamp)
  then do
    now <- getCurrentTime
    return (m & lmTimestamp ?~ now)
  else return m

-- | An IO action appends the the 'ThreadId' of the calling process (see 'myThreadId')
-- to 'lmMessage'.
setLogEventsThreadId :: LogEvent -> IO LogEvent
setLogEventsThreadId m = if isNothing (m ^. lmThreadId)
  then do
    t <- myThreadId
    return (m & lmThreadId ?~ t)
  else return m

-- | An IO action that sets the current hosts fully qualified hostname in 'lmHostname'.
setLogEventsHostname :: LogEvent -> IO LogEvent
setLogEventsHostname m = if isNothing (m ^. lmHostname)
  then do
    fqdn <- Network.getHostName
    return (m & lmHostname ?~ T.pack fqdn)
  else return m

-- | Construct a 'LogEvent' with 'errorSeverity'
errorMessage :: HasCallStack => T.Text -> LogEvent
errorMessage m = withFrozenCallStack
  (def & lmSeverity .~ errorSeverity & lmMessage .~ m & setCallStack callStack)

-- | Construct a 'LogEvent' with 'informationalSeverity'
infoMessage :: HasCallStack => T.Text -> LogEvent
infoMessage m = withFrozenCallStack
  (  def
  &  lmSeverity
  .~ informationalSeverity
  &  lmMessage
  .~ m
  &  setCallStack callStack
  )

-- | Construct a 'LogEvent' with 'debugSeverity'
debugMessage :: HasCallStack => T.Text -> LogEvent
debugMessage m = withFrozenCallStack
  (def & lmSeverity .~ debugSeverity & lmMessage .~ m & setCallStack callStack)

-- | Construct a 'LogEvent' with 'errorSeverity'
errorMessageIO :: (HasCallStack, MonadIO m) => T.Text -> m LogEvent
errorMessageIO =
  withFrozenCallStack
    $ (liftIO . setLogEventsThreadId >=> liftIO . setLogEventsTimestamp)
    . errorMessage

-- | Construct a 'LogEvent' with 'informationalSeverity'
infoMessageIO :: (HasCallStack, MonadIO m) => T.Text -> m LogEvent
infoMessageIO =
  withFrozenCallStack
    $ (liftIO . setLogEventsThreadId >=> liftIO . setLogEventsTimestamp)
    . infoMessage

-- | Construct a 'LogEvent' with 'debugSeverity'
debugMessageIO :: (HasCallStack, MonadIO m) => T.Text -> m LogEvent
debugMessageIO =
  withFrozenCallStack
    $ (liftIO . setLogEventsThreadId >=> liftIO . setLogEventsTimestamp)
    . debugMessage

-- | Things that can become a 'LogEvent'
class ToLogEntry a where
  -- | Convert the value to a 'LogEvent'
  toLogEntry :: a -> LogEvent

instance ToLogEntry LogEvent where
  toLogEntry = id

instance ToLogEntry T.Text where
  toLogEntry = infoMessage

class ToLogMsg a where


newtype EMERGENCY a = EMERGENCY a
newtype ALERT a = ALERT a
newtype CRITICAL a = CRITICAL a
newtype ERROR a = ERROR a
newtype WARNING a = WARNING a
newtype NOTICE a = NOTICE a
newtype INFO a = INFO a
newtype DEBUG a = DEBUG a

-- TODO renamte LogEvent -> LogEntry
-- TODO add newtype LogMessageText

instance ToLogEntry a => ToLogEntry (Severity, a) where
  toLogEntry (s, a) =
      let m = toLogEntry a
      in m & lmSeverity .~ s

instance IsString LogEvent where
  fromString = infoMessage . T.pack

-- $PredefinedPredicates
-- == Log Message Predicates
--
-- These are the predefined 'LogPredicate's:
--
--  * 'allLogMessages'
--  * 'noLogMessages'
--  * 'lmSeverityIsAtLeast'
--  * 'lmSeverityIs'
--  * 'lmMessageStartsWith'
--  * 'discriminateByAppName'
--
-- To find out how to use these predicates,
-- goto "Control.Eff.Log#LogPredicate"


-- | The filter predicate for message that shall be logged.
--
-- See "Control.Eff.Log#LogPredicate"
type LogPredicate = LogEvent -> Bool

-- | All messages.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
allLogMessages :: LogPredicate
allLogMessages = const True

-- | No messages.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
noLogMessages :: LogPredicate
noLogMessages = const False

-- | Match 'LogEvent's that have exactly the given severity.
-- See 'lmSeverityIsAtLeast'.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
lmSeverityIs :: Severity -> LogPredicate
lmSeverityIs s = view (lmSeverity . to (== s))

-- | Match 'LogEvent's that have the given severity __or worse__.
-- See 'lmSeverityIs'.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
lmSeverityIsAtLeast :: Severity -> LogPredicate
lmSeverityIsAtLeast s = view (lmSeverity . to (<= s))

-- | Match 'LogEvent's whose 'lmMessage' starts with the given string.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
lmMessageStartsWith :: T.Text -> LogPredicate
lmMessageStartsWith prefix lm = case T.length prefix of
  0         -> True
  prefixLen -> T.take prefixLen (lm ^. lmMessage) == prefix

-- | Apply a 'LogPredicate' based on the 'lmAppName' and delegate
-- to one of two 'LogPredicate's.
--
-- One useful application for this is to allow info and debug message
-- from one application, e.g. the current application itself,
-- while at the same time allowing only warning and error messages
-- from third party libraries.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
discriminateByAppName :: T.Text -> LogPredicate -> LogPredicate -> LogPredicate
discriminateByAppName appName appPredicate otherPredicate lm =
  if view lmAppName lm == Just appName
    then appPredicate lm
    else otherPredicate lm
