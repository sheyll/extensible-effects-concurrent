{-# LANGUAGE QuantifiedConstraints #-}
-- | This modules contains RFC 5434 inspired logging data types for log events.
module Control.Eff.Log.Message
  ( -- * Log Event Data Type
    LogEvent(..)
   -- ** Field Accessors
  , logEventFacility
  , logEventSeverity
  , logEventTimestamp
  , logEventHostname
  , logEventAppName
  , logEventProcessId
  , logEventMessageId
  , logEventStructuredData
  , logEventSrcLoc
  , logEventThreadId
  , logEventMessage

  -- *** IO Based 'LogEvent' Modification
  , setCallStack
  , prefixLogEventsWith
  , setLogEventsTimestamp
  , setLogEventsThreadId
  , setLogEventsHostname

  -- ** Log Event Construction
  , errorMessage
  , infoMessage
  , debugMessage

  -- *** Log Message Texts
  , LogMsg(..)
  , fromLogMsg
  , ToLogMsg(..)
  , packLogMsg
  , ToTypeLogMsg(..)

  -- *** IO Based Constructor
  , errorMessageIO
  , infoMessageIO
  , debugMessageIO

  -- * 'LogEvent' Predicates #PredefinedPredicates#
  -- $PredefinedPredicates
  , LogPredicate
  , allLogEvents
  , noLogEvents
  , logEventSeverityIs
  , logEventSeverityIsAtLeast
  , logEventMessageStartsWith
  , discriminateByAppName

  -- ** RFC-5424 Structured Data
  , StructuredDataElement(..)
  , SdParameter(..)
  , sdElementId
  , sdElementParameters

  -- * RFC 5424 Severity
  , Severity(fromSeverity)
  , severityToText
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
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Hashable
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
  MkLogEvent { _logEventFacility :: !Facility
             , _logEventSeverity :: !Severity
             , _logEventTimestamp :: (Maybe UTCTime)
             , _logEventHostname :: (Maybe T.Text)
             , _logEventAppName :: (Maybe T.Text)
             , _logEventProcessId :: (Maybe T.Text)
             , _logEventMessageId :: (Maybe T.Text)
             , _logEventStructuredData :: [StructuredDataElement]
             , _logEventThreadId :: (Maybe ThreadId)
             , _logEventSrcLoc :: (Maybe SrcLoc)
             , _logEventMessage :: LogMsg
             }
  deriving (Eq, Generic)

instance Default LogEvent where
  def = MkLogEvent def def def def def def def def def def (packLogMsg "")

-- | This instance is __only__ supposed to be used for unit tests and debugging.
instance Show LogEvent where
  show = T.unpack . _fromLogMsg . _logEventMessage

instance NFData LogEvent

-- | Convert a 'String' to a 'LogMsg'.
--
-- @since 1.0.0
packLogMsg :: String -> LogMsg
packLogMsg = MkLogMsg . T.pack

-- | The main, human readable, log message text.
--
-- A newtype wrapper around 'T.Text'.
--
-- @since 1.0.0
newtype LogMsg = MkLogMsg { _fromLogMsg :: T.Text }
  deriving (Eq, Ord, NFData, Generic, Semigroup, Monoid, Hashable)

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

-- | Convert a 'Severity' to 'T.Text'
--
-- @since 1.0.0
severityToText :: Severity -> T.Text
severityToText (Severity 1) = T.pack "ALERT    "
severityToText (Severity 2) = T.pack "CRITICAL "
severityToText (Severity 3) = T.pack "ERROR    "
severityToText (Severity 4) = T.pack "WARNING  "
severityToText (Severity 5) = T.pack "NOTICE   "
severityToText (Severity 6) = T.pack "INFO     "
severityToText (Severity x) | x <= 0    = T.pack "EMERGENCY"
                            | otherwise = T.pack "DEBUG    "

--  *** Severities

-- | Smart constructor for the RFC-5424 __emergency__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __0__.
-- See 'logEventSeverity'.
emergencySeverity :: Severity
emergencySeverity = Severity 0

-- | Smart constructor for the RFC-5424 __alert__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __1__.
-- See 'logEventSeverity'.
alertSeverity :: Severity
alertSeverity = Severity 1

-- | Smart constructor for the RFC-5424 __critical__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __2__.
-- See 'logEventSeverity'.
criticalSeverity :: Severity
criticalSeverity = Severity 2

-- | Smart constructor for the RFC-5424 __error__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __3__.
-- See 'logEventSeverity'.
errorSeverity :: Severity
errorSeverity = Severity 3

-- | Smart constructor for the RFC-5424 __warning__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __4__.
-- See 'logEventSeverity'.
warningSeverity :: Severity
warningSeverity = Severity 4

-- | Smart constructor for the RFC-5424 __notice__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __5__.
-- See 'logEventSeverity'.
noticeSeverity :: Severity
noticeSeverity = Severity 5

-- | Smart constructor for the RFC-5424 __informational__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __6__.
-- See 'logEventSeverity'.
informationalSeverity :: Severity
informationalSeverity = Severity 6

-- | Smart constructor for the RFC-5424 __debug__ 'LogEvent' 'Severity'.
-- This corresponds to the severity value __7__.
-- See 'logEventSeverity'.
debugSeverity :: Severity
debugSeverity = Severity 7

instance Default Severity where
  def = debugSeverity


-- | An rfc 5424 facility
newtype Facility = Facility {fromFacility :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Smart constructor for the RFC-5424 'LogEvent' facility @kernelMessages@.
-- See 'logEventFacility'.
kernelMessages :: Facility
kernelMessages = Facility 0

-- | Smart constructor for the RFC-5424 'LogEvent' facility @userLevelMessages@.
-- See 'logEventFacility'.
userLevelMessages :: Facility
userLevelMessages = Facility 1

-- | Smart constructor for the RFC-5424 'LogEvent' facility @mailSystem@.
-- See 'logEventFacility'.
mailSystem :: Facility
mailSystem = Facility 2

-- | Smart constructor for the RFC-5424 'LogEvent' facility @systemDaemons@.
-- See 'logEventFacility'.
systemDaemons :: Facility
systemDaemons = Facility 3

-- | Smart constructor for the RFC-5424 'LogEvent' facility @securityAuthorizationMessages4@.
-- See 'logEventFacility'.
securityAuthorizationMessages4 :: Facility
securityAuthorizationMessages4 = Facility 4

-- | Smart constructor for the RFC-5424 'LogEvent' facility @linePrinterSubsystem@.
-- See 'logEventFacility'.
linePrinterSubsystem :: Facility
linePrinterSubsystem = Facility 6

-- | Smart constructor for the RFC-5424 'LogEvent' facility @networkNewsSubsystem@.
-- See 'logEventFacility'.
networkNewsSubsystem :: Facility
networkNewsSubsystem = Facility 7

-- | Smart constructor for the RFC-5424 'LogEvent' facility @uucpSubsystem@.
-- See 'logEventFacility'.
uucpSubsystem :: Facility
uucpSubsystem = Facility 8

-- | Smart constructor for the RFC-5424 'LogEvent' facility @clockDaemon@.
-- See 'logEventFacility'.
clockDaemon :: Facility
clockDaemon = Facility 9

-- | Smart constructor for the RFC-5424 'LogEvent' facility @securityAuthorizationMessages10@.
-- See 'logEventFacility'.
securityAuthorizationMessages10 :: Facility
securityAuthorizationMessages10 = Facility 10

-- | Smart constructor for the RFC-5424 'LogEvent' facility @ftpDaemon@.
-- See 'logEventFacility'.
ftpDaemon :: Facility
ftpDaemon = Facility 11

-- | Smart constructor for the RFC-5424 'LogEvent' facility @ntpSubsystem@.
-- See 'logEventFacility'.
ntpSubsystem :: Facility
ntpSubsystem = Facility 12

-- | Smart constructor for the RFC-5424 'LogEvent' facility @logAuditFacility@.
-- See 'logEventFacility'.
logAuditFacility :: Facility
logAuditFacility = Facility 13

-- | Smart constructor for the RFC-5424 'LogEvent' facility @logAlertFacility@.
-- See 'logEventFacility'.
logAlertFacility :: Facility
logAlertFacility = Facility 14

-- | Smart constructor for the RFC-5424 'LogEvent' facility @clockDaemon2@.
-- See 'logEventFacility'.
clockDaemon2 :: Facility
clockDaemon2 = Facility 15

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local0@.
-- See 'logEventFacility'.
local0 :: Facility
local0 = Facility 16

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local1@.
-- See 'logEventFacility'.
local1 :: Facility
local1 = Facility 17

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local2@.
-- See 'logEventFacility'.
local2 :: Facility
local2 = Facility 18

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local3@.
-- See 'logEventFacility'.
local3 :: Facility
local3 = Facility 19

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local4@.
-- See 'logEventFacility'.
local4 :: Facility
local4 = Facility 20

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local5@.
-- See 'logEventFacility'.
local5 :: Facility
local5 = Facility 21

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local6@.
-- See 'logEventFacility'.
local6 :: Facility
local6 = Facility 22

-- | Smart constructor for the RFC-5424 'LogEvent' facility @local7@.
-- See 'logEventFacility'.
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
logEventTimestamp
  :: Functor f
  => (Maybe UTCTime -> f (Maybe UTCTime))
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'ThreadId' of a 'LogEvent'
-- The function 'setLogEventsThreadId' can be used to set the field.
logEventThreadId
  :: Functor f
  => (Maybe ThreadId -> f (Maybe ThreadId))
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'StructuredDataElement' of a 'LogEvent'
logEventStructuredData
  :: Functor f
  => ([StructuredDataElement] -> f [StructuredDataElement])
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'SrcLoc' of a 'LogEvent'
logEventSrcLoc
  :: Functor f
  => (Maybe SrcLoc -> f (Maybe SrcLoc))
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'Severity' of a 'LogEvent'
logEventSeverity
  :: Functor f => (Severity -> f Severity) -> LogEvent -> f LogEvent

-- | A lens for a user defined of /process/ id of a 'LogEvent'
logEventProcessId
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogEvent
  -> f LogEvent

-- | A lens for a user defined /message id/ of a 'LogEvent'
logEventMessageId
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogEvent
  -> f LogEvent

-- | A lens for the user defined textual message of a 'LogEvent'
logEventMessage :: Functor f => (LogMsg -> f LogMsg) -> LogEvent -> f LogEvent

-- | A lens for the hostname of a 'LogEvent'
-- The function 'setLogEventsHostname' can be used to set the field.
logEventHostname
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogEvent
  -> f LogEvent

-- | A lens for the 'Facility' of a 'LogEvent'
logEventFacility
  :: Functor f => (Facility -> f Facility) -> LogEvent -> f LogEvent

-- | A lens for the RFC 5424 /application/ name of a 'LogEvent'
--
-- One useful pattern for using this field, is to implement log filters that allow
-- info and debug message from the application itself while only allowing warning and error
-- messages from third party libraries:
--
-- > debugLogsForAppName myAppName lm =
-- >   view logEventAppName lm == Just myAppName || logEventSeverityIsAtLeast warningSeverity lm
--
-- This concept is also implemented in 'discriminateByAppName'.
logEventAppName
  :: Functor f
  => (Maybe T.Text -> f (Maybe T.Text))
  -> LogEvent
  -> f LogEvent

-- | Put the source location of the given callstack in 'logEventSrcLoc'
setCallStack :: CallStack -> LogEvent -> LogEvent
setCallStack cs m = case getCallStack cs of
  []              -> m
  (_, srcLoc) : _ -> m & logEventSrcLoc ?~ srcLoc

-- | Prefix the 'logEventMessage'.
prefixLogEventsWith :: LogMsg -> LogEvent -> LogEvent
prefixLogEventsWith = over logEventMessage . (<>)

-- | An IO action that sets the current UTC time in 'logEventTimestamp'.
setLogEventsTimestamp :: LogEvent -> IO LogEvent
setLogEventsTimestamp m = if isNothing (m ^. logEventTimestamp)
  then do
    now <- getCurrentTime
    return (m & logEventTimestamp ?~ now)
  else return m

-- | An IO action appends the the 'ThreadId' of the calling process (see 'myThreadId')
-- to 'logEventMessage'.
setLogEventsThreadId :: LogEvent -> IO LogEvent
setLogEventsThreadId m = if isNothing (m ^. logEventThreadId)
  then do
    t <- myThreadId
    return (m & logEventThreadId ?~ t)
  else return m

-- | An IO action that sets the current hosts fully qualified hostname in 'logEventHostname'.
setLogEventsHostname :: LogEvent -> IO LogEvent
setLogEventsHostname m = if isNothing (m ^. logEventHostname)
  then do
    fqdn <- Network.getHostName
    return (m & logEventHostname ?~ T.pack fqdn)
  else return m

-- | Construct a 'LogEvent' with 'errorSeverity'
errorMessage :: HasCallStack => LogMsg -> LogEvent
errorMessage m = withFrozenCallStack
  (def & logEventSeverity .~ errorSeverity & logEventMessage .~ m & setCallStack callStack)

-- | Construct a 'LogEvent' with 'informationalSeverity'
infoMessage :: HasCallStack => LogMsg -> LogEvent
infoMessage m = withFrozenCallStack
  (  def
  &  logEventSeverity
  .~ informationalSeverity
  &  logEventMessage
  .~ m
  &  setCallStack callStack
  )

-- | Construct a 'LogEvent' with 'debugSeverity'
debugMessage :: HasCallStack => LogMsg -> LogEvent
debugMessage m = withFrozenCallStack
  (def & logEventSeverity .~ debugSeverity & logEventMessage .~ m & setCallStack callStack)

-- | Construct a 'LogEvent' with 'errorSeverity'
errorMessageIO :: (HasCallStack, MonadIO m) => LogMsg -> m LogEvent
errorMessageIO =
  withFrozenCallStack
    $ (liftIO . setLogEventsThreadId >=> liftIO . setLogEventsTimestamp)
    . errorMessage

-- | Construct a 'LogEvent' with 'informationalSeverity'
infoMessageIO :: (HasCallStack, MonadIO m) => LogMsg -> m LogEvent
infoMessageIO =
  withFrozenCallStack
    $ (liftIO . setLogEventsThreadId >=> liftIO . setLogEventsTimestamp)
    . infoMessage

-- | Construct a 'LogEvent' with 'debugSeverity'
debugMessageIO :: (HasCallStack, MonadIO m) => LogMsg -> m LogEvent
debugMessageIO =
  withFrozenCallStack
    $ (liftIO . setLogEventsThreadId >=> liftIO . setLogEventsTimestamp)
    . debugMessage

makeLensesWith (lensRules & generateSignatures .~ False) ''LogMsg

-- | A lens (iso) to access the 'T.Text' of a 'LogMsg'.
--
-- @since 1.0.0
fromLogMsg :: Iso' LogMsg T.Text

-- | A type class to convert a type to a log message
--
-- @since 1.0.0
class ToLogMsg a where
  toLogMsg :: a -> LogMsg

instance ToLogMsg String where
  toLogMsg = MkLogMsg . fromString

-- instance ToLogMsg T.Text where
--   toLogMsg = MkLogMsg

instance ToLogMsg LogMsg where
  toLogMsg = id


-- | A class for 'LogMsg' values for phantom types, like
-- those used to discern 'Pdu's.
--
-- Instead of a value, a proxy is used to form the log message,
-- which is why the log message generated for instances of this
-- class describe the given type.
--
-- @since 1.0.0
class ToTypeLogMsg (a :: k) where
  -- | Generate a 'LogMsg' for the given proxy value.
  toTypeLogMsg :: proxy a -> LogMsg

-- | The filter predicate for message that shall be logged.
--
-- See "Control.Eff.Log#LogPredicate"
type LogPredicate = LogEvent -> Bool

-- | All messages.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
allLogEvents :: LogPredicate
allLogEvents = const True

-- | No messages.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
noLogEvents :: LogPredicate
noLogEvents = const False

-- | Match 'LogEvent's that have exactly the given severity.
-- See 'logEventSeverityIsAtLeast'.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
logEventSeverityIs :: Severity -> LogPredicate
logEventSeverityIs s = view (logEventSeverity . to (== s))

-- | Match 'LogEvent's that have the given severity __or worse__.
-- See 'logEventSeverityIs'.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
logEventSeverityIsAtLeast :: Severity -> LogPredicate
logEventSeverityIsAtLeast s = view (logEventSeverity . to (<= s))

-- | Match 'LogEvent's whose 'logEventMessage' starts with the given string.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
logEventMessageStartsWith :: LogMsg -> LogPredicate
logEventMessageStartsWith (MkLogMsg prefix) lm = case T.length prefix of
  0         -> True
  prefixLen -> T.take prefixLen (lm ^. logEventMessage . fromLogMsg) == prefix

-- | Apply a 'LogPredicate' based on the 'logEventAppName' and delegate
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
  if view logEventAppName lm == Just appName
    then appPredicate lm
    else otherPredicate lm
