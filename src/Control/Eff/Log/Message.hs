-- | An RFC 5434 inspired log message and convenience functions for
-- logging them.
module Control.Eff.Log.Message
  ( -- * Log Message Data Type
    LogMessage(..)
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

  -- *** IO Based 'LogMessage' Modification
  , setCallStack
  , prefixLogMessagesWith
  , setLogMessageTimestamp
  , setLogMessageThreadId
  , setLogMessageHostname

  -- ** Log Message Text Rendering
  , renderRFC5424
  , printLogMessage
  , renderLogMessage

  -- ** Log Message Construction
  , errorMessage
  , infoMessage
  , debugMessage

  -- *** Type Class for Conversion to 'LogMessage'
  , ToLogMessage(..)

  -- *** IO Based Constructor
  , errorMessageIO
  , infoMessageIO
  , debugMessageIO

  -- * 'LogMessage' Predicates #PredefinedPredicates#
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
  , sdName
  , sdParamValue
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
  , Facility(fromFacility)
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
import           Data.String
import           Data.Time.Clock
import           Data.Time.Format
import           GHC.Generics                   hiding (to)
import           GHC.Stack
import           Network.HostName               as Network
import           System.FilePath.Posix
import           Text.Printf

-- | A message data type inspired by the RFC-5424 Syslog Protocol
data LogMessage =
  MkLogMessage { _lmFacility :: !Facility
               , _lmSeverity :: !Severity
               , _lmTimestamp :: !(Maybe UTCTime)
               , _lmHostname :: !(Maybe String)
               , _lmAppName :: !(Maybe String)
               , _lmProcessId :: !(Maybe String)
               , _lmMessageId :: !(Maybe String)
               , _lmStructuredData :: ![StructuredDataElement]
               , _lmThreadId :: !(Maybe ThreadId)
               , _lmSrcLoc :: !(Maybe SrcLoc)
               , _lmMessage :: !String}
  deriving (Eq, Generic)

instance Default LogMessage where
  def = MkLogMessage def def def def def def def def def def ""

instance NFData LogMessage

-- | RFC-5424 defines how structured data can be included in a log message.
data StructuredDataElement =
  SdElement { _sdElementId :: !String
            , _sdElementParameters :: ![SdParameter]}
  deriving (Eq, Ord, Generic)

instance Show StructuredDataElement where
  show (SdElement sdId params) =
    "[" ++ sdName sdId ++ if null params then "" else " " ++ unwords (show <$> params) ++ "]"

instance NFData StructuredDataElement

-- | Component of an RFC-5424 'StructuredDataElement'
data SdParameter =
  MkSdParameter !String !String
  deriving (Eq, Ord, Generic)

instance Show SdParameter where
  show (MkSdParameter k v) = sdName k ++ "=\"" ++ sdParamValue v ++ "\""

-- | Extract the name of an 'SdParameter' the length is cropped to 32 according to RFC 5424.
sdName :: String -> String
sdName = take 32 . filter (\c -> c == '=' || c == ']' || c == ' ' || c == '"')

-- | Extract the value of an 'SdParameter'.
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
sdElementParameters :: Functor f =>
                             ([SdParameter] -> f [SdParameter])
                             -> StructuredDataElement -> f StructuredDataElement

-- | A lens for the key or ID of a group of RFC 5424 key-value pairs.
sdElementId :: Functor f =>
                     (String -> f String)
                     -> StructuredDataElement -> f StructuredDataElement

makeLensesWith (lensRules & generateSignatures .~ False) ''LogMessage

-- | A lens for the UTC time of a 'LogMessage'
-- The function 'setLogMessageTimestamp' can be used to set the field.
lmTimestamp :: Functor f =>
               (Maybe UTCTime -> f (Maybe UTCTime)) -> LogMessage -> f LogMessage

-- | A lens for the 'ThreadId' of a 'LogMessage'
-- The function 'setLogMessageThreadId' can be used to set the field.
lmThreadId :: Functor f =>
              (Maybe ThreadId -> f (Maybe ThreadId)) -> LogMessage -> f LogMessage

-- | A lens for the 'StructuredDataElement' of a 'LogMessage'
lmStructuredData :: Functor f =>
                    ([StructuredDataElement] -> f [StructuredDataElement])
                    -> LogMessage -> f LogMessage

-- | A lens for the 'SrcLoc' of a 'LogMessage'
lmSrcLoc :: Functor f =>
            (Maybe SrcLoc -> f (Maybe SrcLoc)) -> LogMessage -> f LogMessage

-- | A lens for the 'Severity' of a 'LogMessage'
lmSeverity :: Functor f =>
              (Severity -> f Severity) -> LogMessage -> f LogMessage

-- | A lens for a user defined of /process/ id of a 'LogMessage'
lmProcessId :: Functor f =>
               (Maybe String -> f (Maybe String)) -> LogMessage -> f LogMessage

-- | A lens for a user defined /message id/ of a 'LogMessage'
lmMessageId :: Functor f =>
               (Maybe String -> f (Maybe String)) -> LogMessage -> f LogMessage

-- | A lens for the user defined textual message of a 'LogMessage'
lmMessage :: Functor f =>
             (String -> f String) -> LogMessage -> f LogMessage

-- | A lens for the hostname of a 'LogMessage'
-- The function 'setLogMessageHostname' can be used to set the field.
lmHostname :: Functor f =>
              (Maybe String -> f (Maybe String)) -> LogMessage -> f LogMessage

-- | A lens for the 'Facility' of a 'LogMessage'
lmFacility :: Functor f =>
              (Facility -> f Facility) -> LogMessage -> f LogMessage

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
lmAppName :: Functor f =>
             (Maybe String -> f (Maybe String)) -> LogMessage -> f LogMessage

instance Show LogMessage where
  show = unlines . showLmMessage

-- | Print a 'LogMessage' in a very incomplete way, use only for tests and debugging
showLmMessage :: LogMessage -> [String]
showLmMessage (MkLogMessage _f _s _ts _hn _an _pid _mi _sd ti loc msg) =
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


-- | Render a 'LogMessage' human readable.
renderLogMessage :: LogMessage -> String
renderLogMessage l@(MkLogMessage _f s ts hn an pid mi sd _ _ _) =
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
renderRFC5424 l@(MkLogMessage f s ts hn an pid mi sd _ _ _) = unwords
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

-- | Render a 'LogMessage' but set the timestamp and thread id fields.
printLogMessage :: LogMessage -> IO ()
printLogMessage = putStrLn . renderLogMessage

-- | Put the source location of the given callstack in 'lmSrcLoc'
setCallStack :: CallStack -> LogMessage -> LogMessage
setCallStack cs m = case getCallStack cs of
  []              -> m
  (_, srcLoc) : _ -> m & lmSrcLoc ?~ srcLoc

-- | Prefix the 'lmMessage'.
prefixLogMessagesWith :: String -> LogMessage -> LogMessage
prefixLogMessagesWith = over lmMessage . (<>)

-- | An IO action that sets the current UTC time in 'lmTimestamp'.
setLogMessageTimestamp :: LogMessage -> IO LogMessage
setLogMessageTimestamp m = if isNothing (m ^. lmTimestamp)
  then do
    now <- getCurrentTime
    return (m & lmTimestamp ?~ now)
  else return m

-- | An IO action appends the the 'ThreadId' of the calling process (see 'myThreadId')
-- to 'lmMessage'.
setLogMessageThreadId :: LogMessage -> IO LogMessage
setLogMessageThreadId m = if isNothing (m ^. lmThreadId)
  then do
    t <- myThreadId
    return (m & lmThreadId ?~ t)
  else return m

-- | An IO action that sets the current hosts fully qualified hostname in 'lmHostname'.
setLogMessageHostname :: LogMessage -> IO LogMessage
setLogMessageHostname m = if isNothing (m ^. lmTimestamp)
  then do
    fqdn <- Network.getHostName
    return (m & lmHostname ?~ fqdn)
  else return m

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
    $ (liftIO . setLogMessageThreadId >=> liftIO . setLogMessageTimestamp)
    . errorMessage

-- | Construct a 'LogMessage' with 'informationalSeverity'
infoMessageIO :: (HasCallStack, MonadIO m) => String -> m LogMessage
infoMessageIO =
  withFrozenCallStack
    $ (liftIO . setLogMessageThreadId >=> liftIO . setLogMessageTimestamp)
    . infoMessage

-- | Construct a 'LogMessage' with 'debugSeverity'
debugMessageIO :: (HasCallStack, MonadIO m) => String -> m LogMessage
debugMessageIO =
  withFrozenCallStack
    $ (liftIO . setLogMessageThreadId >=> liftIO . setLogMessageTimestamp)
    . debugMessage

-- | Things that can become a 'LogMessage'
class ToLogMessage a where
  -- | Convert the value to a 'LogMessage'
  toLogMessage :: a -> LogMessage

instance ToLogMessage LogMessage where
  toLogMessage = id

instance ToLogMessage String where
  toLogMessage = infoMessage

instance IsString LogMessage where
  fromString = infoMessage

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
-- goto "Control.Eff.Log.Handler#LogPredicate"


-- | The filter predicate for message that shall be logged.
--
-- See "Control.Eff.Log.Handler#LogPredicate"
type LogPredicate = LogMessage -> Bool

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

-- | Match 'LogMessage's that have exactly the given severity.
-- See 'lmSeverityIsAtLeast'.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
lmSeverityIs :: Severity -> LogPredicate
lmSeverityIs s = view (lmSeverity . to (== s))

-- | Match 'LogMessage's that have the given severity __or worse__.
-- See 'lmSeverityIs'.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
lmSeverityIsAtLeast :: Severity -> LogPredicate
lmSeverityIsAtLeast s = view (lmSeverity . to (<= s))

-- | Match 'LogMessage's whose 'lmMessage' starts with the given string.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
lmMessageStartsWith :: String -> LogPredicate
lmMessageStartsWith prefix lm =
  case length prefix of
    0         -> True
    prefixLen -> take prefixLen (lm ^. lmMessage) == prefix

-- | Apply a 'LogPredicate' based on the 'lmAppName' and delegate
-- to one of two 'LogPredicate's.
--
-- One useful application for this is to allow info and debug message
-- from one application, e.g. the current application itself,
-- while at the same time allowing only warning and error messages
-- from third party libraries.
--
-- See "Control.Eff.Log.Message#PredefinedPredicates" for more predicates.
discriminateByAppName :: String -> LogPredicate -> LogPredicate -> LogPredicate
discriminateByAppName appName appPredicate otherPredicate lm =
   if view lmAppName lm == Just appName
      then appPredicate lm
      else otherPredicate lm
