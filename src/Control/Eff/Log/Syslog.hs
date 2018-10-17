-- | An RFC 5434 inspired log message and convenience functions for
-- logging them. TODO document
module Control.Eff.Log.Syslog
  ( Severity(fromSeverity)
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
  , LogMessage(..)
  , lmFacility
  , lmSeverity
  , lmTimestamp
  , lmHostname
  , lmAppname
  , lmProcessId
  , lmMessageId
  , lmStructuredData
  , lmSrcLoc
  , lmMessage
  , StructuredDataElement(..)
  , sdElementId
  , sdElementParameters
  , addSyslogTimestamps
  , syslogMsg
  , withSyslog
  )
where

import           Data.Time.Clock
import           Data.Time.Format
import           Control.Lens
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Log         hiding ( Severity )
import           GHC.Stack
import           Data.Default
import           Control.Eff.Log.MessageFactory
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Maybe
import           GHC.Generics

data LogMessage =
  LogMessage { _lmFacility :: Facility
             , _lmSeverity :: Severity
             , _lmTimestamp :: Maybe UTCTime
             , _lmHostname :: Maybe String
             , _lmAppname :: Maybe String
             , _lmProcessId :: Maybe String
             , _lmMessageId :: Maybe String
             , _lmStructuredData :: [StructuredDataElement]
             , _lmSrcLoc :: Maybe SrcLoc
             , _lmMessage :: String}
  deriving (Eq, Generic)

instance Show LogMessage where
  show (LogMessage f s ts hn an pid mi sd loc msg) =
    unwords
      ( ("<" ++ show (fromSeverity s + fromFacility f * 8) ++ ">" ++ "1")
     : maybe "-" (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))) ts
     : fromMaybe "-" hn
     : fromMaybe "-" an
     : fromMaybe "-" pid
     : fromMaybe "-" mi
     : (if null sd then "-" else show sd)
     : (if null msg then [] else msg : maybe [] (pure . prettySrcLoc) loc))

instance NFData LogMessage

data StructuredDataElement =
  StructuredDataElement {_sdElementId :: String
                        ,_sdElementParameters :: [(String, Maybe String)]}
  deriving (Eq, Ord, Show, Generic)

instance NFData StructuredDataElement

instance Default LogMessage where
  def = LogMessage local7 debugSeverity def def def def def def def ""

-- | An rfc 5424 severity
newtype Severity = Severity {fromSeverity :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)

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

-- | An rfc 5424 facility
newtype Facility = Facility {fromFacility :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)

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

makeLenses ''StructuredDataElement
makeLenses ''LogMessage

addSyslogTimestamps
  :: ( MonadIO io
     , SetMember Lift (Lift io) e
     , Member (Logs LogMessage) e
     , Member (MessageFactoryReader LogMessage) e
     )
  => Eff e a
  -> Eff e a
addSyslogTimestamps = composeMessageFactories $ \m -> do
  now <- getCurrentTime
  return (m & lmTimestamp ?~ now)

syslogMsg
  :: ( HasCallStack
     , MonadIO io
     , SetMember Lift (Lift io) e
     , Member (Logs LogMessage) e
     , Member (MessageFactoryReader LogMessage) e
     )
  => (LogMessage -> LogMessage)
  -> Eff e ()
syslogMsg f = mkLogMsg (f . setCallStack callStack)
 where
  setCallStack :: CallStack -> LogMessage -> LogMessage
  setCallStack cs m = case getCallStack cs of
    []              -> m
    (_, srcLoc) : _ -> m & lmSrcLoc ?~ srcLoc

withSyslog
  :: forall io e a
   . ( Member (Logs LogMessage) e
     , Default LogMessage
     , MonadIO io
     , SetMember Lift (Lift io) e
     )
  => Eff (MessageFactoryReader LogMessage ': e) a
  -> Eff e a
withSyslog = withLogMessageFactory . addSyslogTimestamps
