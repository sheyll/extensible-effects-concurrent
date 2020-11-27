{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Rendering functions for 'LogEvent's.
module Control.Eff.Log.MessageRenderer
  ( -- * Log Message Text Rendering
    LogEventReader,
    LogEventPrinter,
    renderLogEventSyslog,
    renderLogEventConsoleLog,
    renderConsoleMinimalisticWide,
    renderRFC3164,
    renderRFC3164WithRFC5424Timestamps,
    renderRFC3164WithTimestamp,
    renderRFC5424,
    renderRFC5424Header,
    renderRFC5424NoLocation,

    -- ** Partial Log Message Text Rendering
    renderSyslogSeverityAndFacility,
    renderLogMessageSrcLoc,
    renderMaybeLogMessageLens,
    renderLogMessageBodyNoLocation,
    renderLogEventBody,
    renderLogEventBodyFixWidth,

    -- ** Timestamp Rendering
    LogEventTimeRenderer (),
    mkLogEventTimeRenderer,
    suppressTimestamp,
    rfc3164Timestamp,
    rfc5424Timestamp,
    rfc5424NoZTimestamp,
  )
where

import Control.Eff.Log.Message
import Control.Lens
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import GHC.Stack
import System.FilePath.Posix
import Text.Printf

-- | 'LogEvent' rendering function
type LogEventReader a = LogEvent -> a

-- | 'LogEvent' to 'T.Text' rendering function.
--
-- @since 0.31.0
type LogEventPrinter = LogEventReader T.Text

-- | A rendering function for the 'logEventTimestamp' field.
newtype LogEventTimeRenderer = MkLogEventTimeRenderer {renderLogEventTime :: UTCTime -> T.Text}

-- | Make a  'LogEventTimeRenderer' using 'formatTime' in the 'defaultLocale'.
mkLogEventTimeRenderer ::
  -- | The format string that is passed to 'formatTime'
  String ->
  LogEventTimeRenderer
mkLogEventTimeRenderer s =
  MkLogEventTimeRenderer (T.pack . formatTime defaultTimeLocale s)

-- | Don't render the time stamp
suppressTimestamp :: LogEventTimeRenderer
suppressTimestamp = MkLogEventTimeRenderer (const "")

-- | Render the time stamp using @"%h %d %H:%M:%S"@
rfc3164Timestamp :: LogEventTimeRenderer
rfc3164Timestamp = mkLogEventTimeRenderer "%h %d %H:%M:%S"

-- | Render the time stamp to @'iso8601DateFormat' (Just "%H:%M:%S%6QZ")@
rfc5424Timestamp :: LogEventTimeRenderer
rfc5424Timestamp =
  mkLogEventTimeRenderer (iso8601DateFormat (Just "%H:%M:%S%6QZ"))

-- | Render the time stamp like 'rfc5424Timestamp' does, but omit the terminal @Z@ character.
rfc5424NoZTimestamp :: LogEventTimeRenderer
rfc5424NoZTimestamp =
  mkLogEventTimeRenderer (iso8601DateFormat (Just "%H:%M:%S%6Q"))

-- | Print the thread id, the message and the source file location, seperated by simple white space.
renderLogEventBody :: LogEventPrinter
renderLogEventBody =
  T.unwords . filter (not . T.null)
    <$> sequence
      [ renderLogMessageBodyNoLocation,
        fromMaybe "" <$> renderLogMessageSrcLoc
      ]

-- | Print the thread id, the message and the source file location, seperated by simple white space.
renderLogMessageBodyNoLocation :: LogEventPrinter
renderLogMessageBodyNoLocation =
  T.unwords . filter (not . T.null)
    <$> sequence
      [ renderShowMaybeLogMessageLens "" logEventThreadId,
        view (logEventMessage . fromLogMsg)
      ]

-- | Print the /body/ of a 'LogEvent' with fix size fields (60) for the message itself
-- and 30 characters for the location
renderLogEventBodyFixWidth :: LogEventPrinter
renderLogEventBodyFixWidth l@(MkLogEvent _f _s _ts _hn _an _pid _mi _sd ti _ (MkLogMsg msg)) =
  T.unwords $
    filter
      (not . T.null)
      [ maybe "" ((<> " ") . T.pack . show) ti,
        msg <> T.replicate (max 0 (60 - T.length msg)) " ",
        fromMaybe "" (renderLogMessageSrcLoc l)
      ]

-- | Render a field of a 'LogEvent' using the corresponsing lens.
renderMaybeLogMessageLens ::
  T.Text -> Getter LogEvent (Maybe T.Text) -> LogEventPrinter
renderMaybeLogMessageLens x l = fromMaybe x . view l

-- | Render a field of a 'LogEvent' using the corresponsing lens.
renderShowMaybeLogMessageLens ::
  Show a =>
  T.Text ->
  Getter LogEvent (Maybe a) ->
  LogEventPrinter
renderShowMaybeLogMessageLens x l =
  renderMaybeLogMessageLens x (l . to (fmap (T.pack . show)))

-- | Render the source location as: @at filepath:linenumber@.
renderLogMessageSrcLoc :: LogEventReader (Maybe T.Text)
renderLogMessageSrcLoc =
  view
    ( logEventSrcLoc
        . to
          ( fmap
              ( \sl ->
                  T.pack $
                    printf
                      "at %s:%i"
                      (takeFileName (srcLocFile sl))
                      (srcLocStartLine sl)
              )
          )
    )

-- | Render the severity and facility as described in RFC-3164
--
-- Render e.g. as @\<192\>@.
--
-- Useful as header for syslog compatible log output.
renderSyslogSeverityAndFacility :: LogEventPrinter
renderSyslogSeverityAndFacility (MkLogEvent !f !s _ _ _ _ _ _ _ _ _) =
  "<" <> T.pack (show (fromSeverity s + fromFacility f * 8)) <> ">"

-- | Render the 'LogEvent' to contain the severity, message, message-id, pid.
--
-- Omit hostname, PID and timestamp.
--
-- Render the header using 'renderSyslogSeverity'
--
-- Useful for logging to @/dev/log@
renderLogEventSyslog :: LogEventPrinter
renderLogEventSyslog l@(MkLogEvent _ _ _ _ an _ mi _ _ _ _) =
  renderSyslogSeverityAndFacility l
    <> ( T.unwords
           . filter (not . T.null)
           $ [ fromMaybe "" an,
               fromMaybe "" mi,
               renderLogEventBody l
             ]
       )

-- | Render a 'LogEvent' human readable, for console logging
renderLogEventConsoleLog :: LogEventPrinter
renderLogEventConsoleLog l@(MkLogEvent _ _ ts _ _ _ _ sd _ _ _) =
  T.unwords $
    filter
      (not . T.null)
      [ severityToText (view logEventSeverity l),
        let p = fromMaybe "no process" (l ^. logEventProcessId)
         in p <> T.replicate (max 0 (55 - T.length p)) " ",
        maybe "" (renderLogEventTime rfc5424Timestamp) ts,
        renderLogEventBodyFixWidth l,
        if null sd then "" else T.concat (renderSdElement <$> sd)
      ]

-- | Render a 'LogEvent' human readable, for console logging
--
-- @since 0.31.0
renderConsoleMinimalisticWide :: LogEventReader T.Text
renderConsoleMinimalisticWide l =
  T.unwords $
    filter
      (not . T.null)
      [ let s = severityToText (view logEventSeverity l)
         in s <> T.replicate (max 0 (15 - T.length s)) " ",
        let p = fromMaybe "no process" (l ^. logEventProcessId)
         in p <> T.replicate (max 0 (55 - T.length p)) " ",
        let msg = l ^. logEventMessage . fromLogMsg
         in msg <> T.replicate (max 0 (100 - T.length msg)) " "
        -- , fromMaybe "" (renderLogMessageSrcLoc l)
      ]

-- | Render a 'LogEvent' according to the rules in the RFC-3164.
renderRFC3164 :: LogEventPrinter
renderRFC3164 = renderRFC3164WithTimestamp rfc3164Timestamp

-- | Render a 'LogEvent' according to the rules in the RFC-3164 but use
-- RFC5424 time stamps.
renderRFC3164WithRFC5424Timestamps :: LogEventPrinter
renderRFC3164WithRFC5424Timestamps =
  renderRFC3164WithTimestamp rfc5424Timestamp

-- | Render a 'LogEvent' according to the rules in the RFC-3164 but use the custom
-- 'LogEventTimeRenderer'.
renderRFC3164WithTimestamp :: LogEventTimeRenderer -> LogEventPrinter
renderRFC3164WithTimestamp renderTime l@(MkLogEvent _ _ ts hn an pid mi _ _ _ _) =
  T.unwords
    . filter (not . T.null)
    $ [ renderSyslogSeverityAndFacility l, -- PRI
        maybe
          "1979-05-29T00:17:17.000001Z"
          (renderLogEventTime renderTime)
          ts,
        fromMaybe "localhost" hn,
        fromMaybe "haskell" an <> maybe "" (("[" <>) . (<> "]")) pid <> ":",
        fromMaybe "" mi,
        renderLogEventBody l
      ]

-- | Render a 'LogEvent' according to the rules in the RFC-5424.
--
-- Equivalent to @'renderRFC5424Header' <> const " " <> 'renderLogEventBody'@.
--
-- @since 0.21.0
renderRFC5424 :: LogEventPrinter
renderRFC5424 = renderRFC5424Header <> const " " <> renderLogEventBody

-- | Render a 'LogEvent' according to the rules in the RFC-5424, like 'renderRFC5424' but
-- suppress the source location information.
--
-- Equivalent to @'renderRFC5424Header' <> const " " <> 'renderLogMessageBodyNoLocation'@.
--
-- @since 0.21.0
renderRFC5424NoLocation :: LogEventPrinter
renderRFC5424NoLocation = renderRFC5424Header <> const " " <> renderLogMessageBodyNoLocation

-- | Render the header and strucuted data of  a 'LogEvent' according to the rules in the RFC-5424, but do not
-- render the 'logEventMessage'.
--
-- @since 0.22.0
renderRFC5424Header :: LogEventPrinter
renderRFC5424Header l@(MkLogEvent _ _ ts hn an pid mi sd _ _ _) =
  T.unwords
    . filter (not . T.null)
    $ [ renderSyslogSeverityAndFacility l <> "1", -- PRI VERSION
        maybe "-" (renderLogEventTime rfc5424Timestamp) ts,
        fromMaybe "-" hn,
        fromMaybe "-" an,
        fromMaybe "-" pid,
        fromMaybe "-" mi,
        structuredData
      ]
  where
    structuredData = if null sd then "-" else T.concat (renderSdElement <$> sd)

renderSdElement :: StructuredDataElement -> T.Text
renderSdElement (SdElement sdId params) =
  "[" <> sdName sdId
    <> if null params
      then ""
      else " " <> T.unwords (renderSdParameter <$> params) <> "]"

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
  '"' -> "\\\""
  '\\' -> "\\\\"
  ']' -> "\\]"
  x -> T.singleton x
