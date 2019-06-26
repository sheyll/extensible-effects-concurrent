{-# LANGUAGE UndecidableInstances #-}
-- | Capture 'LogMessage's to a 'Writer'.
--
-- See 'Control.Eff.Log.Examples.exampleLogCapture'
module Control.Eff.LogWriter.Capture
  ( captureLogWriter
  , CaptureLogs
  , CaptureLogWriter
  , runCaptureLogWriter
  )
where

import           Control.Eff                   as Eff
import           Control.Eff.Log
import           Control.Eff.Writer.Strict      ( Writer
                                                , tell
                                                , runListWriter
                                                )

-- | A 'LogWriter' monad that provides pure logging by capturing via the 'Writer' effect.
--
-- See 'Control.Eff.Log.Examples.exampleLogCapture'
captureLogWriter :: LogWriter CaptureLogWriter
captureLogWriter = MkLogWriter (MkCaptureLogs . tell)

-- | A 'LogWriter' monad that provides pure logging by capturing via the 'Writer' effect.
type CaptureLogs a = LogWriterM CaptureLogWriter a

-- | Run a 'Writer' for 'LogMessage's.
--
-- Such a 'Writer' is needed to handle 'CaptureLogWriter'
runCaptureLogWriter
  :: Eff (CaptureLogWriter ': e) a -> Eff e (a, [LogMessage])
runCaptureLogWriter = runListWriter

-- | Alias for the 'Writer' that contains the captured 'LogMessage's from 'CaptureLogs'.
type CaptureLogWriter = Writer LogMessage
