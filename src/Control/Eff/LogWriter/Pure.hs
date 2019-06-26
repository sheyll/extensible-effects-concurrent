module Control.Eff.LogWriter.Pure where


-- ** Pure Log Writers

-- | A phantom type for the 'HandleLogWriter' class for /pure/ 'LogWriter's
type PureLogWriter a = LogWriterM Logs a

-- | A 'LogWriter' monad for 'Debug.Trace' based pure logging.
instance HandleLogWriter Logs where
  newtype instance LogWriterM Logs a = MkPureLogWriter { runPureLogWriter :: Identity a }
    deriving (Applicative, Functor, Monad)
  handleLogWriterEffect lw = return (force (runIdentity (force (runPureLogWriter lw))))


