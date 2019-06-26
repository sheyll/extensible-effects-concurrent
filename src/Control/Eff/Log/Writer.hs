{-# LANGUAGE UndecidableInstances #-}
-- | The 'LogWriter' type encapsulates an effectful function to write 'LogMessage's.
--
-- Used in conjunction with the 'HandleLogWriter' class, it
-- can be used to write messages from within an effectful
-- computation.
module Control.Eff.Log.Writer
  (
  -- * 'LogWriter' Definition
    LogWriter(..)
  -- * LogWriter Reader Effect
  , LogWriterReader
  , localLogWriterReader
  , askLogWriter
  , runLogWriterReader
  -- * LogWriter Handler Class
  , HandleLogWriter(..)
  , noOpLogWriter
  , PureLogWriter(..)
  -- ** Writer Combinator
  -- *** Pure Writer Combinator
  , filteringLogWriter
  , mappingLogWriter
  -- *** Impure Writer Combinator
  , mappingLogWriterM
  )
where

import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.Log.Message
import           Data.Default
import           Data.Function                  ( fix )
import           Data.Functor.Identity          ( Identity )
import           Control.DeepSeq                ( force )
import           Control.Monad                  ( (>=>)
                                                , when
                                                )
import           Control.Monad.Base             ( MonadBase() )
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Trans.Control    ( MonadBaseControl
                                                  ( restoreM
                                                  , liftBaseWith
                                                  , StM
                                                  )
                                                )
import           Data.Kind
import           Control.Lens

-- | A function that takes a log message and returns an effect that
-- /logs/ the message.
newtype LogWriter writerM = MkLogWriter
  { runLogWriter :: LogMessage -> writerM ()
  }

instance Applicative w => Default (LogWriter w) where
  def = MkLogWriter (const (pure ()))

-- | Provide the 'LogWriter'
--
-- Exposed for custom extensions, if in doubt use 'withLogging'.
runLogWriterReader :: LogWriter h -> Eff (LogWriterReader h ': e) a -> Eff e a
runLogWriterReader e m = fix (handle_relay (\x _ -> return x)) m e

-- | Get the current 'LogWriter'.
askLogWriter
  :: SetMember LogWriterReader (LogWriterReader h) e => Eff e (LogWriter h)
askLogWriter = send AskLogWriter

-- | Modify the current 'LogWriter'.
localLogWriterReader
  :: forall h e a
   . SetMember LogWriterReader (LogWriterReader h) e
  => (LogWriter h -> LogWriter h)
  -> Eff e a
  -> Eff e a
localLogWriterReader f m =
  f
    <$> askLogWriter
    >>= fix (respond_relay @(LogWriterReader h) (\x _ -> return x)) m

-- | A Reader specialized for 'LogWriter's
--
-- The existing @Reader@ couldn't be used together with 'SetMember', so this
-- lazy reader was written, specialized to reading 'LogWriter'.
data LogWriterReader h v where
  AskLogWriter ::LogWriterReader h (LogWriter h)

instance Handle (LogWriterReader h) e a (LogWriter h -> k) where
  handle k q AskLogWriter lw = k (q ^$ lw) lw

instance forall h m r. (MonadBase m m, LiftedBase m r)
  => MonadBaseControl m (Eff (LogWriterReader h ': r)) where
  type StM (Eff (LogWriterReader h ': r)) a =  StM (Eff r) a
  liftBaseWith f = do
    lf <- askLogWriter
    raise (liftBaseWith (\runInBase -> f (runInBase . runLogWriterReader lf)))
  restoreM = raise . restoreM

instance (LiftedBase m e, Catch.MonadThrow (Eff e))
  => Catch.MonadThrow (Eff (LogWriterReader h ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance (Applicative m, LiftedBase m e, Catch.MonadCatch (Eff e))
  => Catch.MonadCatch (Eff (LogWriterReader h ': e)) where
  catch effect handler = do
    lf <- askLogWriter
    let lower         = runLogWriterReader lf
        nestedEffects = lower effect
        nestedHandler exception = lower (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance (Applicative m, LiftedBase m e, Catch.MonadMask (Eff e))
  => Catch.MonadMask (Eff (LogWriterReader h ': e)) where
  mask maskedEffect = do
    lf <- askLogWriter
    let lower :: Eff (LogWriterReader h ': e) a -> Eff e a
        lower = runLogWriterReader lf
    raise
      (Catch.mask
        (\nestedUnmask -> lower (maskedEffect (raise . nestedUnmask . lower)))
      )
  uninterruptibleMask maskedEffect = do
    lf <- askLogWriter
    let lower :: Eff (LogWriterReader h ': e) a -> Eff e a
        lower = runLogWriterReader lf
    raise
      (Catch.uninterruptibleMask
        (\nestedUnmask -> lower (maskedEffect (raise . nestedUnmask . lower)))
      )
  generalBracket acquire release useIt = do
    lf <- askLogWriter
    let lower :: Eff (LogWriterReader h ': e) a -> Eff e a
        lower = runLogWriterReader lf
    raise
      (Catch.generalBracket (lower acquire)
                            (((.) . (.)) lower release)
                            (lower . useIt)
      )

-- * 'LogWriter' Zoo

-- | The instances of this class are the monads that define (side-) effect(s) of writting logs.
class HandleLogWriter (writer :: Type -> Type) where
  -- | The 'Eff'ects required by the 'handleLogWriterEffect' method.
  --
  -- @since 0.29.1
  type family LogWriterEffects writer :: [Type -> Type]
  type instance LogWriterEffects writer = '[writer]

  -- | Run the side effect of a 'LogWriter' in a compatible 'Eff'.
  handleLogWriterEffect :: (LogWriterEffects writer <:: e) => writer () -> Eff e ()

  -- | Write a message using the 'LogWriter' found in the environment.
  --
  -- The semantics of this function are a combination of 'runLogWriter' and 'handleLogWriterEffect',
  -- with the 'LogWriter' read from a 'LogWriterReader'.
  liftWriteLogMessage :: ( SetMember LogWriterReader (LogWriterReader writer) e
                         , LogWriterEffects writer <:: e
                         )
                      => LogMessage
                      -> Eff e ()
  liftWriteLogMessage m = do
    w <- askLogWriter
    handleLogWriterEffect (runLogWriter w m)


instance HandleLogWriter IO where
  type instance LogWriterEffects IO = '[Lift IO]
  handleLogWriterEffect = send . Lift

-- ** Pure Log Writers

-- | A phantom type for the 'HandleLogWriter' class for /pure/ 'LogWriter's
newtype PureLogWriter a = MkPureLogWriter { runPureLogWriter :: Identity a }
  deriving (Applicative, Functor, Monad)

-- | A 'LogWriter' monad for 'Debug.Trace' based pure logging.
instance HandleLogWriter PureLogWriter where
  type instance LogWriterEffects PureLogWriter = '[]
  handleLogWriterEffect lw = return (force (runIdentity (force (runPureLogWriter lw))))

-- | This 'LogWriter' will discard all messages.
--
-- NOTE: This is just an alias for 'def'
noOpLogWriter :: Applicative m => LogWriter m
noOpLogWriter = def

-- | A 'LogWriter' that applies a predicate to the 'LogMessage' and delegates to
-- to the given writer of the predicate is satisfied.
filteringLogWriter :: Monad e => LogPredicate -> LogWriter e -> LogWriter e
filteringLogWriter p lw =
  MkLogWriter (\msg -> when (p msg) (runLogWriter lw msg))

-- | A 'LogWriter' that applies a function to the 'LogMessage' and delegates the result to
-- to the given writer.
mappingLogWriter :: (LogMessage -> LogMessage) -> LogWriter e -> LogWriter e
mappingLogWriter f lw = MkLogWriter (runLogWriter lw . f)

-- | Like 'mappingLogWriter' allow the function that changes the 'LogMessage' to have effects.
mappingLogWriterM
  :: Monad e => (LogMessage -> e LogMessage) -> LogWriter e -> LogWriter e
mappingLogWriterM f lw = MkLogWriter (f >=> runLogWriter lw)
