module Control.Eff.Log.MessageFactory
  ( mkLogMsg
  , MessageFactory()
  , MessageFactoryReader
  , withLogMessageFactory
  , composeMessageFactories
  , localMessageFactory
  )
where

import           Control.Eff
import           Control.Eff.Log         hiding ( Severity )
import           Control.Eff.Reader.Strict
import           Control.Eff.Lift
import           Control.Monad.IO.Class
import           Data.Default

newtype MessageFactory m =
  MessageFactory { runMessageFactory :: IO m}

type MessageFactoryReader m = Reader (MessageFactory m)

mkLogMsg
  :: forall m io e
   . ( Member (Logs m) e
     , MonadIO io
     , SetMember Lift (Lift io) e
     , Member (MessageFactoryReader m) e
     )
  => (m -> m)
  -> Eff e ()
mkLogMsg f = ask >>= lift . liftIO . runMessageFactory >>= logMsg . f

localMessageFactory
  :: forall m e a
   . (Member (MessageFactoryReader m) e)
  => IO m
  -> Eff e a
  -> Eff e a
localMessageFactory = local . const . MessageFactory

composeMessageFactories
  :: forall m e a
   . (Member (MessageFactoryReader m) e)
  => (m -> IO m)
  -> Eff e a
  -> Eff e a
composeMessageFactories f2 =
  local (\(MessageFactory f1) -> MessageFactory (f1 >>= f2))

withLogMessageFactory
  :: forall m io e a
   . (Member (Logs m) e, Default m, MonadIO io, SetMember Lift (Lift io) e)
  => Eff (MessageFactoryReader m ': e) a
  -> Eff e a
withLogMessageFactory = runReader (MessageFactory (return def))
