{-# LANGUAGE UndecidableInstances #-}

module GenServerTests
  ( test_genServer,
  )
where

import Common
import Control.Eff.Concurrent.Protocol.Broker as Broker
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as E
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as S
import Data.Typeable (Typeable)
import Data.Coerce (coerce)

-- ------------------------------

data Small deriving (Typeable)

instance HasPdu Small where
  data Pdu Small r where
    SmallCall :: Bool -> Pdu Small ('Synchronous Bool)
    SmallCast :: String -> Pdu Small 'Asynchronous
    deriving (Typeable)

instance NFData (Pdu Small r) where
  rnf (SmallCall x) = rnf x
  rnf (SmallCast x) = rnf x

instance ToLogMsg (Pdu Small r) where
  toLogMsg (SmallCall x) = "SmallCall " <> toLogMsg x
  toLogMsg (SmallCast x) = "SmallCast " <> toLogMsg x

-- ----------------------------------------------------------------------------
instance ToTypeLogMsg Small where
  toTypeLogMsg _ = "Small"

instance IoLogging e => S.Server Small (Processes e) where
  data StartArgument Small = MkSmall deriving (Show)
  newtype Model Small = SmallModel String deriving (Default)
  update _me MkSmall x =
    case x of
      E.OnCall rt (SmallCall f) ->
        do
          S.modifyModel (\(SmallModel y) -> SmallModel (y ++ ", " ++ show f))
          sendReply rt f
      E.OnCast msg ->
        logInfo msg
      other ->
        interrupt (ErrorInterrupt (toLogMsg other))

instance ToLogMsg (S.StartArgument Small)

-- ----------------------------------------------------------------------------

data Big deriving (Typeable)

instance ToTypeLogMsg Big where
  toTypeLogMsg _ = "Big"

instance HasPdu Big where
  type EmbeddedPduList Big = '[Small]
  data Pdu Big r where
    BigCall :: Bool -> Pdu Big ('Synchronous Bool)
    BigCast :: String -> Pdu Big 'Asynchronous
    BigSmall :: Pdu Small r -> Pdu Big r
    deriving (Typeable)

instance NFData (Pdu Big r) where
  rnf (BigCall x) = rnf x
  rnf (BigCast x) = rnf x
  rnf (BigSmall x) = rnf x

instance ToLogMsg (Pdu Big r) where
  toLogMsg (BigCall x) = "SmallCall " <> toLogMsg x
  toLogMsg (BigCast x) = "SmallCast " <> toLogMsg x
  toLogMsg (BigSmall x) = "BigSmall  " <> toLogMsg x

instance HasPduPrism Big Small where
  embedPdu = BigSmall
  fromPdu (BigSmall x) = Just x
  fromPdu _ = Nothing

-- ----------------------------------------------------------------------------

instance IoLogging e => S.Server Big (Processes e) where
  data StartArgument Big = MkBig deriving (Show)
  newtype Model Big = BigModel String deriving (Default)
  update me MkBig = \case
    E.OnCall rt req ->
      case req of
        BigCall o -> do
          logNotice (LABEL "BigCall" o)
          sendReply rt o
        BigSmall x ->
          S.coerceEffects
            ( S.update
                (coerce me)
                MkSmall
                (S.OnCall (toEmbeddedReplyTarget rt) x)
            )
    E.OnCast req ->
      case req of
        BigCast o -> S.putModel (BigModel o)
        BigSmall x -> S.coerceEffects (S.update (coerce me) MkSmall (S.OnCast x))
    other ->
      interrupt (ErrorInterrupt (toLogMsg other))

instance ToLogMsg (StartArgument Big)

-- ----------------------------------------------------------------------------

test_genServer :: HasCallStack => TestTree
test_genServer =
  setTravisTestOptions $
    testGroup
      "Server"
      [ runTestCase "When a server is started it handles call Pdus without dieing" $ do
          big <- S.startLink MkBig
          call big (BigCall True) >>= lift . assertBool "invalid result 1"
          isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
          call big (BigCall False) >>= lift . assertBool "invalid result 2" . not
          isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
          cast big (BigCast "rezo")
          isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
          cast big (BigSmall (SmallCast "yo diggi"))
          isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
          call big (BigSmall (SmallCall False)) >>= lift . assertBool "invalid result 3" . not
          isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
      ]
