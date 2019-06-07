{-# LANGUAGE UndecidableInstances #-}
module GenServerTests
  ( test_genServer
  ) where

import Common
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent
import Control.Eff.Concurrent.Protocol.Supervisor as Sup
import qualified Control.Eff.Concurrent.Protocol.EffectfulServer as E
import qualified Control.Eff.Concurrent.Protocol.StatefulServer as S
import Control.Eff.Concurrent.Protocol.Request
import Control.Lens
import Data.Coerce
import Data.Functor.Contravariant ((>$<))
import Data.Text as T
import Data.Type.Pretty
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.HUnit

-- ------------------------------

data Small deriving Typeable

type instance ToPretty Small = PutStr "small"

data instance  Pdu Small r where
        SmallCall :: Bool -> Pdu Small ('Synchronous Bool)
        SmallCast :: String -> Pdu Small 'Asynchronous
    deriving Typeable

instance NFData (Pdu Small r) where
  rnf (SmallCall x) = rnf x
  rnf (SmallCast x) = rnf x

instance Show (Pdu Small r) where
  showsPrec d (SmallCall x) = showParen (d > 10) (showString "SmallCall " . shows x)
  showsPrec d (SmallCast x) = showParen (d > 10) (showString "SmallCast " . showString x)


-- ----------------------------------------------------------------------------

instance LogIo e => S.Server Small (Processes e) where
  data StartArgument Small (Processes e) = MkSmall
  type Model Small = String
  update MkSmall x =
    case x of
      E.OnCall ser o (SmallCall f) ->
       sendReply ser o f
      E.OnCast msg ->
       logInfo' (show msg)
      other ->
        interrupt (ErrorInterrupt (show other))

-- ----------------------------------------------------------------------------

data Big deriving (Typeable)

type instance ToPretty Big = PutStr "big"

data instance  Pdu Big r where
        BigCall :: Bool -> Pdu Big ('Synchronous Bool)
        BigCast :: String -> Pdu Big 'Asynchronous
        BigSmall :: Pdu Small r -> Pdu Big r
    deriving Typeable

instance NFData (Pdu Big r) where
  rnf (BigCall x) = rnf x
  rnf (BigCast x) = rnf x
  rnf (BigSmall x) = rnf x

instance Show (Pdu Big r) where
  showsPrec d (BigCall x) = showParen (d > 10) (showString "SmallCall " . shows x)
  showsPrec d (BigCast x) = showParen (d > 10) (showString "SmallCast " . showString x)
  showsPrec d (BigSmall x) = showParen (d > 10) (showString "BigSmall " . showsPrec 11 x)

instance EmbedProtocol Big Small where
  embeddedPdu =
    prism'
      BigSmall
      (\case
         (BigSmall x) -> Just x
         _ -> Nothing)

-- ----------------------------------------------------------------------------

instance LogIo e => S.Server Big (Processes e) where
  data instance StartArgument Big (Processes e) = MkBig
  type Model Big = String
  update MkBig = \case
    E.OnCall ser orig req ->
          case req of
            BigCall o -> do
              logNotice ("BigCall " <> pack (show o))
              sendReply ser orig o
            BigSmall x -> S.update MkSmall (S.OnCall (coerce >$< ser) (coerce orig) x)
    E.OnCast req ->
        case req of
          BigCast o -> S.putModel @Big o
          BigSmall x -> S.update MkSmall (S.OnCast x)
    other ->
      interrupt (ErrorInterrupt (show other))
-- ----------------------------------------------------------------------------

test_genServer :: HasCallStack => TestTree
test_genServer = setTravisTestOptions $ testGroup "Server" [
  runTestCase "When a server is started it handles call Pdus without dieing" $ do
    big <- S.start MkBig
    call big (BigCall True) >>=  lift . assertBool "invalid result 1"
    isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
    call big (BigCall False) >>=  lift . assertBool "invalid result 2" . not
    isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
    cast big (BigCast "rezo")
    isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
    cast big (BigSmall (SmallCast "yo diggi"))
    isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
    call big (BigSmall (SmallCall False )) >>=  lift . assertBool "invalid result 3" . not
    isProcessAlive (_fromEndpoint big) >>= lift . assertBool "process dead"
  ]