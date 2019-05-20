module ServerTests
  ( test_genServer
  ) where

import Common
import Control.Concurrent (threadDelay)
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent
import Control.Eff.Concurrent.Process.ForkIOScheduler as Scheduler
import Control.Eff.Concurrent.Process.Timer
import Control.Eff.Concurrent.Protocol.Supervisor as Sup
import Control.Lens
import Data.Either (fromRight, isLeft, isRight)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Type.Pretty
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.HUnit

-- ------------------------------
data Big = MkBig
  deriving (Typeable)

type instance ToPretty Big = PutStr "big"

data instance  Pdu Big r where
        BigCall :: Bool -> Protocol Big ('Synchronous Bool)
        BigCast :: String -> Protocol Big 'Asynchronous
        BigSmall :: Protocol Small r -> Protocol Big r
    deriving (Typeable)

instance Show (Protocol Big r) where
  showsPrec d (BigCast x) = showParen (d > 10) (showString "BigCast " . showString x)
  showsPrec d (BigCall x) = showParen (d > 10) (showString "BigCall " . showString x)
  showsPrec d (BigSmall x) = showParen (d > 10) (showString "BigSmall " . showsPrec 11 x)

data Small = MkSmall
  deriving (Typeable)

type instance ToPretty Small = PutStr "small"

data instance  Protocol Small r where
        SmallCall :: Bool -> Protocol Small ('Synchronous Bool)
        SmallCast :: String -> Protocol Small 'Asynchronous
    deriving (Typeable)

instance NFData (Protocol Small r) where
  rnf (SmallCall x) = rnf x
  rnf (SmallCast x) = rnf x

instance Show (Protocol Small r) where
  showsPrec d (SmallCast x) = showParen (d > 10) (showString "SmallCast " . showString x)
  showsPrec d (SmallCall x) = showParen (d > 10) (showString "SmallCall " . showString x)

instance HasProtocol Big Small where
  embeddedPdu =
    prism'
      BigSmall
      (\case
         (BigSmall x) -> Just x
         _ -> Nothing)

-- ----------------------------------------------------------------------------
instance Server Big e where
  serverInit MkBig = return ((), ())
  handlePde MkBig = \case
    Call orig req ->
      case req of
        BigCall o -> sendReply orig o
        BigSmall (SmallCall x) -> error "TODO BigSmall call"
    Cast req ->
      case req of
        BigCast o -> sendReply orig o
        BigSmall (SmallCast x) -> error "TODO BigSmall cast"
  recoverFromInterrupt MkBig NormalExitRequested = exitNormally

-- ----------------------------------------------------------------------------

test_genServer :: HasCallStack => TestTree
test_genServer = setTravisTestOptions $ testGroup "Server" [
  runTestCase "When a server is started it handles call Pdus" $ do
    big <- spawnProtocolServer MkBig
    call big (BigCall True) >>=  left . assertBool "invalid result"
  ]