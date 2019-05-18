module GenServerTests
  ( test_genServer
  ) where

import Common
import Control.Concurrent (threadDelay)
import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent
import Control.Eff.Concurrent.Api.Supervisor as Sup
import Control.Eff.Concurrent.Process.ForkIOScheduler as Scheduler
import Control.Eff.Concurrent.Process.Timer
import Control.Lens
import Data.Either (fromRight, isLeft, isRight)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Type.Pretty
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.HUnit

-- | A class for 'Api' instances that can 'Lens' and 'Prism' /into/ and /out of/
-- 'Api' instances.
class WrapsApi api subApi where
  apiPrism :: Prism' (Api api result) (Api subApi result)
  apiPrism = prism' wrapApi unwrapApi
  wrapApi :: Api subApi r -> Api api r
  wrapApi = review apiPrism
  unwrapApi :: Api api r -> Maybe (Api subApi r)
  unwrapApi = preview apiPrism

instance WrapsApi a a where
  apiPrism = prism' id Just
  wrapApi = id
  unwrapApi = Just

data instance  Api (Either a1 a2) x where
        LeftApi :: Api a1 r -> Api (Either a1 a2) r
        RightApi :: Api a2 r -> Api (Either a1 a2) r

instance WrapsApi (Either a1 a2) a1 where
  wrapApi = LeftApi
  unwrapApi (LeftApi l) = Just l
  unwrapApi _ = Nothing

instance WrapsApi (Either a1 a2) a2 where
  apiPrism =
    prism' RightApi $ \case
      RightApi r -> Just r
      LeftApi _ -> Nothing

callWrapped ::
     ( WrapsApi api subApi
     , '[Interrupts, Logs] <:: eff
     , SetMember Process (Process q) eff
     , Tangible result
     , Typeable api
     , Tangible (Api api ('Synchronous result))
     , PrettyTypeShow (ToPretty api)
     )
  => Server api
  -> Api subApi ('Synchronous result)
  -> Eff eff result
callWrapped svr = call svr . wrapApi

castWrapped ::
     ( WrapsApi api apiPrism
     , '[Interrupts, Logs] <:: eff
     , SetMember Process (Process q) eff
     , Tangible result
     , Typeable api
     , Tangible (Api api 'Asynchronous)
     , PrettyTypeShow (ToPretty api)
     )
  => Server api
  -> Api apiPrism 'Asynchronous
  -> Eff eff ()
castWrapped svr = cast svr . wrapApi

-- ------------------------------

data Big
  deriving (Typeable)

type instance ToPretty Big = PutStr "big"

data instance  Api Big r where
        BigCall :: String -> Api Big ('Synchronous String)
        BigCast :: String -> Api Big 'Asynchronous
        BigSmall :: Api Small r -> Api Big r
    deriving (Typeable)

instance Show (Api Big r) where
  showsPrec d (BigCast x) = showParen (d > 10) (showString "BigCast " . showString x)
  showsPrec d (BigCall x) = showParen (d > 10) (showString "BigCall " . showString x)
  showsPrec d (BigSmall x) = showParen (d > 10) (showString "BigSmall " . showsPrec 11 x)

data Small
  deriving (Typeable)

type instance ToPretty Small = PutStr "small"

data instance  Api Small r where
        SmallCall :: String -> Api Small ('Synchronous String)
        SmallCast :: String -> Api Small 'Asynchronous
    deriving (Typeable)

instance NFData (Api Small r) where
  rnf (SmallCall x) = rnf x
  rnf (SmallCast x) = rnf x

instance Show (Api Small r) where
  showsPrec d (SmallCast x) = showParen (d > 10) (showString "SmallCast " . showString x)
  showsPrec d (SmallCall x) = showParen (d > 10) (showString "SmallCall " . showString x)

instance WrapsApi Big Small where
  apiPrism =
    prism'
      BigSmall
      (\case
         (BigSmall x) -> Just x
         _ -> Nothing)

-- ----------------------------------------------------------------------------

test_genServer :: HasCallStack => TestTree
test_genServer = setTravisTestOptions $ testGroup "GenServer" []
