{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- | Add-ons to 'Control.Eff.Exception'
module Control.Eff.ExceptionExtra
  ( try
  , liftRethrow
  , runErrorRethrowIO
  , liftCatch
  , liftCatchEff
  , liftTry
  , module X
  )
where

import           Control.Monad                  ( (>=>) )
import qualified Control.Exception             as Exc
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Exception         as X
import           GHC.Stack

-- * Effects combining "Control.Eff.Exception" and 'Exc.try'

-- | Catch an exception and return it in an 'Either'.
try :: forall e r a . Member (Exc e) r => Eff r a -> Eff r (Either e a)
try e = catchError (Right <$> e) (return . Left)

-- | Lift an IO action and catch all error using 'Exc.try' then wrap the
-- 'Exc.Exception' using a given wrapper function and rethrow it using
-- 'X.throwError'.
liftRethrow
  :: forall proxy e r a
   . (Exc.Exception e, SetMember Lift (Lift IO) r, SetMember Exc (Exc e) r)
  => proxy e
  -> IO a
  -> Eff r a
liftRethrow _ m = lift (Exc.try m) >>= either (throwError @e) return

-- | Run an effect with exceptions like 'X.runError' and rethrow it as
-- 'Exc.SomeException' using 'Exc.throw'
runErrorRethrowIO
  :: forall e r a
   . (Exc.Exception e, SetMember Lift (Lift IO) r)
  => Eff (Exc e ': r) a
  -> Eff r a
runErrorRethrowIO = runError >=> either (Exc.throw . Exc.SomeException) return

-- * Effect Utilities around 'Exc.try'

-- | Lift an IO action and catch all errors with 'Exc.try' with a pure
-- handler.
liftCatch
  :: forall e r a
   . (HasCallStack, Exc.Exception e, SetMember Lift (Lift IO) r)
  => (e -> a)
  -> IO a
  -> Eff r a
liftCatch handleE m =
  lift (Exc.try m) >>= either (return . handleE) return

-- | Lift an IO action and catch all errors with 'Exc.try' with an effect
-- handler.
liftCatchEff
  :: forall e r a
   . (SetMember Lift (Lift IO) r, HasCallStack, Exc.Exception e)
  => (e -> Eff r a)
  -> IO a
  -> Eff r a
liftCatchEff handleE m =
  lift (Exc.try m) >>= either handleE return

-- | Catch 'Exc.Exception' thrown by an effect.
liftTry
  :: forall e r a
   . (HasCallStack, Exc.Exception e, SetMember Lift (Lift IO) r)
  => Eff r a
  -> Eff r (Either e a)
liftTry m =
   (Right <$> m) `catchDynE` (return . Left)
