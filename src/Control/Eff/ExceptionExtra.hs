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
  , module X
  )
where

import           Control.Monad                  ( (>=>) )
import qualified Control.Exception             as Exc
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Exception         as X

-- | Catch an exception and return it in an 'Either'.
try :: forall e r a . Member (Exc e) r => Eff r a -> Eff r (Either e a)
try e = catchError (Right <$> e) (return . Left)

-- | Lift an IO action and catch all error using 'Exc.try' then wrap the
-- 'Exc.Exception' using a given wrapper function and rethrow it using
-- 'X.throwError'.
liftRethrow
  :: forall e r a
   . (Exc.Exception e, SetMember Lift (Lift IO) r, Member (Exc e) r)
  => (Exc.SomeException -> e)
  -> IO a
  -> Eff r a
liftRethrow liftE m = lift (Exc.try m) >>= either (throwError . liftE) return

-- | Run an effect with exceptions like 'X.runError' and rethrow it as
-- 'Exc.SomeException' using 'Exc.throw'
runErrorRethrowIO
  :: forall e r a
   . (Exc.Exception e, SetMember Lift (Lift IO) r)
  => Eff (Exc e ': r) a
  -> Eff r a
runErrorRethrowIO = runError >=> either (Exc.throw . Exc.SomeException) return
