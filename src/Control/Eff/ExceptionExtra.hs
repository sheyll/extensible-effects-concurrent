{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- | Add-ons to 'Control.Eff.Exception'
module Control.Eff.ExceptionExtra
  ( liftTry
  , module X
  )
where

import qualified Control.Exception             as Exc
import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Exception         as X
import           GHC.Stack

-- | Catch 'Exc.Exception' thrown by an effect.
liftTry
  :: forall e r a
   . (HasCallStack, Exc.Exception e, SetMember Lift (Lift IO) r)
  => Eff r a
  -> Eff r (Either e a)
liftTry m = (Right <$> m) `catchDynE` (return . Left)
