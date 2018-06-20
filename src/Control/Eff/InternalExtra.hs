-- | "Control.Eff" utilities, some stolen from future versions of
-- @extensible-effects@. In future versions, there might be something like a
-- @Control.Eff.Extend@ module that might contain a lot of it, according to
-- the discussion here:
-- https://github.com/suhailshergill/extensible-effects/issues/98
module Control.Eff.InternalExtra where

import Control.Eff

-- | Embeds a less-constrained 'Eff' into a more-constrained one. Analogous to
-- MTL's 'lift'.
raise :: Eff r a -> Eff (e ': r) a
raise = loop
  where
    loop (Val x) = pure x
    loop (E u q) = E (weaken u) $ qComps q loop
{-# INLINE raise #-}
