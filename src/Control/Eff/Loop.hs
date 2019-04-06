-- | In rare occasions GHC optimizes innocent looking loops into space-leaking monsters.
-- See the discussion here: <https://ghc.haskell.org/trac/ghc/ticket/13080 GHC issue 13080> for more
-- details, or <https://ro-che.info/articles/2017-01-10-nested-loop-space-leak this blog post about space leaks in nested loops>
--
-- These functions in this module __/might/__ help, at least in conjunction with
-- the @-fno-full-laziness@ GHC option.
--
-- There is a unit test in the sources of this module, which can be used to do a
-- comparative heap profiling of these function vs. their counterparts in the
-- @base@ package.
--
-- Here are the images of the profiling results, the images show that the
-- functions in this module do not leak space, compared to the original functions
-- ('Control.Monad.forever' and 'Control.Monad.replicateM_'):
--
-- ![Heap profiling of the unit tests called Loop-without-space-leaks shows that less than 200k Bytes were used](docs/extensible-effects-concurrent-test-Loop-without-space-leaks.png)
-- ![Heap profiling of the unit tests called Loop-WITH-space-leaks shows that at least 160M Bytes were used](docs/extensible-effects-concurrent-test-Loop-WITH-space-leaks.png)
--
-- @since 0.4.0.0
module Control.Eff.Loop
  ( foreverCheap
  , replicateCheapM_
  )
where

import           Control.Monad                  ( void )

-- | A version of 'Control.Monad.forever' that hopefully tricks
-- GHC into __/not/__ creating a space leak.
-- The intuition is, that we want to do something that is /cheap/, and hence
-- should be __recomputed__ instead of shared.
--
-- @since 0.4.0.0
{-# NOINLINE foreverCheap #-}
foreverCheap :: Monad m => m a -> m ()
foreverCheap x = x >> foreverCheap (x >> x)

-- | A version of 'Control.Monad.replicateM_' that hopefully tricks
-- GHC into __/not/__ creating a space leak.
-- The intuition is, that we want to do something that is /cheap/, and hence
-- should be __recomputed__ instead of shared.
--
-- @since 0.4.0.0
{-# NOINLINE replicateCheapM_ #-}
replicateCheapM_ :: Monad m => Int -> m a -> m ()
replicateCheapM_ n e = if n <= 0
  then return ()
  else if n == 1
    then void e
    else
      let nL = n `div` 2
          nR = n - nL
      in  replicateCheapM_ nL e >> replicateCheapM_ nR e
