-- | Internal module containing internal helpers that didn't
-- make it into their own library.
module Control.Eff.Concurrent.Misc
  ( showSTypeRepPrec
  , showSTypeRep
  , showSTypeable
  , showSPrecTypeable
  )
  where

import           Data.Dynamic
import           Data.Typeable ()
import           Type.Reflection

-- | Render a 'Typeable' to a 'ShowS'.
--
-- @since 0.28.0
showSTypeable :: forall message . Typeable message => ShowS
showSTypeable = showSTypeRep (SomeTypeRep (typeRep @message))

-- | Render a 'Typeable' to a 'ShowS' with a precedence parameter.
--
-- @since 0.28.0
showSPrecTypeable :: forall message . Typeable message => Int -> ShowS
showSPrecTypeable d = showSTypeRepPrec d (SomeTypeRep (typeRep @message))

-- | This is equivalent to @'showSTypeRepPrec' 0@
--
-- @since 0.24.0
showSTypeRep :: SomeTypeRep -> ShowS
showSTypeRep = showSTypeRepPrec 0

-- | An internal utility to print 'Typeable' without the kinds.
-- This is like 'showsPrec' in that it accepts a /precedence/ parameter,
-- and the result is in parentheses when the precedence is higher than 9.
--
-- @since 0.24.0
showSTypeRepPrec :: Int -> SomeTypeRep -> ShowS
showSTypeRepPrec d (SomeTypeRep tr) sIn =
  let (con, conArgs) = splitApps tr
   in case conArgs of
        [] -> showString (tyConName con) sIn
        _ ->
          showParen
            (d >= 10)
            (showString (tyConName con) . showChar ':' .
              foldr1 (\f acc -> showChar '-' . f . acc)
                     (showSTypeRepPrec 10 <$> conArgs))
            sIn
