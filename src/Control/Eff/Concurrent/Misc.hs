-- | Internal module containing internal helpers that didn't
-- make it into their own library.
module Control.Eff.Concurrent.Misc
  (
  )
where

import Data.Dynamic
import Data.Typeable ()
import Type.Reflection

-- | Render a 'Typeable' to a 'ShowS'.
--
-- @since 0.28.0
showSTypeable :: forall message. Typeable message => ShowS
showSTypeable = showSTypeRep (SomeTypeRep (typeRep @message))

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
showSTypeRepPrec _d (SomeTypeRep tr) =
  let (con, conArgs) = splitApps tr
      renderArgs =
        foldr1
          (\f acc -> showString ", " . f . acc)
          (showSTypeRepPrec 10 <$> conArgs)
   in showString (tyConName con)
        . if not (null conArgs)
          then
            showChar '<'
              . renderArgs
              . showChar '>'
          else id
