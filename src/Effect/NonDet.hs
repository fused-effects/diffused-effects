module Effect.NonDet
( -- * NonDet effects
  NonDet
, module Effect.Choose
, module Effect.Empty
, oneOf
, foldMapA
) where

import Algebra
import Effect.Choose
import Effect.Empty
import Effect.NonDet.Internal (NonDet)
import Data.Coerce

oneOf :: (Foldable t, Has NonDet m) => t a -> m a
oneOf = foldMapA pure

foldMapA :: (Foldable t, Has NonDet m) => (a -> m b) -> t a -> m b
foldMapA f = getChoosing #. foldMap (Choosing #. f)


-- | Compose a function operationally equivalent to 'id' on the left.
--
--   cf https://github.com/fused-effects/diffused-effects/pull/1#discussion_r323560758
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}
