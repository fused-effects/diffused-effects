module Control.Effect.NonDet
( -- * NonDet effects
  NonDet
, module Control.Effect.Choose
, module Control.Effect.Empty
, oneOf
, foldMapA
  -- * Re-exports
, Alternative(..)
, Algebra
, Has
, MonadPlus(..)
, guard
, optional
, run
) where

import Control.Algebra
import Control.Applicative (Alternative(..), optional)
import Control.Effect.Choose (Choose(..))
import Control.Effect.Empty (Empty(..))
import Control.Effect.NonDet.Internal (NonDet)
import Control.Monad (MonadPlus(..), guard)
import Data.Coerce
import Data.Monoid (Alt(..))

oneOf :: (Foldable t, Alternative m) => t a -> m a
oneOf = foldMapA pure

foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = getAlt #. foldMap (Alt #. f)


-- | Compose a function operationally equivalent to 'id' on the left.
--
--   cf https://github.com/fused-effects/diffused-effects/pull/1#discussion_r323560758
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}
