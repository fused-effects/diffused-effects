module Control.Algebra.Pure
( -- * Pure effect
  module Control.Effect.Pure
  -- * Pure carrier
, run
, PureC(..)
) where

import Control.Applicative
import Control.Effect.Pure
import Control.Monad.Fix
import Data.Coerce

-- | Run an action exhausted of effects to produce its final result value.
run :: PureC a -> a
run = runPureC
{-# INLINE run #-}

newtype PureC a = PureC { runPureC :: a }

instance Functor PureC where
  fmap = coerce
  {-# INLINE fmap #-}

  a <$ _ = pure a
  {-# INLINE (<$) #-}

instance Applicative PureC where
  pure = PureC
  {-# INLINE pure #-}

  (<*>) = coerce
  {-# INLINE (<*>) #-}

  liftA2 = coerce
  {-# INLINE liftA2 #-}

  _ *> b = b
  {-# INLINE (*>) #-}

  a <* _ = a
  {-# INLINE (<*) #-}

instance Monad PureC where
  return = pure
  {-# INLINE return #-}

  PureC a >>= f = f a
  {-# INLINE (>>=) #-}

instance MonadFix PureC where
  mfix f = PureC (fix (runPureC . f))
  {-# INLINE mfix #-}
