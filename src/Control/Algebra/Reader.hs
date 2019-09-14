{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Reader
( -- * Reader effect
  module Control.Effect.Reader
  -- * Reader carrier
, runReader
, ReaderC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Applicative (liftA2)
import Control.Algebra.Class
import Control.Effect.Reader
import Control.Monad.Trans.Class

-- | Run a 'Reader' effect with the passed environment value.
--
--   prop> run (runReader a (pure b)) === b
runReader :: r -> ReaderC r m a -> m a
runReader r c = runReaderC c r
{-# INLINE runReader #-}

newtype ReaderC r m a = ReaderC { runReaderC :: r -> m a }
  deriving (Functor)

instance Applicative m => Applicative (ReaderC r m) where
  pure = ReaderC . const . pure
  {-# INLINE pure #-}
  ReaderC f <*> ReaderC a = ReaderC (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}
  ReaderC u *> ReaderC v = ReaderC $ \ r -> u r *> v r
  {-# INLINE (*>) #-}
  ReaderC u <* ReaderC v = ReaderC $ \ r -> u r <* v r
  {-# INLINE (<*) #-}

instance Monad m => Monad (ReaderC r m) where
  ReaderC a >>= f = ReaderC (\ r -> a r >>= runReader r . f)
  {-# INLINE (>>=) #-}

instance MonadTrans (ReaderC r) where
  lift = ReaderC . const
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Reader r :+: sig) (ReaderC r m) where
  alg (L (Ask       k)) = ReaderC (\ r -> runReader r (k r))
  alg (L (Local f m k)) = ReaderC (\ r -> runReader (f r) m) >>= k
  alg (R other)         = ReaderC (\ r -> alg (hmap (runReader r) other))
  {-# INLINE alg #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
