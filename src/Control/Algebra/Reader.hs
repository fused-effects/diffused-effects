{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Reader
( -- * Reader effect
  module Control.Effect.Reader
  -- * Reader carrier
, runReader
, ReaderC(..)
  -- * Re-exports
, Has
, run
) where

import Control.Algebra
import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Reader
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
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

instance Alternative m => Alternative (ReaderC r m) where
  empty = ReaderC (const empty)
  {-# INLINE empty #-}
  ReaderC l <|> ReaderC r = ReaderC (liftA2 (<|>) l r)
  {-# INLINE (<|>) #-}

instance Monad m => Monad (ReaderC r m) where
  ReaderC a >>= f = ReaderC (\ r -> a r >>= runReader r . f)
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (ReaderC r m) where
  fail = ReaderC . const . Fail.fail
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ReaderC s m) where
  mfix f = ReaderC (\ r -> mfix (runReader r . f))
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ReaderC r m) where
  liftIO = ReaderC . const . liftIO
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (ReaderC r m)

instance MonadTrans (ReaderC r) where
  lift = ReaderC . const
  {-# INLINE lift #-}

instance Algebra m => Algebra (ReaderC r m) where
  type Signature (ReaderC r m) = Reader r :+: Signature m
  alg (L (Ask       k)) = ReaderC (\ r -> runReader r (k r))
  alg (L (Local f m k)) = ReaderC (\ r -> runReader (f r) m) >>= k
  alg (R other)         = ReaderC (\ r -> alg (hmap (runReader r) other))
  {-# INLINE alg #-}
