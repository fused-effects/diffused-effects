{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Empty.Maybe
( -- * Empty effect
  module Control.Effect.Empty
  -- * Empty Algebra
, runEmpty
, EmptyC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Applicative (liftA2)
import Control.Algebra.Class
import Control.Effect.Empty
import Control.Monad.Trans.Class

-- | Run an 'Empty' effect, returning 'Nothing' for empty computations, or 'Just' the result otherwise.
--
--   prop> run (runEmpty empty)    === Nothing
--   prop> run (runEmpty (pure a)) === Just a
runEmpty :: EmptyC m a -> m (Maybe a)
runEmpty = runEmptyC

newtype EmptyC m a = EmptyC { runEmptyC :: m (Maybe a) }
  deriving (Functor)

instance Applicative m => Applicative (EmptyC m) where
  pure = EmptyC . pure . Just
  {-# INLINE pure #-}
  EmptyC f <*> EmptyC a = EmptyC (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (EmptyC m) where
  EmptyC a >>= f = EmptyC (a >>= maybe (pure Nothing) (runEmptyC . f))
  {-# INLINE (>>=) #-}

instance MonadTrans EmptyC where
  lift = EmptyC . fmap Just
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (Empty :+: sig) (EmptyC m) where
  alg (L Empty) = EmptyC (pure Nothing)
  alg (R other) = EmptyC (alg (handle (Just ()) (maybe (pure Nothing) runEmptyC) other))
  {-# INLINE alg #-}

-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
