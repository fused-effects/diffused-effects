{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Fresh.Strict
( -- * Fresh effect
  module Control.Effect.Fresh
  -- * Fresh Algebra
, runFresh
, FreshC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra.Class
import Control.Algebra.State.Strict
import Control.Effect.Fresh
import Control.Monad.Trans.Class

-- | Run a 'Fresh' effect counting up from 0.
--
--   prop> run (runFresh (replicateM n fresh)) === [0..pred n]
--   prop> run (runFresh (replicateM n fresh *> pure b)) === b
runFresh :: Functor m => FreshC m a -> m a
runFresh = evalState 0 . runFreshC

newtype FreshC m a = FreshC { runFreshC :: StateC Int m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

instance (Algebra sig m, Effect sig) => Algebra (Fresh :+: sig) (FreshC m) where
  alg (L (Fresh   k)) = FreshC $ do
    i <- get
    put (succ i)
    runFreshC (k i)
  alg (L (Reset m k)) = FreshC $ do
    i <- get
    a <- runFreshC m
    put (i :: Int)
    runFreshC (k a)
  alg (R other)       = FreshC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Monad (replicateM)
-- >>> import Data.List (nub)
