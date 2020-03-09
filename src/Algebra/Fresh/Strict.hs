{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Algebra.Fresh.Strict
( -- * Fresh effect
  module Effect.Fresh
  -- * Fresh carrier
, runFresh
, FreshC(..)
  -- * Re-exports
, Has
, run
) where

import Algebra
import Control.Monad.Trans.State.Strict
import Control.Applicative (Alternative(..))
import Effect.Fresh
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Fresh' effect counting up from 0.
--
--   prop> run (runFresh (replicateM n fresh)) === [0..pred n]
--   prop> run (runFresh (replicateM n fresh *> pure b)) === b
runFresh :: Monad m => FreshC m a -> m a
runFresh = (`evalStateT` 0) . runFreshC

newtype FreshC m a = FreshC { runFreshC :: StateT Int m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra m, Effect (Sig m)) => Algebra (FreshC m) where
  type Sig (FreshC m) = Fresh :+: Sig m

  alg = \case
    L (Fresh   k) -> FreshC $ do
      i <- get
      put (succ i)
      runFreshC (k i)
    L (Reset m k) -> FreshC $ do
      i <- get
      a <- runFreshC m
      put (i :: Int)
      runFreshC (k a)
    R other       -> FreshC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}
