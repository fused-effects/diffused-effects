{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Resumable.Resume
( -- * Resumable effect
  module Control.Effect.Resumable
  -- * Resumable Algebra
, runResumable
, ResumableC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra.Class
import Control.Algebra.Reader
import Control.Effect.Resumable
import Control.Monad.Trans.Class

-- | Run a 'Resumable' effect, resuming uncaught errors with a given handler.
--
--   Note that this may be less efficient than defining a specialized Algebra type and instance specifying the handler’s behaviour directly. Performance-critical code may wish to do that to maximize the opportunities for fusion and inlining.
--
--   >>> data Err a where Err :: Int -> Err Int
--
--   prop> run (runResumable (\ (Err b) -> pure (1 + b)) (pure a)) === a
--   prop> run (runResumable (\ (Err b) -> pure (1 + b)) (throwResumable (Err a))) === 1 + a
runResumable
  :: (forall x . err x -> m x)
  -> ResumableC err m a
  -> m a
runResumable with = runReader (Handler with) . runResumableC

newtype ResumableC err m a = ResumableC { runResumableC :: ReaderC (Handler err m) m a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans (ResumableC err) where
  lift = ResumableC . lift
  {-# INLINE lift #-}

newtype Handler err m = Handler { runHandler :: forall x . err x -> m x }

instance Algebra sig m => Algebra (Resumable err :+: sig) (ResumableC err m) where
  alg (L (Resumable err k)) = ResumableC (ReaderC (\ handler -> runHandler handler err)) >>= k
  alg (R other)             = ResumableC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XGADTs
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Data.Functor.Const
-- >>> import Data.Functor.Identity
