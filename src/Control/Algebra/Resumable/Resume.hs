{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Resumable.Resume
( -- * Resumable effect
  module Control.Effect.Resumable
  -- * Resumable carrier
, runResumable
, ResumableC(..)
  -- * Re-exports
, Has
, run
) where

import Control.Algebra
import Control.Algebra.Reader
import Control.Applicative (Alternative(..))
import Control.Effect.Resumable
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Resumable' effect, resuming uncaught errors with a given handler.
--
--   Note that this may be less efficient than defining a specialized carrier type and Algebra instance specifying the handler’s behaviour directly. Performance-critical code may wish to do that to maximize the opportunities for fusion and inlining.
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
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (ResumableC err) where
  lift = ResumableC . lift
  {-# INLINE lift #-}

newtype Handler err m = Handler { runHandler :: forall x . err x -> m x }

instance Algebra m => Algebra (ResumableC err m) where
  type Signature (ResumableC err m) = Resumable err :+: Signature m
  alg (L (Resumable err k)) = ResumableC (ReaderC (\ handler -> runHandler handler err)) >>= k
  alg (R other)             = ResumableC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}
