{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Resumable.Abort
( -- * Resumable effect
  module Control.Effect.Resumable
  -- * Resumable Algebra
, runResumable
, ResumableC(..)
, SomeError(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.Algebra.Class
import Control.Algebra.Error.Either
import Control.DeepSeq
import Control.Effect.Resumable
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Classes

-- | Run a 'Resumable' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runResumable (pure a)) === Right @(SomeError Identity) @Int a
runResumable :: ResumableC err m a -> m (Either (SomeError err) a)
runResumable = runError . runResumableC

newtype ResumableC err m a = ResumableC { runResumableC :: ErrorC (SomeError err) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, Effect sig) => Algebra (Resumable err :+: sig) (ResumableC err m) where
  alg (L (Resumable err _)) = ResumableC (throwError (SomeError err))
  alg (R other)             = ResumableC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}


-- | An error at some existentially-quantified type index.
data SomeError err
  = forall a . SomeError (err a)

-- | Equality for 'SomeError' is determined by an 'Eq1' instance for the error type.
--
--   Note that since we can’t tell whether the type indices are equal, let alone what 'Eq' instance to use for them, the comparator passed to 'liftEq' always returns 'True'. Thus, 'SomeError' is best used with type-indexed GADTs for the error type.
--
--   prop> SomeError (Identity a) === SomeError (Identity b)
--   prop> (SomeError (Const a) === SomeError (Const b)) == (a == b)
instance Eq1 err => Eq (SomeError err) where
  SomeError exc1 == SomeError exc2 = liftEq (const (const True)) exc1 exc2

-- | Ordering for 'SomeError' is determined by an 'Ord1' instance for the error type.
--
--   Note that since we can’t tell whether the type indices are equal, let alone what 'Ord' instance to use for them, the comparator passed to 'liftCompare' always returns 'EQ'. Thus, 'SomeError' is best used with type-indexed GADTs for the error type.
--
--   prop> (SomeError (Identity a) `compare` SomeError (Identity b)) === EQ
--   prop> (SomeError (Const a) `compare` SomeError (Const b)) === (a `compare` b)
instance Ord1 err => Ord (SomeError err) where
  SomeError exc1 `compare` SomeError exc2 = liftCompare (const (const EQ)) exc1 exc2

-- | Showing for 'SomeError' is determined by a 'Show1' instance for the error type.
--
--   Note that since we can’t tell what 'Show' instance to use for the type index, the functions passed to 'liftShowsPrec' always return the empty 'ShowS'. Thus, 'SomeError' is best used with type-indexed GADTs for the error type.
--
--   prop> show (SomeError (Identity a)) === "SomeError (Identity )"
--   prop> show (SomeError (Const a)) === ("SomeError (Const " ++ showsPrec 11 a ")")
instance Show1 err => Show (SomeError err) where
  showsPrec d (SomeError err) = showsUnaryWith (liftShowsPrec (const (const id)) (const id)) "SomeError" d err


-- | Evaluation of 'SomeError' to normal forms is determined by a 'NFData1' instance for the error type.
--
--   prop> pure (rnf (SomeError (Identity (error "error"))) :: SomeError Identity) `shouldThrow` errorCall "error"
instance NFData1 err => NFData (SomeError err) where
  rnf (SomeError err) = liftRnf (\a -> seq a ()) err
