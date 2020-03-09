{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Cut.Church
( -- * Cut carrier
  runCut
, runCutA
, runCutM
, CutC(..)
  -- * Cut effect
, module Effect.Cut
  -- * NonDet effects
, module Effect.NonDet
) where

import           Algebra
import           Control.Applicative (liftA2)
import qualified Control.Applicative as Alt
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Effect.Cut
import           Effect.NonDet

runCut :: (a -> m b -> m b) -> m b -> m b -> CutC m a -> m b
runCut cons nil fail (CutC runCutC) = runCutC cons nil fail

runCutA :: (Alt.Alternative f, Applicative m) => CutC m a -> m (f a)
runCutA = runCut (fmap . (Alt.<|>) . pure) (pure Alt.empty) (pure Alt.empty)

runCutM :: (Applicative m, Monoid b) => (a -> b) -> CutC m a -> m b
runCutM leaf = runCut (fmap . mappend . leaf) (pure mempty) (pure mempty)

newtype CutC m a = CutC (forall b . (a -> m b -> m b) -> m b -> m b -> m b)
  deriving (Functor)

instance Applicative (CutC m) where
  pure a = CutC (\ cons nil _ -> cons a nil)
  {-# INLINE pure #-}
  CutC f <*> CutC a = CutC $ \ cons nil fail ->
    f (\ f' fs -> a (cons . f') fs fail) nil fail
  {-# INLINE (<*>) #-}

instance Monad (CutC m) where
  CutC a >>= f = CutC $ \ cons nil fail ->
    a (\ a' as -> runCut cons as fail (f a')) nil fail
  {-# INLINE (>>=) #-}

-- | A single fixpoint is shared between all branches.
instance MonadFix m => MonadFix (CutC m) where
  mfix f = CutC $ \ cons nil fail -> mfix
    (toCut . f . run . fromCut)
    >>= run . runCut (fmap . cons) (pure nil) (pure fail) where
    toCut = runCut (fmap . (<|>) . pure) (pure empty) (pure cutfail)
    fromCut = runCut (<$) (error "mfix CutC: empty") (error "mfix CutC: cutfail")
  {-# INLINE mfix #-}

instance MonadTrans CutC where
  lift m = CutC (\ cons nil _ -> m >>= flip cons nil)
  {-# INLINE lift #-}

instance (Algebra m, Effect (Sig m)) => Algebra (CutC m) where
  type Sig (CutC m) = Cut :+: NonDet :+: Sig m

  alg = \case
    L Cutfail    -> CutC $ \ _    _   fail -> fail
    L (Call m k) -> CutC $ \ cons nil fail -> runCut (\ a as -> runCut cons as fail (k a)) nil nil m
    R (L (L Empty))      -> CutC $ \ _ nil _ -> nil
    R (L (R (Choose k))) -> CutC $ \ cons nil fail -> runCut cons (runCut cons nil fail (k False)) fail (k True)
    R (R other)          -> CutC $ \ cons nil fail -> alg (handle (pure ()) dst other) >>= runIdentity . runCut (coerce cons) (coerce nil) (coerce fail)
    where
    dst :: Applicative m => CutC Identity (CutC m a) -> m (CutC Identity a)
    dst = runIdentity . runCut (fmap . liftA2 (<|>) . runCut (fmap . (<|>) . pure) (pure empty) (pure cutfail)) (pure (pure empty)) (pure (pure cutfail))
  {-# INLINE alg #-}
