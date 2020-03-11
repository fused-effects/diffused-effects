{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Cut.Church
( -- * Cut carrier
  runCut
, runCutA
, runCutM
, CutT(..)
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

runCut :: (a -> m b -> m b) -> m b -> m b -> CutT m a -> m b
runCut cons nil fail (CutT runCutT) = runCutT cons nil fail

runCutA :: (Alt.Alternative f, Applicative m) => CutT m a -> m (f a)
runCutA = runCut (fmap . (Alt.<|>) . pure) (pure Alt.empty) (pure Alt.empty)

runCutM :: (Applicative m, Monoid b) => (a -> b) -> CutT m a -> m b
runCutM leaf = runCut (fmap . mappend . leaf) (pure mempty) (pure mempty)

newtype CutT m a = CutT { runCutT :: forall b . (a -> m b -> m b) -> m b -> m b -> m b }
  deriving (Functor)

instance Applicative (CutT m) where
  pure a = CutT (\ cons nil _ -> cons a nil)
  {-# INLINE pure #-}
  CutT f <*> CutT a = CutT $ \ cons nil fail ->
    f (\ f' fs -> a (cons . f') fs fail) nil fail
  {-# INLINE (<*>) #-}

instance Monad (CutT m) where
  CutT a >>= f = CutT $ \ cons nil fail ->
    a (\ a' as -> runCut cons as fail (f a')) nil fail
  {-# INLINE (>>=) #-}

-- | A single fixpoint is shared between all branches.
instance MonadFix m => MonadFix (CutT m) where
  mfix f = CutT $ \ cons nil fail -> mfix
    (toCut . f . run . fromCut)
    >>= run . runCut (fmap . cons) (pure nil) (pure fail) where
    toCut = runCut (fmap . (<|>) . pure) (pure empty) (pure cutfail)
    fromCut = runCut (<$) (error "mfix CutT: empty") (error "mfix CutT: cutfail")
  {-# INLINE mfix #-}

instance MonadTrans CutT where
  lift m = CutT (\ cons nil _ -> m >>= flip cons nil)
  {-# INLINE lift #-}

instance Algebra m => Algebra (CutT m) where
  type Sig (CutT m) = Cut :+: NonDet :+: Sig m

  alg (hdl :: forall x . ctx (n x) -> CutT m (ctx x)) (ctx :: ctx ()) = \case
    L Cutfail    -> CutT $ \ _    _   fail -> fail
    L (Call m k) -> CutT $ \ cons nil fail -> runCut (\ a as -> runCut cons as fail (hdl (fmap k a))) nil nil (hdl (m <$ ctx))
    R (L (L Empty))      -> CutT $ \ _ nil _ -> nil
    R (L (R (Choose k))) -> CutT $ \ cons nil fail -> runCut cons (runCut cons nil fail (hdl (k False <$ ctx))) fail (hdl (k True <$ ctx))
    R (R other)          -> CutT $ \ cons nil fail -> thread dst (pure ctx) other >>= runIdentity . runCut (coerce cons) (coerce nil) (coerce fail)
    where
    dst :: CutT Identity (ctx (n a)) -> m (CutT Identity (ctx a))
    dst = runIdentity . runCut (fmap . liftA2 (<|>) . runCut (fmap . (<|>) . pure) (pure empty) (pure cutfail) . hdl) (pure (pure empty)) (pure (pure cutfail))
  {-# INLINE alg #-}
