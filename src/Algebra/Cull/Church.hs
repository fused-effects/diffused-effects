{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Cull.Church
( -- * Cull carrier
  runCull
, runCullA
, runCullM
, CullC(..)
  -- * Cull effect
, module Effect.Cull
  -- * NonDet effects
, module Effect.NonDet
) where

import           Algebra
import           Algebra.NonDet.Church
import           Control.Applicative (liftA2)
import qualified Control.Applicative as Alt
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Effect.Cull
import           Effect.NonDet

runCull :: (m b -> m b -> m b) -> (a -> m b) -> m b -> CullC m a -> m b
runCull fork leaf nil (CullC m) = runNonDet fork leaf nil (runReaderT m False)

runCullA :: (Alt.Alternative f, Applicative m) => CullC m a -> m (f a)
runCullA = runCull (liftA2 (Alt.<|>)) (pure . pure) (pure Alt.empty)

runCullM :: (Applicative m, Monoid b) => (a -> b) -> CullC m a -> m b
runCullM leaf = runCull (liftA2 mappend) (pure . leaf) (pure mempty)

-- | @since 1.0.0.0
newtype CullC m a = CullC (ReaderT Bool (NonDetC m) a)
  deriving (Applicative, Functor, Monad)

deriving instance (Algebra m, Effect (Sig m), MonadFix m) => MonadFix (CullC m)

instance MonadTrans CullC where
  lift = CullC . lift . lift
  {-# INLINE lift #-}

instance (Algebra m, Effect (Sig m)) => Algebra (CullC m) where
  type Sig (CullC m) = Cull :+: NonDet :+: Sig m

  alg = \case
    L (Cull (CullC m) k) -> CullC (local (const True) m) >>= k
    R (L (L Empty))      -> CullC empty
    R (L (R (Choose k))) -> CullC $ ReaderT $ \ cull -> do
      let CullC l = k False
          CullC r = k True
      if cull then
        NonDetC $ \ fork leaf nil ->
          runNonDet fork leaf (runNonDet fork leaf nil (runReaderT r cull)) (runReaderT l cull)
      else
        runReaderT l cull <|> runReaderT r cull
    R (R other)          -> CullC (alg (R (R (handleCoercible other))))
  {-# INLINE alg #-}
