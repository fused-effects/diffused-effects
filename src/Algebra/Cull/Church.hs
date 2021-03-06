{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Cull.Church
( -- * Cull carrier
  runCull
, runCullA
, runCullM
, CullT(..)
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

runCull :: (m b -> m b -> m b) -> (a -> m b) -> m b -> CullT m a -> m b
runCull fork leaf nil (CullT m) = runNonDet fork leaf nil (runReaderT m False)

runCullA :: (Alt.Alternative f, Applicative m) => CullT m a -> m (f a)
runCullA = runCull (liftA2 (Alt.<|>)) (pure . pure) (pure Alt.empty)

runCullM :: (Applicative m, Monoid b) => (a -> b) -> CullT m a -> m b
runCullM leaf = runCull (liftA2 mappend) (pure . leaf) (pure mempty)

-- | @since 1.0.0.0
newtype CullT m a = CullT { runCullT :: ReaderT Bool (NonDetT m) a }
  deriving (Applicative, Functor, Monad, MonadFix)

instance MonadTrans CullT where
  lift = CullT . lift . lift
  {-# INLINE lift #-}

instance Algebra m => Algebra (CullT m) where
  type Sig (CullT m) = Cull :+: NonDet :+: Sig m

  alg hdl ctx = \case
    L (Cull m k)     -> CullT (local (const True) (runCullT (hdl (m <$ ctx)))) >>= hdl . fmap k
    R (L (L Empty))  -> CullT empty
    R (L (R Choose)) -> CullT $ ReaderT $ \ cull -> do
      let CullT l = pure (False <$ ctx)
          CullT r = pure (True  <$ ctx)
      if cull then
        NonDetT $ \ fork leaf nil ->
          runNonDet fork leaf (runNonDet fork leaf nil (runReaderT r cull)) (runReaderT l cull)
      else
        runReaderT l cull <|> runReaderT r cull
    R (R other)      -> CullT (alg (runCullT . hdl) ctx (R (R other)))
  {-# INLINE alg #-}
