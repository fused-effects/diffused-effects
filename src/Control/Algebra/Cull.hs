{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Algebra.Cull
( -- * Cull effect
  module Control.Effect.Cull
  -- * Cull carrier
, runCull
, CullC(..)
) where

import           Control.Algebra
import           Control.Algebra.NonDet.Church
import           Control.Effect.Cull
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader

-- | Run a 'Cull' effect. Branches outside of any 'cull' block will not be pruned.
--
--   prop> run (runNonDet (runCull (pure a <|> pure b))) === [a, b]
runCull :: Alternative m => CullC m a -> m a
runCull (CullC m) = runNonDetC (runReaderT m False) (<|>) pure empty

newtype CullC m a = CullC { runCullC :: ReaderT Bool (NonDetC m) a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO)

instance Alternative (CullC m) where
  empty = CullC empty
  {-# INLINE empty #-}
  l <|> r = CullC $ ReaderT $ \ cull ->
    if cull then
      NonDetC $ \ fork leaf nil ->
        runNonDetC (runReaderT (runCullC l) cull) fork leaf (runNonDetC (runReaderT (runCullC r) cull) fork leaf nil)
    else
      runReaderT (runCullC l) cull <|> runReaderT (runCullC r) cull
  {-# INLINE (<|>) #-}

instance MonadPlus (CullC m)

instance MonadTrans CullC where
  lift = CullC . lift . lift
  {-# INLINE lift #-}

instance (Algebra m, Effect (Signature m)) => Algebra (CullC m) where
  type Signature (CullC m) = Cull :+: NonDet :+: Signature m
  alg (L (Cull m k))         = CullC (local (const True) (runCullC m)) >>= k
  alg (R (L (L Empty)))      = empty
  alg (R (L (R (Choose k)))) = k True <|> k False
  alg (R (R other))          = CullC (alg (R (R (handleCoercible other))))
  {-# INLINE alg #-}
