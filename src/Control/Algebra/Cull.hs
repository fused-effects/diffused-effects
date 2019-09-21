{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Cull
( -- * Cull effect
  module Control.Effect.Cull
  -- * Cull carrier
, runCull
, CullC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra
import Control.Algebra.NonDet.Church
import Control.Algebra.Reader
import Control.Applicative (Alternative(..))
import Control.Effect.Cull
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Cull' effect. Branches outside of any 'cull' block will not be pruned.
--
--   prop> run (runNonDet (runCull (pure a <|> pure b))) === [a, b]
runCull :: Alternative m => CullC m a -> m a
runCull (CullC m) = runNonDetC (runReader False m) (<|>) pure empty

newtype CullC m a = CullC { runCullC :: ReaderC Bool (NonDetC m) a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO)

instance Alternative (CullC m) where
  empty = CullC empty
  {-# INLINE empty #-}
  l <|> r = CullC $ ReaderC $ \ cull ->
    if cull then
      NonDetC $ \ fork leaf nil ->
        runNonDetC (runReader cull (runCullC l)) fork leaf (runNonDetC (runReader cull (runCullC r)) fork leaf nil)
    else
      runReader cull (runCullC l) <|> runReader cull (runCullC r)
  {-# INLINE (<|>) #-}

instance MonadPlus (CullC m)

instance MonadTrans CullC where
  lift = CullC . lift . lift
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (Cull :+: NonDet :+: sig) (CullC m) where
  alg (L (Cull m k))         = CullC (local (const True) (runCullC m)) >>= k
  alg (R (L (L Empty)))      = empty
  alg (R (L (R (Choose k)))) = k True <|> k False
  alg (R (R other))          = CullC (alg (R (R (handleCoercible other))))
  {-# INLINE alg #-}
