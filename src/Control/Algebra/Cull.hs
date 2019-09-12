{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Cull
( -- * Cull effect
  module Control.Effect.Cull
  -- * Cull Algebra
, runCull
, CullC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.Algebra.Class
import Control.Algebra.NonDet.Church
import Control.Algebra.Reader
import Control.Effect.Cull
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bool (bool)

-- | Run a 'Cull' effect. Branches outside of any 'cull' block will not be pruned.
--
--   prop> run (runNonDet (runCull (pure a <|> pure b))) === [a, b]
runCull :: Alternative m => CullC m a -> m a
runCull (CullC m) = runNonDetC (runReader False m) (<|>) pure empty

newtype CullC m a = CullC { runCullC :: ReaderC Bool (NonDetC m) a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO)

instance (Algebra sig m, Effect sig) => Alternative (CullC m) where
  empty = send Empty
  {-# INLINE empty #-}
  l <|> r = send (Choose (bool r l))
  {-# INLINE (<|>) #-}

instance (Algebra sig m, Effect sig) => MonadPlus (CullC m)

instance MonadTrans CullC where
  lift = CullC . lift . lift
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (Cull :+: Empty :+: Choose :+: sig) (CullC m) where
  alg (L (Cull m k))         = CullC (local (const True) (runCullC m)) >>= k
  alg (R (L Empty))          = CullC empty
  alg (R (R (L (Choose k)))) = CullC $ ReaderC $ \ cull ->
    if cull then
      NonDetC $ \ fork leaf nil ->
        runNonDetC (runReader cull (runCullC (k True))) fork leaf (runNonDetC (runReader cull (runCullC (k False))) fork leaf nil)
    else
      runReader cull (runCullC (k True)) <|> runReader cull (runCullC (k False))
  alg (R (R (R other)))      = CullC (alg (R (R (R (handleCoercible other)))))
  {-# INLINE alg #-}
