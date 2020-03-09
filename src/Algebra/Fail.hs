{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Fail
( -- * Fail effect
  module Effect.Fail
  -- * Fail carrier
, runFail
, FailC(..)
  -- * Re-exports
, Has
, run
) where

import Algebra
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Effect.Fail
import Effect.Throw

-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
--   prop> run (runFail (pure a)) === Right a
runFail :: FailC m a -> m (Either String a)
runFail = runExceptT . runFailC

newtype FailC m a = FailC { runFailC :: ExceptT String m a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadTrans)

instance (Algebra m, Effect (Sig m)) => Algebra (FailC m) where
  type Sig (FailC m) = Fail :+: Sig m

  alg = \case
    L (Fail s) -> FailC (throwError s)
    R other    -> FailC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}
