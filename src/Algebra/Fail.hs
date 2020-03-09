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

runFail :: FailC m a -> m (Either String a)
runFail = runExceptT . runFailC

newtype FailC m a = FailC { runFailC :: ExceptT String m a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadTrans)

instance Algebra m => Algebra (FailC m) where
  type Sig (FailC m) = Fail :+: Sig m

  alg ctx hdl = \case
    L (Fail s) -> FailC (throwError s)
    R other    -> FailC (alg ctx (runFailC . hdl) (R other))
  {-# INLINE alg #-}
