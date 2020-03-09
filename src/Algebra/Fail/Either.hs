{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Fail.Either
( -- * Fail effect
  module Effect.Fail
  -- * Fail carrier
, runFail
, FailT(..)
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

runFail :: FailT m a -> m (Either String a)
runFail = runExceptT . runFailT

newtype FailT m a = FailT { runFailT :: ExceptT String m a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadTrans)

instance Algebra m => Algebra (FailT m) where
  type Sig (FailT m) = Fail :+: Sig m

  alg ctx hdl = \case
    L (Fail s) -> FailT (throwError s)
    R other    -> FailT (alg ctx (runFailT . hdl) (R other))
  {-# INLINE alg #-}
