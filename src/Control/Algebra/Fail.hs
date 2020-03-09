{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Algebra.Fail
( -- * Fail effect
  module Control.Effect.Fail
, Fail.MonadFail(..)
  -- * Fail carrier
, runFail
, FailC(..)
  -- * Re-exports
, Has
, run
) where

import           Control.Algebra
import           Control.Applicative (Alternative(..))
import           Control.Effect.Throw
import           Control.Effect.Fail
import           Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
--   prop> run (runFail (pure a)) === Right a
runFail :: FailC m a -> m (Either String a)
runFail = runExceptT . runFailC

newtype FailC m a = FailC { runFailC :: ExceptT String m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra m, Effect (Sig m)) => Fail.MonadFail (FailC m) where
  fail s = FailC (throwError s)
  {-# INLINE fail #-}

instance (Algebra m, Effect (Sig m)) => Algebra (FailC m) where
  type Sig (FailC m) = Fail :+: Sig m
  alg (L (Fail s)) = Fail.fail s
  alg (R other)    = FailC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}
