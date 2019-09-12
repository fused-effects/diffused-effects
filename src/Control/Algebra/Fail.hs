{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Fail
( -- * Fail effect
  module Control.Effect.Fail
  -- * Fail Algebra
, runFail
, FailC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra.Class
import Control.Algebra.Error.Either
import Control.Effect.Fail
import Control.Monad.Trans.Class

-- | Run a 'Fail' effect, returning failure messages in 'Left' and successful computations’ results in 'Right'.
--
--   prop> run (runFail (pure a)) === Right a
runFail :: FailC m a -> m (Either String a)
runFail = runError . runFailC

newtype FailC m a = FailC { runFailC :: ErrorC String m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

instance (Algebra sig m, Effect sig) => Algebra (Fail :+: sig) (FailC m) where
  alg (L (Fail s)) = FailC (throwError s)
  alg (R other)    = FailC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
