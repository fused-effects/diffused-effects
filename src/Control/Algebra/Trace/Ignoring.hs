{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Trace.Ignoring
( -- * Trace effect
  module Control.Effect.Trace
  -- * Trace carrier
, runTrace
, TraceC(..)
-- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra.Class
import Control.Effect.Trace
import Control.Monad.Trans.Class

-- | Run a 'Trace' effect, ignoring all traces.
--
--   prop> run (runTrace (trace a *> pure b)) === b
runTrace :: TraceC m a -> m a
runTrace = runTraceC

newtype TraceC m a = TraceC { runTraceC :: m a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans TraceC where
  lift = TraceC
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Trace :+: sig) (TraceC m) where
  alg (L trace) = traceCont trace
  alg (R other) = TraceC (alg (handleCoercible other))
  {-# INLINE alg #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
