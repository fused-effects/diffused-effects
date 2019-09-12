{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Trace.Printing
( -- * Trace effect
  module Control.Effect.Trace
  -- * Trace Algebra
, runTrace
, TraceC(..)
-- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra.Class
import Control.Effect.Lift
import Control.Effect.Trace
import Control.Monad.Trans.Class
import System.IO

-- | Run a 'Trace' effect, printing traces to 'stderr'.
runTrace :: TraceC m a -> m a
runTrace = runTraceC

newtype TraceC m a = TraceC { runTraceC :: m a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans TraceC where
  lift = TraceC
  {-# INLINE lift #-}

instance (Algebra sig m, Member (Lift IO) sig) => Algebra (Trace :+: sig) (TraceC m) where
  alg (L (Trace s k)) = sendM (hPutStrLn stderr s) *> k
  alg (R other)       = TraceC (alg (handleCoercible other))
  {-# INLINE alg #-}
