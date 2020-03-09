{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Algebra.Trace.Ignoring
( -- * Trace effect
  module Effect.Trace
  -- * Trace carrier
, runTrace
, TraceC(..)
-- * Re-exports
, Has
, run
) where

import Algebra
import Control.Applicative (Alternative(..))
import Effect.Trace
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Trace' effect, ignoring all traces.
--
--   prop> run (runTrace (trace a *> pure b)) === b
runTrace :: TraceC m a -> m a
runTrace = runTraceC

newtype TraceC m a = TraceC { runTraceC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TraceC where
  lift = TraceC
  {-# INLINE lift #-}

instance Algebra m => Algebra (TraceC m) where
  type Sig (TraceC m) = Trace :+: Sig m
  alg (L trace) = traceCont trace
  alg (R other) = TraceC (alg (handleCoercible other))
  {-# INLINE alg #-}
