{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Trace.Returning
( -- * Trace effect
  module Effect.Trace
  -- * Trace carrier
, runTrace
, TraceC(..)
-- * Re-exports
, Has
, run
) where

import           Algebra
import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Effect.Trace

-- | Run a 'Trace' effect, returning all traces as a list.
--
--   prop> run (runTrace (trace a *> trace b *> pure c)) === ([a, b], c)
runTrace :: Functor m => TraceC m a -> m ([String], a)
runTrace = fmap (\ (a, s) -> (reverse s, a)) . (`runStateT` []) . runTraceC

newtype TraceC m a = TraceC { runTraceC :: StateT [String] m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra m, Effect (Sig m)) => Algebra (TraceC m) where
  type Sig (TraceC m) = Trace :+: Sig m

  alg (L (Trace m k)) = TraceC (modify (m :)) *> k
  alg (R other)       = TraceC (alg (R (handleCoercible other)))
