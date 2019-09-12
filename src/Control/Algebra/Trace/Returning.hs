{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Trace.Returning
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

import Control.Applicative (Alternative(..))
import Control.Algebra.Class
import Control.Algebra.State.Strict
import Control.Effect.Trace
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor (first)

-- | Run a 'Trace' effect, returning all traces as a list.
--
--   prop> run (runTrace (trace a *> trace b *> pure c)) === ([a, b], c)
runTrace :: Functor m => TraceC m a -> m ([String], a)
runTrace = fmap (first reverse) . runState [] . runTraceC

newtype TraceC m a = TraceC { runTraceC :: StateC [String] m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, Effect sig) => Algebra (Trace :+: sig) (TraceC m) where
  alg (L (Trace m k)) = TraceC (modify (m :)) *> k
  alg (R other)       = TraceC (alg (R (handleCoercible other)))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
