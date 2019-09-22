{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Trace.Printing
( -- * Trace effect
  module Control.Effect.Trace
  -- * Trace carrier
, runTrace
, TraceC(..)
-- * Re-exports
, Has
, run
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Effect.Trace
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.IO

-- | Run a 'Trace' effect, printing traces to 'stderr'.
runTrace :: TraceC m a -> m a
runTrace = runTraceC

newtype TraceC m a = TraceC { runTraceC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TraceC where
  lift = TraceC
  {-# INLINE lift #-}

instance (MonadIO m, Algebra m) => Algebra (TraceC m) where
  type Signature (TraceC m) = Trace :+: Signature m
  alg (L (Trace s k)) = liftIO (hPutStrLn stderr s) *> k
  alg (R other)       = TraceC (alg (handleCoercible other))
  {-# INLINE alg #-}
