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

import Control.Applicative (Alternative(..))
import Control.Algebra.Class
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

instance (MonadIO m, Algebra sig m) => Algebra (Trace :+: sig) (TraceC m) where
  eff (L (Trace s k)) = liftIO (hPutStrLn stderr s) *> k
  eff (R other)       = TraceC (eff (handleCoercible other))
  {-# INLINE eff #-}
