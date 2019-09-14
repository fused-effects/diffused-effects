{-# LANGUAGE DeriveFunctor, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.State.Strict
( -- * State effect
  module Control.Effect.State
  -- * Strict state carrier
, runState
, evalState
, execState
, StateC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra.Class
import Control.Effect.State
import Control.Monad.Trans.Class

-- | Run a 'State' effect starting from the passed value.
--
--   prop> run (runState a (pure b)) === (a, b)
runState :: s -> StateC s m a -> m (s, a)
runState s x = runStateC x s
{-# INLINE[3] runState #-}

-- | Run a 'State' effect, yielding the result value and discarding the final state.
--
--   prop> run (evalState a (pure b)) === b
evalState :: forall s m a . Functor m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s
{-# INLINE[3] evalState #-}

-- | Run a 'State' effect, yielding the final state and discarding the return value.
--
--   prop> run (execState a (pure b)) === a
execState :: forall s m a . Functor m => s -> StateC s m a -> m s
execState s = fmap fst . runState s
{-# INLINE[3] execState #-}


newtype StateC s m a = StateC { runStateC :: s -> m (s, a) }
  deriving (Functor)

instance Monad m => Applicative (StateC s m) where
  pure a = StateC (\ s -> pure (s, a))
  {-# INLINE pure #-}
  StateC f <*> StateC a = StateC $ \ s -> do
    (s', f') <- f s
    (s'', a') <- a s'
    let fa = f' a'
    fa `seq` pure (s'', fa)
  {-# INLINE (<*>) #-}
  m *> k = m >>= \_ -> k
  {-# INLINE (*>) #-}

instance Monad m => Monad (StateC s m) where
  StateC m >>= f = StateC $ \ s -> do
    (s', a) <- m s
    let fa = f a
    fa `seq` runState s' fa
  {-# INLINE (>>=) #-}

instance MonadTrans (StateC s) where
  lift m = StateC (\ s -> (,) s <$> m)
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (State s :+: sig) (StateC s m) where
  alg (L (Get   k)) = StateC (\ s -> runState s (k s))
  alg (L (Put s k)) = StateC (\ _ -> runState s k)
  alg (R other)     = StateC (\ s -> alg (handle (s, ()) (uncurry runState) other))
  {-# INLINE alg #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
