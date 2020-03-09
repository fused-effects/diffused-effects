{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Algebra.State.Strict
( -- * State effect
  module Control.Effect.State
  -- * Strict state carrier
, runState
, evalState
, execState
, StateC(..)
) where

import           Control.Algebra
import           Control.Applicative (Alternative(..))
import           Control.Effect.State
import           Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

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
  m *> k = m >>= const k
  {-# INLINE (*>) #-}

instance (Alternative m, Monad m) => Alternative (StateC s m) where
  empty = StateC (const empty)
  {-# INLINE empty #-}
  StateC l <|> StateC r = StateC (\ s -> l s <|> r s)
  {-# INLINE (<|>) #-}

instance Monad m => Monad (StateC s m) where
  StateC m >>= f = StateC $ \ s -> do
    (s', a) <- m s
    let fa = f a
    fa `seq` runState s' fa
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (StateC s m) where
  fail s = StateC (const (Fail.fail s))
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (StateC s m) where
  mfix f = StateC (\ s -> mfix (runState s . f . snd))
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (StateC s m) where
  liftIO io = StateC (\ s -> (,) s <$> liftIO io)
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (StateC s m)

instance MonadTrans (StateC s) where
  lift m = StateC (\ s -> (,) s <$> m)
  {-# INLINE lift #-}

instance (Algebra m, Effect (Signature m)) => Algebra (StateC s m) where
  type Signature (StateC s m) = State s :+: Signature m
  alg (L (Get   k)) = StateC (\ s -> runState s (k s))
  alg (L (Put s k)) = StateC (\ _ -> runState s k)
  alg (R other)     = StateC (\ s -> alg (handle (s, ()) (uncurry runState) other))
  {-# INLINE alg #-}
