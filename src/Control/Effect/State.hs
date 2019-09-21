{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.State
( -- * State effect
  State(..)
, get
, gets
, put
, modify
, modifyLazy
) where

import Control.Algebra
import GHC.Generics (Generic1)
import Prelude hiding (fail)

data State s m k
  = Get (s -> m k)
  | Put s (m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

-- | Get the current state value.
--
--   prop> snd (run (runState a get)) === a
get :: (Member (State s) (Signature m), Algebra m) => m s
get = send (Get pure)
{-# INLINEABLE get #-}

-- | Project a function out of the current state value.
--
--   prop> snd (run (runState a (gets (applyFun f)))) === applyFun f a
gets :: (Member (State s) (Signature m), Algebra m) => (s -> a) -> m a
gets f = send (Get (pure . f))
{-# INLINEABLE gets #-}

-- | Replace the state value with a new value.
--
--   prop> fst (run (runState a (put b))) === b
--   prop> snd (run (runState a (get <* put b))) === a
--   prop> snd (run (runState a (put b *> get))) === b
put :: (Member (State s) (Signature m), Algebra m) => s -> m ()
put s = send (Put s (pure ()))
{-# INLINEABLE put #-}

-- | Replace the state value with the result of applying a function to the current state value.
--   This is strict in the new state.
--
--   prop> fst (run (runState a (modify (+1)))) === (1 + a :: Integer)
modify :: (Member (State s) (Signature m), Algebra m) => (s -> s) -> m ()
modify f = do
  a <- get
  put $! f a
{-# INLINEABLE modify #-}

-- | Replace the state value with the result of applying a function to the current state value.
--   This is lazy in the new state; injudicious use of this function may lead to space leaks.
modifyLazy :: (Member (State s) (Signature m), Algebra m) => (s -> s) -> m ()
modifyLazy f = get >>= put . f
{-# INLINEABLE modifyLazy #-}

-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Effect.State.Strict
