{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
module Control.Effect.State.Named
( State(..)
, get
, gets
, put
, modify
, modifyLazy
) where

import Control.Algebra
import Control.Effect.Sum.Named
import Control.Effect.State (State(..))

-- | Get the current state value.
--
--   prop> snd (run (runState a get)) === a
get :: forall name s m . (NamedMember name (State s) (Signature m), Algebra m) => m s
get = sendNamed @name (Get pure)
{-# INLINEABLE get #-}

-- | Project a function out of the current state value.
--
--   prop> snd (run (runState a (gets (applyFun f)))) === applyFun f a
gets :: forall name s m a . (NamedMember name (State s) (Signature m), Algebra m) => (s -> a) -> m a
gets f = sendNamed @name (Get (pure . f))
{-# INLINEABLE gets #-}

-- | Replace the state value with a new value.
--
--   prop> fst (run (runState a (put b))) === b
--   prop> snd (run (runState a (get <* put b))) === a
--   prop> snd (run (runState a (put b *> get))) === b
put :: forall name s m . (NamedMember name (State s) (Signature m), Algebra m) => s -> m ()
put s = sendNamed @name (Put s (pure ()))
{-# INLINEABLE put #-}

-- | Replace the state value with the result of applying a function to the current state value.
--   This is strict in the new state.
--
--   prop> fst (run (runState a (modify (+1)))) === (1 + a :: Integer)
modify :: forall name s m . (NamedMember name (State s) (Signature m), Algebra m) => (s -> s) -> m ()
modify f = do
  a <- get @name
  put @name $! f a
{-# INLINEABLE modify #-}

-- | Replace the state value with the result of applying a function to the current state value.
--   This is lazy in the new state; injudicious use of this function may lead to space leaks.
modifyLazy :: forall name s m . (NamedMember name (State s) (Signature m), Algebra m) => (s -> s) -> m ()
modifyLazy f = get @name >>= put @name . f
{-# INLINEABLE modifyLazy #-}
