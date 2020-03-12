module Effect.State
( -- * State effect
  State(..)
, get
, gets
, put
, modify
, modifyLazy
, state
  -- * Re-exports
, Algebra
, Has
, run
) where

import Algebra
import Effect.State.Internal (State(..))

get :: Has (State s) m => m s
get = send Get
{-# INLINEABLE get #-}

gets :: Has (State s) m => (s -> a) -> m a
gets = (`fmap` get)
{-# INLINEABLE gets #-}

put :: Has (State s) m => s -> m ()
put s = send (Put s)
{-# INLINEABLE put #-}

modify :: Has (State s) m => (s -> s) -> m ()
modify f = do
  a <- get
  put $! f a
{-# INLINEABLE modify #-}

modifyLazy :: Has (State s) m => (s -> s) -> m ()
modifyLazy f = get >>= put . f
{-# INLINEABLE modifyLazy #-}

state :: Has (State s) m => (s -> (s, a)) -> m a
state f = do
  (s', a) <- gets f
  a <$ put s'
{-# INLINEABLE state #-}
