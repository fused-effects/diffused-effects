{-# LANGUAGE RankNTypes #-}
module Effect.Lift
( -- * Lift effect
  Lift(..)
, sendM
, sendIO
, liftWith
  -- * Re-exports
, Algebra
, Has
, run
) where

import Algebra
import Effect.Lift.Internal (Lift(..))

sendM :: (Has (Lift n) m, Functor n) => n a -> m a
sendM m = liftWith (\ _ ctx -> (<$ ctx) <$> m)

sendIO :: Has (Lift IO) m => IO a -> m a
sendIO = sendM

liftWith
  :: Has (Lift n) m
  => (forall ctx . Functor ctx => (forall a . ctx (m a) -> n (ctx a)) -> ctx () -> n (ctx a))
  -> m a
liftWith with = send (LiftWith with)
