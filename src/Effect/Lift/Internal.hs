{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Lift.Internal
( Lift(..)
) where

data Lift sig m k where
  LiftWith :: (forall ctx . Functor ctx => (forall a . ctx (m a) -> sig (ctx a)) -> ctx () -> sig (ctx a)) -> Lift sig m a
