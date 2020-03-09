{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Lift.Internal
( Lift(..)
) where

data Lift sig m k
  = forall a . LiftWith
    (forall ctx . Functor ctx => ctx () -> (forall a . ctx (m a) -> sig (ctx a)) -> sig (ctx a))
    (a -> m k)
