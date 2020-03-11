{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Lift.Internal
( Lift(..)
) where

data Lift sig m k
  = forall a . LiftWith
    (forall ctx . Functor ctx => (forall a . ctx (m a) -> sig (ctx a)) -> ctx () -> sig (ctx a))
    (a -> m k)
