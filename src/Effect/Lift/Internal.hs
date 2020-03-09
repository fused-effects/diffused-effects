{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Lift.Internal
( Lift(..)
) where

import Effect.Class
import Data.Functor.Compose

data Lift sig m k
  = forall a . LiftWith
    (forall ctx . Functor ctx => ctx () -> (forall a . ctx (m a) -> sig (ctx a)) -> sig (ctx a))
    (a -> m k)

instance Functor m => Functor (Lift sig m) where
  fmap f (LiftWith with k) = LiftWith with (fmap f . k)

instance HFunctor (Lift sig) where
  hmap f (LiftWith go k) = LiftWith (\c lift -> go c (lift . fmap f)) (f . k)

instance Functor sig => Effect (Lift sig) where
  handle ctx dst (LiftWith with k) = LiftWith
    (\ ctx' dst' -> getCompose <$> with (Compose (ctx <$ ctx')) (fmap Compose . dst' . fmap dst . getCompose))
    (dst . fmap k)
