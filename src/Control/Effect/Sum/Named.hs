{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Control.Effect.Sum.Named
( Named(..)
, NamedMember(..)
, HasNamed
, sendNamed
) where

import Control.Algebra

newtype Named (name :: k) (eff :: (* -> *) -> (* -> *)) m a = Named { getNamed :: eff m a }
  deriving (HFunctor, Effect)


class NamedMember (name :: k) sub sup | name sup -> sub where
  injNamed :: Named name sub m a -> sup m a

instance NamedMember name sub (Named name sub) where
  injNamed = id

instance {-# OVERLAPPABLE #-} NamedMember name sub (Named name sub :+: sup) where
  injNamed = L

instance {-# OVERLAPPABLE #-} NamedMember name sub sup => NamedMember name sub (sub' :+: sup) where
  injNamed = R . injNamed


type HasNamed name eff m = (Algebra m, HasNamedIn name (Sig m) eff)

-- | Construct a request for a named effect to be interpreted by some handler later on.
sendNamed :: forall name eff m a . HasNamed name eff m => eff m a -> m a
sendNamed = alg . injNamed . Named @_ @name
{-# INLINE sendNamed #-}


class NamedMember name eff sig => HasNamedIn name sig eff
instance {-# OVERLAPPABLE #-} HasNamedIn name (Named name eff) eff
instance {-# OVERLAPPABLE #-} NamedMember name eff (l :+: r) => HasNamedIn name (l :+: r) eff
