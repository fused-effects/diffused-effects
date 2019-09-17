{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Control.Effect.Sum.Named
( Named(..)
, NamedMember(..)
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


-- | Construct a request for a named effect to be interpreted by some handler later on.
sendNamed :: forall name effect sig m a . (NamedMember name effect sig, Algebra sig m) => effect m a -> m a
sendNamed = alg . injNamed . Named @name
{-# INLINE sendNamed #-}
