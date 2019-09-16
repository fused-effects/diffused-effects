{-# LANGUAGE DataKinds, DeriveGeneric, DeriveTraversable, FlexibleInstances, FunctionalDependencies, KindSignatures, TypeOperators, UndecidableInstances #-}
module Control.Effect.Sum
( (:+:)(..)
, Member(..)
, send
, Named(..)
, NamedMember(..)
) where

import Control.Algebra.Class
import Control.Effect.Class
import GHC.Generics (Generic1)
import GHC.TypeLits

data (f :+: g) (m :: * -> *) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

infixr 4 :+:

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g)
instance (Effect f, Effect g)     => Effect   (f :+: g)


class Member (sub :: (* -> *) -> (* -> *)) sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance Member sub sub where
  inj = id
  prj = Just

instance {-# OVERLAPPABLE #-} Member sub (sub :+: sup) where
  inj = L . inj
  prj (L f) = Just f
  prj _     = Nothing

instance {-# OVERLAPPABLE #-} Member sub sup => Member sub (sub' :+: sup) where
  inj = R . inj
  prj (R g) = prj g
  prj _     = Nothing


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member effect sig, Algebra sig m) => effect m a -> m a
send = alg . inj
{-# INLINE send #-}


newtype Named (name :: Symbol) (eff :: (* -> *) -> (* -> *)) m k = Named { getNamed :: eff m k }

class NamedMember (name :: Symbol) sub sup | name sup -> sub where
  injNamed :: Named name sub m k -> sup m k

instance NamedMember name sub (Named name sub) where
  injNamed = id

instance {-# OVERLAPPABLE #-} NamedMember name sub (Named name sub :+: sup) where
  injNamed = L

instance {-# OVERLAPPABLE #-} NamedMember name sub sup => NamedMember name sub (sub' :+: sup) where
  injNamed = R . injNamed
