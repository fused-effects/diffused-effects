{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, DeriveTraversable, FlexibleInstances, FunctionalDependencies, KindSignatures, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Control.Effect.Sum
( (:+:)(..)
, Member(..)
, send
) where

import Control.Algebra.Class
import Control.Effect.Class
import GHC.Generics (Generic1)

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
