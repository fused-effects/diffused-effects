{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Sum
( (:+:)(..)
, Member(..)
) where

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

instance {-# OVERLAPPABLE #-}
         Member sub (l1 :+: l2 :+: r)
      => Member sub ((l1 :+: l2) :+: r) where
  inj = reassoc . inj where
    reassoc (L l)     = L (L l)
    reassoc (R (L l)) = L (R l)
    reassoc (R (R r)) = R r
  prj = prj . reassoc where
    reassoc (L (L l)) = L l
    reassoc (L (R l)) = R (L l)
    reassoc (R r)     = R (R r)

instance {-# OVERLAPPABLE #-}
         Member sub (sub :+: r) where
  inj = L
  prj (L l) = Just l
  prj _     = Nothing

instance {-# OVERLAPPABLE #-}
         Member sub r
      => Member sub (l :+: r) where
  inj = R . inj
  prj (R r) = prj r
  prj _     = Nothing
