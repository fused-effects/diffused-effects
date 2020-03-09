{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Effect.Sum
( -- * Membership
  Member(..)
, Members
  -- * Sums
, (:+:)(..)
, reassociateSumL
) where

import Data.Kind (Constraint)

data (f :+: g) (m :: * -> *) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 4 :+:


class Member (sub :: (* -> *) -> (* -> *)) sup where
  inj :: sub m a -> sup m a

instance Member t t where
  inj = id
  {-# INLINE inj #-}

instance {-# OVERLAPPABLE #-}
         Member t (l1 :+: l2 :+: r)
      => Member t ((l1 :+: l2) :+: r) where
  inj = reassociateSumL . inj
  {-# INLINE inj #-}

instance {-# OVERLAPPABLE #-}
         Member l (l :+: r) where
  inj = L
  {-# INLINE inj #-}

instance {-# OVERLAPPABLE #-}
         Member l r
      => Member l (l' :+: r) where
  inj = R . inj
  {-# INLINE inj #-}


reassociateSumL :: (l1 :+: l2 :+: r) m a -> ((l1 :+: l2) :+: r) m a
reassociateSumL = \case
  L l     -> L (L l)
  R (L l) -> L (R l)
  R (R r) -> R r
{-# INLINE reassociateSumL #-}


type family Members sub sup :: Constraint where
  Members (l :+: r) u = (Members l u, Members r u)
  Members t         u = Member t u
