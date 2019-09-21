{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, DeriveTraversable, FlexibleInstances, MultiParamTypeClasses, PolyKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
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


type family FromJust (maybe :: Maybe a) :: a where
  FromJust ('Just a) = a

type family (left :: Maybe k) <> (right :: Maybe k) :: Maybe k where
  'Nothing <> b        = b
  a        <> 'Nothing = a

type family Prepend (s :: k) (ss :: Maybe [k]) where
  Prepend s ('Just ss) = 'Just (s ': ss)
  Prepend _ 'Nothing   = 'Nothing

data Side = L_ | R_

type family PathTo' (side :: Side) sub sup :: Maybe [Side] where
  PathTo' s t t         = 'Just '[s]
  PathTo' s t (l :+: r) = Prepend s (PathTo' 'L_ t l <> PathTo' 'R_ t r)
  PathTo' _ _ _         = 'Nothing

type family PathTo sub sup :: [Side] where
  PathTo t t         = '[]
  PathTo t (l :+: r) = FromJust (PathTo' 'L_ t l <> PathTo' 'R_ t r)

class ElementAt (path :: [Side]) (sub :: (* -> *) -> (* -> *)) sup where
  prj' :: sup m a -> Maybe (sub m a)

instance ElementAt '[] t t where
  prj' = Just

instance ElementAt path t l => ElementAt ('L_ ': path) t (l :+: r) where
  prj' (L l) = prj'  @path l
  prj' _     = Nothing

instance ElementAt path t r => ElementAt ('R_ ': path) t (l :+: r) where
  prj' (R r) = prj'  @path r
  prj' _     = Nothing

class Element (sub :: (* -> *) -> (* -> *)) sup where
  proj :: sup m a -> Maybe (sub m a)

instance (PathTo sub sup ~ path, ElementAt path sub sup) => Element sub sup where
  proj = prj' @path
