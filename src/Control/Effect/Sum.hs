{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, PolyKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Effect.Sum
( (:+:)(..)
, Member
, inj
, prj
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


type Member (sub :: (* -> *) -> (* -> *)) sup = MemberAt (PathTo sub sup) sub sup

inj :: forall sub m a sup . Member sub sup => sub m a -> sup m a
inj = inj' @(PathTo sub sup)

prj :: forall sub m a sup . Member sub sup => sup m a -> Maybe (sub m a)
prj = prj' @(PathTo sub sup)


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member effect sig, Algebra sig m) => effect m a -> m a
send = alg . inj
{-# INLINE send #-}


type family FromJust (maybe :: Maybe a) :: a where
  FromJust ('Just a) = a

type family (left :: Maybe k) <|> (right :: Maybe k) :: Maybe k where
  'Just a <|> _       = 'Just a
  _       <|> 'Just b = 'Just b
  _       <|> _       = 'Nothing

type family Prepend (s :: j -> k) (ss :: Maybe j) :: Maybe k where
  Prepend s ('Just ss) = 'Just (s ss)
  Prepend _ 'Nothing   = 'Nothing

data L a
data R a

type family PathTo' (side :: * -> *) (sub :: (* -> *) -> (* -> *)) sup :: Maybe * where
  PathTo' s t t         = 'Just (s ())
  PathTo' s t (l :+: r) = Prepend s (PathTo' L t l <|> PathTo' R t r)
  PathTo' _ _ _         = 'Nothing

type family PathTo sub sup where
  PathTo t t         = ()
  PathTo t (l :+: r) = FromJust (PathTo' L t l <|> PathTo' R t r)

class MemberAt path (sub :: (* -> *) -> (* -> *)) sup | path sup -> sub where
  inj' :: sub m a -> sup m a
  prj' :: sup m a -> Maybe (sub m a)

instance MemberAt () t t where
  inj' = id
  prj' = Just

instance MemberAt path t l => MemberAt (L path) t (l :+: r) where
  inj' = L . inj' @path
  prj' (L l) = prj'  @path l
  prj' _     = Nothing

instance MemberAt path t r => MemberAt (R path) t (l :+: r) where
  inj' = R . inj' @path
  prj' (R r) = prj'  @path r
  prj' _     = Nothing
