{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Effect.Sum
( (:+:)(..)
, Member
, inj
, prj
) where

import Control.Effect.Class
import GHC.Generics (Generic1)
import GHC.TypeLits (ErrorMessage(..), TypeError)

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


type family FromMaybe (x :: a) (maybe :: Maybe a) :: a where
  FromMaybe _ ('Just a) = a
  FromMaybe a 'Nothing  = a

type family (left :: Maybe k) <|> (right :: Maybe k) :: Maybe k where
  'Just a <|> _       = 'Just a
  _       <|> 'Just b = 'Just b
  _       <|> _       = 'Nothing

type family Prepend (s :: j -> k) (ss :: Maybe j) :: Maybe k where
  Prepend s ('Just ss) = 'Just (s ss)
  Prepend _ 'Nothing   = 'Nothing

data L a
data R a
data E a

type family PathTo' (side :: * -> *) (sub :: (* -> *) -> (* -> *)) sup :: Maybe * where
  PathTo' s t t         = 'Just (s ())
  PathTo' s t (l :+: r) = Prepend s (PathTo' L t l <|> PathTo' R t r)
  PathTo' _ _ _         = 'Nothing

type family PathTo sub sup where
  PathTo t t         = ()
  PathTo t (l :+: r) = FromMaybe (E (Parens ('ShowType t) ':<>: 'Text " is not a member of " ':<>: Parens ('ShowType (l :+: r)))) (PathTo' L t l <|> PathTo' R t r)
  PathTo t s         = E (Parens ('ShowType t) ':<>: 'Text " is not a member of " ':<>: Parens ('ShowType s))

type Parens t = 'Text "(" ':<>: t ':<>: 'Text ")"

class MemberAt path (sub :: (* -> *) -> (* -> *)) sup where
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

instance TypeError err => MemberAt (E err) t u where
  inj' _ = undefined
  prj' _ = undefined
