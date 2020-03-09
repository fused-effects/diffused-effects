{-# LANGUAGE ExplicitForAll, FlexibleInstances, GeneralizedNewtypeDeriving, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Named
( runNamed
, NamedC(..)
) where

import Control.Algebra
import Control.Effect.Sum.Named

runNamed :: NamedC name m a -> m a
runNamed = runNamedC

newtype NamedC name (m :: * -> *) a = NamedC { runNamedC :: m a }
  deriving (Applicative, Functor, Monad)

type family Lhs a where
  Lhs (a :+: _) = a

type family Rhs a where
  Rhs (_ :+: b) = b

instance (Algebra m, Sig m ~ (eff :+: sig), HFunctor eff, HFunctor sig) => Algebra (NamedC name m) where
  type Sig (NamedC name m) = Named name (Lhs (Sig m)) :+: Rhs (Sig m)
  alg (L eff) = NamedC . alg . handleCoercible . L $ getNamed eff
  alg (R eff) = NamedC . alg . handleCoercible . R $ eff
