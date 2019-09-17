{-# LANGUAGE ExplicitForAll, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
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

instance (Algebra (eff :+: sig) m, HFunctor eff, HFunctor sig) => Algebra (Named name eff :+: sig) (NamedC name m) where
  alg (L eff) = NamedC . alg . handleCoercible . L $ getNamed eff
  alg (R eff) = NamedC . alg . handleCoercible . R $ eff
