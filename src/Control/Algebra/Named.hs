{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, PolyKinds, UndecidableInstances #-}
module Control.Algebra.Named
( NamedC(..)
) where

import Control.Algebra
import Control.Effect.Sum.Named

newtype NamedC (name :: k) m a = NamedC { runNamed :: m a }
  deriving (Applicative, Functor, Monad)

instance Algebra sig m => Algebra (Named name sig) (NamedC name m) where
  alg = NamedC . alg . handleCoercible . getNamed
