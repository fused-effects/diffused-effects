{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Class
( Algebra(..)
) where

import qualified Control.Algebra.Pure as Pure
import           Control.Effect.Class

-- | The class of algebras (effect handlers) for carriers (monad transformers) over signatures (effects), whose actions are given by the 'alg' method.
class (HFunctor sig, Monad m) => Algebra sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  alg :: sig m a -> m a


instance Algebra Pure.Pure Pure.PureC where
  alg v = case v of {}
  {-# INLINE alg #-}
