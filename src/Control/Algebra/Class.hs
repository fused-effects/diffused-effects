{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Control.Algebra.Class
( Algebra(..)
) where

import Control.Effect.Class

-- | The class of algebras (effect handlers) for carriers (monad transformers) over signatures (effects), whose actions are given by the 'alg' method.
class (HFunctor (Signature m), Monad m) => Algebra m where
  type Signature m :: (* -> *) -> (* -> *)
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  alg :: Signature m m a -> m a
