{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Algebra.Class
( Algebra(..)
) where

import Control.Effect.Catch.Internal
import Control.Effect.Class
import Control.Effect.Empty.Internal
import Control.Effect.Error.Internal
import Control.Effect.Sum
import Control.Effect.Throw.Internal

instance Algebra Maybe where
  type Signature Maybe = Empty

  alg Empty = Nothing

-- | The class of algebras (effect handlers) for carriers (monad transformers) over signatures (effects), whose actions are given by the 'alg' method.
class (HFunctor (Signature m), Monad m) => Algebra m where
  type Signature m :: (* -> *) -> (* -> *)
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  alg :: Signature m m a -> m a

instance Algebra (Either e) where
  type Signature (Either e) = Error e

  alg = \case
    L (Throw e)     -> Left e
    R (Catch m h k) -> either h pure m >>= k
