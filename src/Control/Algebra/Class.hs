{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Class
( Algebra(..)
, send
-- * Re-exports
, module Control.Effect.Class
, Pure.run
, (Sum.:+:)(..)
, Sum.Member(..)
) where

import qualified Control.Algebra.Pure as Pure
import           Control.Effect.Class
import qualified Control.Effect.Sum as Sum

-- | The class of algebras (effect handlers) for carriers (monad transformers) over signatures (effects), whose actions are given by the 'alg' method.
class (HFunctor sig, Monad m) => Algebra sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  alg :: sig m a -> m a


instance Algebra Pure.Pure Pure.PureC where
  alg v = case v of {}
  {-# INLINE alg #-}


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Sum.Member effect sig, Algebra sig m) => effect m a -> m a
send = alg . Sum.inj
{-# INLINE send #-}
