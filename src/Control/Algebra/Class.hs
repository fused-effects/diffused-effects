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

-- | The class of Algebras (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Algebra sig m | m -> sig where
  -- | Construct a value in the Algebra for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m a -> m a


instance Algebra Pure.Pure Pure.PureC where
  eff v = case v of {}
  {-# INLINE eff #-}


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Sum.Member effect sig, Algebra sig m) => effect m a -> m a
send = eff . Sum.inj
{-# INLINE send #-}
