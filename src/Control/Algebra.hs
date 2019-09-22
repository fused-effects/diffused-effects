{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Control.Algebra
( -- * Re-exports
  module Control.Algebra.Class
, module Control.Algebra.Pure
, module Control.Effect.Class
, (:+:)(..)
, Has
, send
) where

import Control.Algebra.Class
import Control.Algebra.Pure
import Control.Effect.Class
import Control.Effect.Sum

type Has eff m = HasIn (Signature m) eff m

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: Has eff m => eff m a -> m a
send = alg . inj
{-# INLINE send #-}


class (Algebra m, Member eff sig) => HasIn sig eff m
instance {-# OVERLAPPABLE #-} (Algebra m, Member eff (l :+: r)) => HasIn (l :+: r) eff m
instance {-# OVERLAPPABLE #-} Algebra m => HasIn eff eff m
