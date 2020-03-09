{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra
( -- * Re-exports
  module Control.Algebra.Class
, module Control.Effect.Class
, (:+:)(..)
, Has
, run
, send
) where

import Control.Algebra.Class
import Control.Effect.Class
import Control.Effect.Sum
import Data.Functor.Identity

type Has eff m = (Algebra m, HasIn (Sig m) eff)

run :: Identity a -> a
run = runIdentity
{-# INLINE run #-}

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: Has eff m => eff m a -> m a
send = alg . inj
{-# INLINE send #-}


class Member eff sig => HasIn sig eff
instance {-# OVERLAPPABLE #-} HasIn eff eff
instance {-# OVERLAPPABLE #-} Member eff (l :+: r) => HasIn (l :+: r) eff
