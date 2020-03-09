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

type Has eff m = (Members eff (Sig m), Algebra m)

run :: Identity a -> a
run = runIdentity
{-# INLINE run #-}

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff (Sig m), Algebra m) => eff m a -> m a
send = alg . inj
{-# INLINE send #-}
