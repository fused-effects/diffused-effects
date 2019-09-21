{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
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

type Has effect m = (Member effect (Signature m), Algebra m)

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: Has effect m => effect m a -> m a
send = alg . inj
{-# INLINE send #-}
