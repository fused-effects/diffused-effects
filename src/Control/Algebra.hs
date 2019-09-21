{-# LANGUAGE ConstraintKinds, FlexibleContexts, TypeOperators #-}
module Control.Algebra
( -- * Re-exports
  module Control.Algebra.Class
, module Control.Effect.Class
, module Control.Effect.Sum
, run
, Handles
, send
) where

import Control.Algebra.Class
import Control.Algebra.Pure
import Control.Effect.Class
import Control.Effect.Sum

type Handles m effect = (Member effect (Signature m), Algebra m)

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: m `Handles` effect => effect m a -> m a
send = alg . inj
{-# INLINE send #-}
