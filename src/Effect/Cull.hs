{-# LANGUAGE ExistentialQuantification #-}
module Effect.Cull
( -- * Cull effect
  Cull(..)
, cull
  -- * Re-exports
, module Effect.NonDet
) where

import Algebra
import Effect.NonDet

data Cull m k
  = forall a . Cull (m a) (a -> m k)

cull :: Has Cull m => m a -> m a
cull m = send (Cull m pure)
