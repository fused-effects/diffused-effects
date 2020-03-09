{-# LANGUAGE KindSignatures #-}
module Effect.Fail
( -- * Fail effect
  fail
, Fail(..)
) where

import Algebra
import Prelude hiding (fail)

fail :: Has Fail m => String -> m a
fail = send . Fail

newtype Fail (m :: * -> *) k = Fail String
