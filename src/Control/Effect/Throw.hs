{-# LANGUAGE FlexibleContexts #-}
module Control.Effect.Throw
( -- * Throw effect
  Throw(..)
, throwError
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Throw.Internal (Throw(..))

throwError :: Has (Throw e) m => e -> m a
throwError = send . Throw
