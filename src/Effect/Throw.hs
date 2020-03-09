module Effect.Throw
( -- * Throw effect
  Throw(..)
, throwError
  -- * Re-exports
, Algebra
, Has
, run
) where

import Algebra
import Effect.Throw.Internal (Throw(..))

throwError :: Has (Throw e) m => e -> m a
throwError = send . Throw
