module Effect.Catch
( -- * Catch effect
  Catch(..)
, catchError
  -- * Re-exports
, Algebra
, Has
, run
) where

import Algebra
import Effect.Catch.Internal (Catch(..))

catchError :: Has (Catch e) m => m a -> (e -> m a) -> m a
catchError m h = send (Catch m h pure)
