{-# LANGUAGE FlexibleContexts #-}
module Effect.Empty
( -- * Empty effect
  Empty(..)
, empty
, guard
  -- * Re-exports
, Algebra
, Has
, run
) where

import Algebra
import Effect.Empty.Internal (Empty(..))

empty :: Has Empty m => m a
empty = send Empty

guard :: Has Empty m => Bool -> m ()
guard True  = pure ()
guard False = empty
