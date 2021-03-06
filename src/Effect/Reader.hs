module Effect.Reader
( -- * Reader effect
  Reader(..)
, ask
, asks
, local
  -- * Re-exports
, Algebra
, Has
, run
) where

import Algebra
import Effect.Reader.Internal (Reader(..))

ask :: Has (Reader r) m => m r
ask = send Ask
{-# INLINE ask #-}

asks :: Has (Reader r) m => (r -> a) -> m a
asks = (`fmap` ask)
{-# INLINE asks #-}

local :: Has (Reader r) m => (r -> r) -> m a -> m a
local f m = send (Local f m)
{-# INLINE local #-}
