{-# LANGUAGE FlexibleContexts #-}
module Control.Effect.Writer
( -- * Writer effect
  Writer(..)
, tell
, listen
, listens
, censor
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Effect.Writer.Internal (Writer(..))

tell :: Has (Writer w) m => w -> m ()
tell w = send (Tell w (pure ()))
{-# INLINE tell #-}

listen :: Has (Writer w) m => m a -> m (w, a)
listen m = send (Listen m (curry pure))
{-# INLINE listen #-}

listens :: Has (Writer w) m => (w -> b) -> m a -> m (b, a)
listens f m = send (Listen m (curry pure . f))
{-# INLINE listens #-}

censor :: Has (Writer w) m => (w -> w) -> m a -> m a
censor f m = send (Censor f m pure)
{-# INLINE censor #-}
