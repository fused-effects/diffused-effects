{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Effect.Writer
( -- * Writer effect
  Writer(..)
, tell
, writer
, listen
, listens
, censor
, pass
  -- * Re-exports
, Algebra
, Has
, run
) where

import Algebra
import Data.Bifunctor (first)
import Effect.Writer.Internal (Writer(..))

tell :: Has (Writer w) m => w -> m ()
tell = send . Tell
{-# INLINE tell #-}

writer :: Has (Writer w) m => (w, a) -> m a
writer ~(w, a) = a <$ tell w
{-# INLINE writer #-}

listen :: Has (Writer w) m => m a -> m (w, a)
listen m = send (Listen m)
{-# INLINE listen #-}

listens :: Has (Writer w) m => (w -> b) -> m a -> m (b, a)
listens f = fmap (first f) . listen
{-# INLINE listens #-}

censor :: Has (Writer w) m => (w -> w) -> m a -> m a
censor f m = send (Censor f m)
{-# INLINE censor #-}

pass :: forall w m a . (Monoid w, Has (Writer w) m) => m (w -> w, a) -> m a
pass m = do
  (w, (f, a)) <- censor @w (const mempty) (listen m)
  a <$ tell (f w)
{-# INLINE pass #-}
