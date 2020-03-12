{-# LANGUAGE GADTs #-}
module Effect.Writer.Internal
( Writer(..)
) where

data Writer w m k where
  Tell   :: w               -> Writer w m ()
  Listen :: m a             -> Writer w m (w, a)
  Censor :: (w -> w) -> m a -> Writer w m a
