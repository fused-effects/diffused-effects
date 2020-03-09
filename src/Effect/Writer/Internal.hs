{-# LANGUAGE ExistentialQuantification #-}
module Effect.Writer.Internal
( Writer(..)
) where

data Writer w m k
  = Tell w (m k)
  | forall a . Listen (m a) (w -> a -> m k)
  | forall a . Censor (w -> w) (m a) (a -> m k)
