module Effect.Choose.Internal
( Choose(..)
) where

newtype Choose m k = Choose (Bool -> m k)
