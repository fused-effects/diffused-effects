{-# LANGUAGE ExistentialQuantification #-}
module Effect.Catch.Internal
( Catch(..)
) where

data Catch e m k
  = forall b . Catch (m b) (e -> m b) (b -> m k)
