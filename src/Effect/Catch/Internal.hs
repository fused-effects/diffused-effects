{-# LANGUAGE GADTs #-}
module Effect.Catch.Internal
( Catch(..)
) where

data Catch e m k where
  Catch :: m a -> (e -> m a) -> Catch e m a
