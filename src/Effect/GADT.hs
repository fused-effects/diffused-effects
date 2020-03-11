{-# LANGUAGE GADTs #-}
module Effect.GADT
( Error(..)
) where

data Error e m k where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a
