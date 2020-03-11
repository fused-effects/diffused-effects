{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Effect.GADT
( Empty(..)
, Error(..)
) where

import Data.Kind (Type)

data Empty (m :: Type -> Type) k where
  Empty :: Empty m a

data Error e m k where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a
