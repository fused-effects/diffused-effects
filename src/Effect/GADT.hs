{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Effect.GADT
( Choose(..)
, Empty(..)
, Error(..)
) where

import Data.Kind (Type)

data Choose (m :: Type -> Type) k where
  Choose :: Choose m Bool

data Empty (m :: Type -> Type) k where
  Empty :: Empty m a

data Error e m k where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a
