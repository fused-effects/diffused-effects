{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Effect.GADT
( Choose(..)
, Empty(..)
, Error(..)
, NonDet
) where

import Data.Kind (Type)
import Effect.Sum

data Choose (m :: Type -> Type) k where
  Choose :: Choose m Bool

data Empty (m :: Type -> Type) k where
  Empty :: Empty m a

data Error e m k where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a

type NonDet = Empty :+: Choose
