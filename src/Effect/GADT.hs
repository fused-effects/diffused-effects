{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Effect.GADT
( Catch(..)
, Choose(..)
, Empty(..)
, Error
, NonDet
, Throw(..)
) where

import Data.Kind (Type)
import Effect.Sum

data Catch e m k where
  Catch :: m a -> (e -> m a) -> Catch e m a

data Choose (m :: Type -> Type) k where
  Choose :: Choose m Bool

data Empty (m :: Type -> Type) k where
  Empty :: Empty m a

type Error e = Throw e :+: Catch e

type NonDet = Empty :+: Choose

data Throw e (m :: Type -> Type) k where
  Throw :: e -> Throw e m a
