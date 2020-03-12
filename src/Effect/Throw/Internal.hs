{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
module Effect.Throw.Internal
( Throw(..)
) where

import Data.Kind (Type)

data Throw e (m :: Type -> Type) k where
  Throw :: e -> Throw e m a
