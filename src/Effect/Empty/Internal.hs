{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
module Effect.Empty.Internal
( Empty(..)
) where

import Data.Kind (Type)

data Empty (m :: Type -> Type) k where
  Empty :: Empty m a
