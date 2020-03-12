{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Effect.Choose.Internal
( Choose(..)
) where

import Data.Kind (Type)

data Choose (m :: Type -> Type) k where
  Choose :: Choose m Bool
