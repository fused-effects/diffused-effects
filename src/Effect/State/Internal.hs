{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Effect.State.Internal
( State(..)
) where

import Data.Kind (Type)

data State s (m :: Type -> Type) k where
  Get ::      State s m s
  Put :: s -> State s m ()
