{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.Effect.State.Internal
( State(..)
) where

import Control.Effect.Class
import GHC.Generics

data State s m k
  = Get (s -> m k)
  | Put s (m k)
  deriving (Functor, Generic1)

instance HFunctor (State s)
instance Effect   (State s)
