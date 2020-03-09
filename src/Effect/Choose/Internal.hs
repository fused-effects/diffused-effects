{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Effect.Choose.Internal
( Choose(..)
) where

import Effect.Class
import GHC.Generics (Generic1)

newtype Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance HFunctor Choose
instance Effect   Choose
