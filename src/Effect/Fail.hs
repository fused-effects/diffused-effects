{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}
module Effect.Fail
( -- * Fail effect
  Fail(..)
) where

import Effect.Class
import GHC.Generics (Generic1)

newtype Fail (m :: * -> *) k = Fail String
  deriving (Functor, Generic1)

instance HFunctor Fail
instance Effect   Fail
