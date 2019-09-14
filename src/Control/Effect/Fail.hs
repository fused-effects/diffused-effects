{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}
module Control.Effect.Fail
( -- * Fail effect
  Fail(..)
) where

import Control.Algebra.Class
import GHC.Generics (Generic1)

newtype Fail (m :: * -> *) k = Fail String
  deriving (Functor, Generic1)

instance HFunctor Fail
instance Effect   Fail
