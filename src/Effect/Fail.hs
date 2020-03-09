{-# LANGUAGE DeriveFunctor, DeriveGeneric, KindSignatures #-}
module Effect.Fail
( -- * Fail effect
  fail
, Fail(..)
) where

import Algebra
import GHC.Generics (Generic1)
import Prelude hiding (fail)

fail :: Has Fail m => String -> m a
fail = send . Fail

newtype Fail (m :: * -> *) k = Fail String
  deriving (Functor, Generic1)

instance HFunctor Fail
instance Effect   Fail
