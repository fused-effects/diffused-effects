{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, KindSignatures #-}
module Control.Effect.Fail
( -- * Fail effect
  Fail(..)
, fail
) where

import Control.Algebra.Class
import GHC.Generics (Generic1)
import Prelude hiding (fail)

newtype Fail (m :: * -> *) k = Fail String
  deriving (Functor, Generic1)

instance HFunctor Fail
instance Effect   Fail

fail :: (Algebra sig m, Member Fail sig) => String -> m a
fail = send . Fail
