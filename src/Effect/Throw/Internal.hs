{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
module Effect.Throw.Internal
( Throw(..)
) where

import Effect.Class
import GHC.Generics (Generic1)

newtype Throw e (m :: * -> *) k = Throw e
  deriving (Functor, Generic1)

instance HFunctor (Throw e)
instance Effect   (Throw e)
