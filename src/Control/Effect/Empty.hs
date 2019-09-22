{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, KindSignatures, TypeOperators #-}
module Control.Effect.Empty
( -- * Empty effect
  Empty(..)
, empty
) where

import Control.Algebra
import GHC.Generics (Generic1)

-- | An effect modelling nondeterminism without choice.
--
--   This can be seen as similar to 'Control.Effect.Fail', but without an error message.
data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance HFunctor Empty
instance Effect   Empty

-- | Abort the computation.
--
--   prop> run (runEmpty empty) === Nothing
empty :: Has Empty m => m a
empty = send Empty
