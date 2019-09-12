{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, KindSignatures #-}
module Control.Effect.Empty
( -- * Empty effect
  Empty(..)
, empty
, guard
) where

import Control.Algebra.Class
import Data.Bool (bool)
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
empty :: (Algebra sig m, Member Empty sig) => m a
empty = send Empty

guard :: (Algebra sig m, Member Empty sig) => Bool -> m ()
guard = bool empty (pure ())
