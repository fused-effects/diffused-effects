{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Algebra.Class
( Algebra(..)
) where

import Control.Effect.Catch.Internal
import Control.Effect.Class
import Control.Effect.Empty.Internal
import Control.Effect.Error.Internal
import Control.Effect.Reader.Internal
import Control.Effect.Sum
import Control.Effect.Throw.Internal
import Control.Monad (join)

class (HFunctor (Signature m), Monad m) => Algebra m where
  type Signature m :: (* -> *) -> (* -> *)

  alg :: Signature m m a -> m a

instance Algebra Maybe where
  type Signature Maybe = Empty

  alg Empty = Nothing

instance Algebra (Either e) where
  type Signature (Either e) = Error e

  alg = \case
    L (Throw e)     -> Left e
    R (Catch m h k) -> either h pure m >>= k

instance Algebra ((->) r) where
  type Signature ((->) r) = Reader r

  alg = \case
    Ask       k -> join k
    Local f m k -> m . f >>= k
