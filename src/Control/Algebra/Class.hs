{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Algebra.Class
( Algebra(..)
) where

import Control.Effect.Catch.Internal
import Control.Effect.Choose.Internal
import Control.Effect.Class
import Control.Effect.Empty.Internal
import Control.Effect.Error.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Reader.Internal
import Control.Effect.Sum
import Control.Effect.Throw.Internal
import Control.Effect.Writer.Internal
import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty)

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

instance Algebra NonEmpty where
  type Signature NonEmpty = Choose

  alg (Choose m) = m True <> m False

instance Algebra [] where
  type Signature [] = NonDet

  alg = \case
    L Empty      -> []
    R (Choose m) -> m True <> m False

instance Monoid w => Algebra ((,) w) where
  type Signature ((,) w) = Writer w

  alg = \case
    Tell w     k -> join (w, k)
    Listen m   k -> let (w, a) = m ; (w', a') = k w a in (mappend w w', a')
    Censor f m k -> let (w, a) = m ; (w', a') = k   a in (mappend (f w) w', a')
