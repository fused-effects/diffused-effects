{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Choose.Church
( -- * Choose carrier
  runChoose
, runChooseS
, ChooseC(..)
  -- * Choose effect
, module Effect.Choose
) where

import           Algebra
import           Control.Applicative (liftA2)
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty(..), head, tail)
import qualified Data.Semigroup as S
import           Effect.Choose
import           Prelude hiding (head, tail)

runChoose :: (m b -> m b -> m b) -> (a -> m b) -> ChooseC m a -> m b
runChoose fork leaf (ChooseC runChooseC) = runChooseC fork leaf

runChooseS :: (S.Semigroup b, Applicative m) => (a -> m b) -> ChooseC m a -> m b
runChooseS = runChoose (liftA2 (S.<>))

newtype ChooseC m a = ChooseC (forall b . (m b -> m b -> m b) -> (a -> m b) -> m b)
  deriving (Functor)

instance Applicative (ChooseC m) where
  pure a = ChooseC (\ _ leaf -> leaf a)
  {-# INLINE pure #-}
  ChooseC f <*> ChooseC a = ChooseC $ \ fork leaf ->
    f fork (\ f' -> a fork (leaf . f'))
  {-# INLINE (<*>) #-}

instance Monad (ChooseC m) where
  ChooseC a >>= f = ChooseC $ \ fork leaf ->
    a fork (runChoose fork leaf . f)
  {-# INLINE (>>=) #-}

-- | Separate fixpoints are computed for each branch.
instance MonadFix m => MonadFix (ChooseC m) where
  mfix f = ChooseC $ \ fork leaf ->
    mfix (runChooseS (pure . pure) . f . head)
    >>= \case
      a:|[] -> leaf a
      a:|_  -> leaf a `fork` runChoose fork leaf (mfix (liftAll . fmap tail . runChooseS (pure . pure) . f))
      where
    liftAll m = ChooseC $ \ fork leaf -> m >>= foldr1 fork . fmap leaf
  {-# INLINE mfix #-}

instance MonadTrans ChooseC where
  lift m = ChooseC (\ _ leaf -> m >>= leaf)
  {-# INLINE lift #-}

instance (Algebra m, Effect (Sig m)) => Algebra (ChooseC m) where
  type Sig (ChooseC m) = Choose :+: Sig m

  alg = \case
    L (Choose k) -> ChooseC $ \ fork leaf -> fork (runChoose fork leaf (k True)) (runChoose fork leaf (k False))
    R other      -> ChooseC $ \ fork leaf -> alg (handle (pure ()) dst other) >>= runIdentity . runChoose (coerce fork) (coerce leaf)
    where
    dst :: Applicative m => ChooseC Identity (ChooseC m a) -> m (ChooseC Identity a)
    dst = runIdentity . runChoose (liftA2 (liftA2 (<|>))) (pure . runChoose (liftA2 (<|>)) (pure . pure))
  {-# INLINE alg #-}
