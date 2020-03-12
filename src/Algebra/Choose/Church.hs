{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Choose.Church
( -- * Choose carrier
  runChoose
, runChooseS
, ChooseT(..)
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

runChoose :: (m b -> m b -> m b) -> (a -> m b) -> ChooseT m a -> m b
runChoose fork leaf (ChooseT runChooseT) = runChooseT fork leaf

runChooseS :: (S.Semigroup b, Applicative m) => (a -> m b) -> ChooseT m a -> m b
runChooseS = runChoose (liftA2 (S.<>))

newtype ChooseT m a = ChooseT { runChooseT :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b }
  deriving (Functor)

instance Applicative (ChooseT m) where
  pure a = ChooseT (\ _ leaf -> leaf a)
  {-# INLINE pure #-}
  ChooseT f <*> ChooseT a = ChooseT $ \ fork leaf ->
    f fork (\ f' -> a fork (leaf . f'))
  {-# INLINE (<*>) #-}

instance Monad (ChooseT m) where
  ChooseT a >>= f = ChooseT $ \ fork leaf ->
    a fork (runChoose fork leaf . f)
  {-# INLINE (>>=) #-}

-- | Separate fixpoints are computed for each branch.
instance MonadFix m => MonadFix (ChooseT m) where
  mfix f = ChooseT $ \ fork leaf ->
    mfix (runChooseS (pure . pure) . f . head)
    >>= \case
      a:|[] -> leaf a
      a:|_  -> leaf a `fork` runChoose fork leaf (mfix (liftAll . fmap tail . runChooseS (pure . pure) . f))
      where
    liftAll m = ChooseT $ \ fork leaf -> m >>= foldr1 fork . fmap leaf
  {-# INLINE mfix #-}

instance MonadTrans ChooseT where
  lift m = ChooseT (\ _ leaf -> m >>= leaf)
  {-# INLINE lift #-}

instance Algebra m => Algebra (ChooseT m) where
  type Sig (ChooseT m) = Choose :+: Sig m

  alg (hdl :: forall x . ctx (n x) -> ChooseT m (ctx x)) (ctx :: ctx ()) = \case
    L Choose -> ChooseT $ \ fork leaf -> fork (leaf (True <$ ctx)) (leaf (False <$ ctx))
    R other  -> ChooseT $ \ fork leaf -> thread dst (pure ctx) other >>= runIdentity . runChoose (coerce fork) (coerce leaf)
    where
    dst :: ChooseT Identity (ctx (n a)) -> m (ChooseT Identity (ctx a))
    dst = runIdentity . runChoose (liftA2 (liftA2 (<|>))) (pure . runChoose (liftA2 (<|>)) (pure . pure) . hdl)
  {-# INLINE alg #-}
