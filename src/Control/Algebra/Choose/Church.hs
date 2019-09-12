{-# LANGUAGE DeriveTraversable, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Choose.Church
( -- * Choose effect
  module Control.Effect.Choose
  -- * Choose Algebra
, runChoose
, ChooseC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Applicative ((<|>), liftA2)
import Control.Algebra.Class
import Control.Effect.Choose
import Control.Monad (join)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Maybe (fromJust)
import Prelude hiding (fail)

runChoose :: (m b -> m b -> m b) -> (a -> m b) -> ChooseC m a -> m b
runChoose fork leaf m = runChooseC m fork leaf

-- | A Algebra for 'Choose' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
newtype ChooseC m a = ChooseC
  { -- | A higher-order function receiving two continuations, respectively implementing choice and 'pure'.
    runChooseC :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b
  }
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

instance Fail.MonadFail m => Fail.MonadFail (ChooseC m) where
  fail s = lift (Fail.fail s)
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (ChooseC m) where
  mfix f = ChooseC $ \ fork leaf ->
    mfix (runChoose (liftA2 Fork) (pure . Leaf)
      . f . fromJust . fold (<|>) Just)
    >>= fold fork leaf
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ChooseC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance MonadTrans ChooseC where
  lift m = ChooseC (\ _ leaf -> m >>= leaf)
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (Choose :+: sig) (ChooseC m) where
  alg (L (Choose k)) = ChooseC $ \ fork leaf -> fork (runChooseC (k True) fork leaf) (runChooseC (k False) fork leaf)
  alg (R other)      = ChooseC $ \ fork leaf -> alg (handle (Leaf ()) (fmap join . traverse (runChoose (liftA2 Fork) (pure . Leaf))) other) >>= fold fork leaf
  {-# INLINE alg #-}


data BinaryTree a = Leaf a | Fork (BinaryTree a) (BinaryTree a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative BinaryTree where
  pure = Leaf
  f <*> a = fold Fork (<$> a) f

instance Monad BinaryTree where
  a >>= f = fold Fork f a


fold :: (b -> b -> b) -> (a -> b) -> BinaryTree a -> b
fold fork leaf = go where
  go (Leaf a)   = leaf a
  go (Fork a b) = fork (go a) (go b)
