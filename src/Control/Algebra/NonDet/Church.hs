{-# LANGUAGE DeriveTraversable, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Algebra.NonDet.Church
( -- * Choose effect
  module Control.Effect.Choose
  -- * Empty effect
, module Control.Effect.Empty
  -- * NonDet Algebra
, runNonDet
, NonDetC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Algebra.Class
import Control.Effect.Choose
import Control.Effect.Empty
import Data.Bool (bool)
import Control.Monad (MonadPlus(..), join)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Maybe (fromJust)

-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
--   Using @[]@ as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first. However, unlike 'Control.Effect.Cull.runNonDetOnce', this will still enumerate the entire search space before returning, meaning that it will diverge for infinite search spaces, even when using 'Maybe'.
--
--   prop> run (runNonDet (pure a)) === [a]
--   prop> run (runNonDet (pure a)) === Just a
runNonDet :: (m b -> m b -> m b) -> (a -> m b) -> m b -> NonDetC m a -> m b
runNonDet fork leaf nil (NonDetC m) = m fork leaf nil

-- | A carrier for 'NonDet' effects based on Ralf Hinze’s design described in [Deriving Backtracking Monad Transformers](https://www.cs.ox.ac.uk/ralf.hinze/publications/#P12).
newtype NonDetC m a = NonDetC
  { -- | A higher-order function receiving three continuations, respectively implementing '<|>', 'pure', and 'empty'.
    runNonDetC :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b -> m b
  }
  deriving (Functor)

-- $
--   prop> run (runNonDet (pure a *> pure b)) === Just b
--   prop> run (runNonDet (pure a <* pure b)) === Just a
instance Applicative (NonDetC m) where
  pure a = NonDetC (\ _ leaf _ -> leaf a)
  {-# INLINE pure #-}
  NonDetC f <*> NonDetC a = NonDetC $ \ fork leaf nil ->
    f fork (\ f' -> a fork (leaf . f') nil) nil
  {-# INLINE (<*>) #-}

-- $
--   prop> run (runNonDet (pure a <|> (pure b <|> pure c))) === Fork (Leaf a) (Fork (Leaf b) (Leaf c))
--   prop> run (runNonDet ((pure a <|> pure b) <|> pure c)) === Fork (Fork (Leaf a) (Leaf b)) (Leaf c)
instance (Algebra sig m, Effect sig) => Alternative (NonDetC m) where
  empty = send Empty
  {-# INLINE empty #-}
  l <|> r = send (Choose (bool r l))
  {-# INLINE (<|>) #-}

instance Monad (NonDetC m) where
  NonDetC a >>= f = NonDetC $ \ fork leaf nil ->
    a fork (\ a' -> runNonDetC (f a') fork leaf nil) nil
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (NonDetC m) where
  fail s = lift (Fail.fail s)
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (NonDetC m) where
  mfix f = NonDetC $ \ fork leaf nil ->
    mfix (\ a -> runNonDetC (f (fromJust (fold (<|>) Just Nothing a)))
      (liftA2 Fork)
      (pure . Leaf)
      (pure Nil))
    >>= fold fork leaf nil
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (NonDetC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance (Algebra sig m, Effect sig) => MonadPlus (NonDetC m)

instance MonadTrans NonDetC where
  lift m = NonDetC (\ _ leaf _ -> m >>= leaf)
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (Empty :+: Choose :+: sig) (NonDetC m) where
  alg (L Empty)          = NonDetC (\ _ _ nil -> nil)
  alg (R (L (Choose k))) = NonDetC $ \ fork leaf nil -> fork (runNonDetC (k True) fork leaf nil) (runNonDetC (k False) fork leaf nil)
  alg (R (R other))      = NonDetC $ \ fork leaf nil -> alg (handle (Leaf ()) (fmap join . traverse (runNonDet (liftA2 Fork) (pure . Leaf) (pure Nil))) other) >>= fold fork leaf nil
  {-# INLINE alg #-}


data BinaryTree a = Nil | Leaf a | Fork (BinaryTree a) (BinaryTree a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative BinaryTree where
  pure = Leaf
  {-# INLINE pure #-}
  f <*> a = fold Fork (<$> a) Nil f
  {-# INLINE (<*>) #-}

instance Alternative BinaryTree where
  empty = Nil
  {-# INLINE empty #-}
  (<|>) = Fork
  {-# INLINE (<|>) #-}

instance Monad BinaryTree where
  a >>= f = fold Fork f Nil a
  {-# INLINE (>>=) #-}


fold :: (b -> b -> b) -> (a -> b) -> b -> BinaryTree a -> b
fold fork leaf nil = go where
  go Nil        = nil
  go (Leaf a)   = leaf a
  go (Fork a b) = fork (go a) (go b)
{-# INLINE fold #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Data.Foldable (asum)
