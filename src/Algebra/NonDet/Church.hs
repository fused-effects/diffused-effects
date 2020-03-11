{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.NonDet.Church
( -- * NonDet effects
  module Effect.NonDet
  -- * NonDet carrier
, runNonDet
, runNonDetA
, NonDetT(..)
) where

import           Algebra
import           Control.Applicative (liftA2)
import qualified Control.Applicative as Alt
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Data.Coerce
import           Data.Functor.Identity
import           Effect.NonDet

runNonDet :: (m b -> m b -> m b) -> (a -> m b) -> m b -> NonDetT m a -> m b
runNonDet fork leaf nil (NonDetT m) = m fork leaf nil

runNonDetA :: (Applicative m, Alt.Alternative f) => NonDetT m a -> m (f a)
runNonDetA = runNonDet (liftA2 (Alt.<|>)) (pure . pure) (pure Alt.empty)

runNonDetND :: (Applicative m, Has NonDet f) => NonDetT m a -> m (f a)
runNonDetND = runNonDet (liftA2 (<|>)) (pure . pure) (pure empty)

newtype NonDetT m a = NonDetT { runNonDetT :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b -> m b }
  deriving (Functor)

instance Applicative (NonDetT m) where
  pure a = NonDetT (\ _ leaf _ -> leaf a)
  {-# INLINE pure #-}
  NonDetT f <*> NonDetT a = NonDetT $ \ fork leaf nil ->
    f fork (\ f' -> a fork (leaf . f') nil) nil
  {-# INLINE (<*>) #-}

instance Monad (NonDetT m) where
  NonDetT a >>= f = NonDetT $ \ fork leaf nil ->
    a fork (\ a' -> runNonDetT (f a') fork leaf nil) nil
  {-# INLINE (>>=) #-}

instance MonadFix m => MonadFix (NonDetT m) where
  mfix f = NonDetT $ \ fork leaf nil ->
    mfix (runNonDetA . f . head)
    >>= runNonDet fork leaf nil . foldr
      (\ a _ -> NonDetT (\ fork leaf nil -> fork (leaf a) (runNonDet fork leaf nil (mfix (liftAll . fmap tail . runNonDetA . f)))))
      (NonDetT (\ _ _ nil -> nil))
    where
    liftAll m = NonDetT $ \ fork leaf nil -> m >>= foldr (fork . leaf) nil
  {-# INLINE mfix #-}

instance MonadTrans NonDetT where
  lift m = NonDetT (\ _ leaf _ -> m >>= leaf)
  {-# INLINE lift #-}

instance Algebra m => Algebra (NonDetT m) where
  type Sig (NonDetT m) = NonDet :+: Sig m

  alg (hdl :: forall x . ctx (n x) -> NonDetT m (ctx x)) (ctx :: ctx ()) = \case
    L (L Empty)      -> NonDetT $ \ _ _ nil -> nil
    L (R (Choose k)) -> NonDetT $ \ fork leaf nil -> fork (runNonDet fork leaf nil (hdl (k True <$ ctx))) (runNonDet fork leaf nil (hdl (k False <$ ctx)))
    R other          -> NonDetT $ \ fork leaf nil -> thread dst (pure ctx) other >>= runIdentity . runNonDet (coerce fork) (coerce leaf) (coerce nil)
    where
    dst :: NonDetT Identity (ctx (n a)) -> m (NonDetT Identity (ctx a))
    dst = runIdentity . runNonDet (liftA2 (liftA2 (<|>))) (Identity . runNonDetND . hdl) (pure (pure empty))
  {-# INLINE alg #-}
