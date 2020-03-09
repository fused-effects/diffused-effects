{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.NonDet.Church
( -- * NonDet effects
  module Effect.NonDet
  -- * NonDet carrier
, runNonDet
, runNonDetA
, NonDetC(..)
) where

import           Algebra
import           Control.Applicative (liftA2)
import qualified Control.Applicative as Alt
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Data.Coerce
import           Data.Functor.Identity
import           Effect.NonDet

runNonDet :: (m b -> m b -> m b) -> (a -> m b) -> m b -> NonDetC m a -> m b
runNonDet fork leaf nil (NonDetC m) = m fork leaf nil

runNonDetA :: (Applicative m, Alt.Alternative f) => NonDetC m a -> m (f a)
runNonDetA = runNonDet (liftA2 (Alt.<|>)) (pure . pure) (pure Alt.empty)

runNonDetND :: (Applicative m, Has NonDet f) => NonDetC m a -> m (f a)
runNonDetND = runNonDet (liftA2 (<|>)) (pure . pure) (pure empty)

newtype NonDetC m a = NonDetC { runNonDetC :: forall b . (m b -> m b -> m b) -> (a -> m b) -> m b -> m b }
  deriving (Functor)

instance Applicative (NonDetC m) where
  pure a = NonDetC (\ _ leaf _ -> leaf a)
  {-# INLINE pure #-}
  NonDetC f <*> NonDetC a = NonDetC $ \ fork leaf nil ->
    f fork (\ f' -> a fork (leaf . f') nil) nil
  {-# INLINE (<*>) #-}

instance Monad (NonDetC m) where
  NonDetC a >>= f = NonDetC $ \ fork leaf nil ->
    a fork (\ a' -> runNonDetC (f a') fork leaf nil) nil
  {-# INLINE (>>=) #-}

instance (Algebra m, Effect (Sig m), MonadFix m) => MonadFix (NonDetC m) where
  mfix f = NonDetC $ \ fork leaf nil ->
    mfix (runNonDetA . f . head)
    >>= runNonDet fork leaf nil . foldr
      (\ a _ -> pure a <|> mfix (liftAll . fmap tail . runNonDetA . f))
      empty
    where
    liftAll m = NonDetC $ \ fork leaf nil -> m >>= foldr (fork . leaf) nil
  {-# INLINE mfix #-}

instance MonadTrans NonDetC where
  lift m = NonDetC (\ _ leaf _ -> m >>= leaf)
  {-# INLINE lift #-}

instance (Algebra m, Effect (Sig m)) => Algebra (NonDetC m) where
  type Sig (NonDetC m) = NonDet :+: Sig m

  alg = \case
    L (L Empty)      -> empty
    L (R (Choose k)) -> k True <|> k False
    R other          -> NonDetC $ \ fork leaf nil -> alg (handle (pure ()) dst other) >>= runIdentity . runNonDet (coerce fork) (coerce leaf) (coerce nil)
    where
    dst :: Applicative m => NonDetC Identity (NonDetC m a) -> m (NonDetC Identity a)
    dst = runIdentity . runNonDet (liftA2 (liftA2 (<|>))) (Identity . runNonDetND) (pure (pure empty))
  {-# INLINE alg #-}
