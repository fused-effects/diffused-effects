{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Cut.Church
( -- * Cut effect
  module Control.Effect.Cut
  -- * Cut Algebra
, runCut
, CutC(..)
-- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra.Class
import Control.Effect.Choose
import Control.Effect.Cut
import Control.Effect.Empty
import Control.Monad.Trans.Class

-- | Run a 'Cut' effect within an underlying 'Alternative' instance (typically another 'Algebra' for 'Choose' & 'Empty' effects).
--
--   prop> run (runNonDetOnce (runCut (pure a))) === Just a
runCut :: (a -> m b -> m b) -> m b -> m b -> CutC m a -> m b
runCut cons nil cutfail (CutC m) = m cons nil cutfail

newtype CutC m a = CutC
  { -- | A higher-order function receiving three parameters: a function to combine each solution with the rest of the solutions, an action to run when no results are produced (e.g. on 'empty'), and an action to run when no results are produced and backtrcking should not be attempted (e.g. on 'cutfail').
    runCutC :: forall b . (a -> m b -> m b) -> m b -> m b -> m b
  }
  deriving (Functor)

instance Applicative (CutC m) where
  pure a = CutC (\ cons nil _ -> cons a nil)
  {-# INLINE pure #-}
  CutC f <*> CutC a = CutC $ \ cons nil fail ->
    f (\ f' fs -> a (cons . f') fs fail) nil fail
  {-# INLINE (<*>) #-}

instance Monad (CutC m) where
  CutC a >>= f = CutC $ \ cons nil fail ->
    a (\ a' as -> runCutC (f a') cons as fail) nil fail
  {-# INLINE (>>=) #-}

instance MonadTrans CutC where
  lift m = CutC (\ cons nil _ -> m >>= flip cons nil)
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (Cut :+: Empty :+: Choose :+: sig) (CutC m) where
  alg (L Cutfail)    = CutC $ \ _    _   fail -> fail
  alg (L (Call m k)) = CutC $ \ cons nil fail -> runCutC m (\ a as -> runCutC (k a) cons as fail) nil nil
  alg (R (L Empty))          = CutC (\ _ nil _ -> nil)
  alg (R (R (L (Choose k)))) = CutC (\ cons nil fail -> runCutC (k True) cons (runCutC (k False) cons nil fail) fail)
  alg (R (R (R other)))      = CutC $ \ cons nil _ -> alg (handle [()] (fmap concat . traverse (runCut (fmap . (:)) (pure []) (pure []))) other) >>= foldr cons nil
  {-# INLINE alg #-}
