{-# LANGUAGE DeriveFunctor, FlexibleInstances, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Cut.Church
( -- * Cut effect
  module Control.Effect.Cut
  -- * Cut carrier
, runCut
, runCutAll
, CutC(..)
-- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Effect.Choose
import Control.Effect.Cut
import Control.Effect.Empty
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

-- | Run a 'Cut' effect within an underlying 'Alternative' instance (typically another 'Algebra' for 'Choose' & 'Empty' effects).
--
--   prop> run (runNonDetOnce (runCut (pure a))) === Just a
runCut :: Alternative m => CutC m a -> m a
runCut m = runCutC m ((<|>) . pure) empty empty

-- | Run a 'Cut' effect, returning all its results in an 'Alternative' collection.
runCutAll :: (Alternative f, Applicative m) => CutC m a -> m (f a)
runCutAll (CutC m) = m (fmap . (<|>) . pure) (pure empty) (pure empty)

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

instance Alternative (CutC m) where
  empty = CutC (\ _ nil _ -> nil)
  {-# INLINE empty #-}
  CutC l <|> CutC r = CutC (\ cons nil fail -> l cons (r cons nil fail) fail)
  {-# INLINE (<|>) #-}

instance Monad (CutC m) where
  CutC a >>= f = CutC $ \ cons nil fail ->
    a (\ a' as -> runCutC (f a') cons as fail) nil fail
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (CutC m) where
  fail s = lift (Fail.fail s)
  {-# INLINE fail #-}

instance MonadFix m => MonadFix (CutC m) where
  mfix f = CutC (\ cons nil _ -> mfix (\ a -> runCutC (f (head a)) (fmap . (:)) (pure []) (pure [])) >>= foldr cons nil)
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (CutC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance MonadPlus (CutC m)

instance MonadTrans CutC where
  lift m = CutC (\ cons nil _ -> m >>= flip cons nil)
  {-# INLINE lift #-}

instance (Algebra m, Effect (Signature m)) => Algebra (CutC m) where
  type Signature (CutC m) = Cut :+: Empty :+: Choose :+: Signature m
  alg (L Cutfail)    = CutC $ \ _    _   fail -> fail
  alg (L (Call m k)) = CutC $ \ cons nil fail -> runCutC m (\ a as -> runCutC (k a) cons as fail) nil nil
  alg (R (L Empty))          = empty
  alg (R (R (L (Choose k)))) = k True <|> k False
  alg (R (R (R other)))      = CutC $ \ cons nil _ -> alg (handle [()] (fmap concat . traverse runCutAll) other) >>= foldr cons nil
  {-# INLINE alg #-}
