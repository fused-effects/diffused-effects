{-# LANGUAGE DeriveFunctor, ExplicitForAll, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}

{- |
This module provides an 'InterposeC' Algebra capable of "eavesdropping" on requests
made to other Algebras. This is a useful capability for dynamism in deeply-nested
effect stacks, but can lead to complicated control flow. Be careful.
-}
module Control.Algebra.Interpose
( InterposeC (..)
, runInterpose
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Applicative
import Control.Algebra.Class
import Control.Algebra.Reader
import Control.Monad (MonadPlus (..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | 'runInterpose' takes a handler for a given effect (such as 'State' or 'Reader')
-- and runs that handler whenever an effect of that type is encountered. Within a
-- handler you can use all the capabilities of the underlying monad stack, including
-- the intercepted effect, and you can pass the effect on to the original handler
-- using 'send'.
--
--   prop> run . evalState @Int a . runInterpose @(State Int) (\op -> modify @Int (+b) *> send op) $ modify @Int (+b) === a + b + b
--
runInterpose :: (forall x . eff m x -> m x) -> InterposeC eff m a -> m a
runInterpose handler = runReader (Handler handler) . runInterposeC

newtype InterposeC eff m a = InterposeC { runInterposeC :: ReaderC (Handler eff m) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans (InterposeC eff) where
  lift = InterposeC . lift

newtype Handler eff m = Handler (forall x . eff m x -> m x)

runHandler :: (HFunctor eff, Functor m) => Handler eff m -> eff (ReaderC (Handler eff m) m) a -> m a
runHandler h@(Handler handler) = handler . hmap (runReader h)

instance (HFunctor eff, Algebra sig m, Member eff sig) => Algebra sig (InterposeC eff m) where
  eff (op :: sig (InterposeC eff m) a)
    | Just (op' :: eff (InterposeC eff m) a) <- prj op = do
      handler <- InterposeC ask
      lift (runHandler handler (handleCoercible op'))
    | otherwise = InterposeC (ReaderC (\ handler -> eff (hmap (runReader handler . runInterposeC) op)))

-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Effect.State
