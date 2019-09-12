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

import Control.Algebra.Class
import Control.Algebra.Reader
import Control.Monad.Trans.Class

-- | 'runInterpose' takes a handler for a given effect (such as 'State' or 'Reader')
-- and runs that handler whenever an effect of that type is encountered. Within a
-- handler you can use all the capabilities of the underlying monad stack, including
-- the intercepted effect, and you can pass the effect on to the original handler
-- using 'send'.
--
--   prop> run . evalState @Int a . runInterpose @(State Int) (\op -> modify @Int (+b) *> send op) $ modify @Int (+b) === a + b + b
--
runInterpose :: (forall x . alg m x -> m x) -> InterposeC alg m a -> m a
runInterpose handler = runReader (Handler handler) . runInterposeC

newtype InterposeC alg m a = InterposeC { runInterposeC :: ReaderC (Handler alg m) m a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans (InterposeC alg) where
  lift = InterposeC . lift

newtype Handler alg m = Handler (forall x . alg m x -> m x)

runHandler :: (HFunctor alg, Functor m) => Handler alg m -> alg (ReaderC (Handler alg m) m) a -> m a
runHandler h@(Handler handler) = handler . hmap (runReader h)

instance (HFunctor alg, Algebra sig m, Member alg sig) => Algebra sig (InterposeC alg m) where
  alg (op :: sig (InterposeC alg m) a)
    | Just (op' :: alg (InterposeC alg m) a) <- prj op = do
      handler <- InterposeC ask
      lift (runHandler handler (handleCoercible op'))
    | otherwise = InterposeC (ReaderC (\ handler -> alg (hmap (runReader handler . runInterposeC) op)))

-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Effect.State
