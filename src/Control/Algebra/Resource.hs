{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Resource
( -- * Resource effect
  module Control.Effect.Resource
  -- * Resource Algebra
, runResource
, ResourceC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import           Control.Algebra.Class
import           Control.Algebra.Reader
import           Control.Effect.Lift
import           Control.Effect.Resource
import qualified Control.Exception as Exc
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class

-- | Executes a 'Resource' effect. Because this runs using 'MonadUnliftIO',
-- invocations of 'runResource' must happen at the "bottom" of a stack of
-- effect invocations, i.e. before the use of any monads that lack such
-- instances, such as 'StateC':
--
-- @
--   runM
--   . runResource
--   . runState @Int 1
--   $ myComputation
-- @
runResource :: MonadUnliftIO m
            => ResourceC m a
            -> m a
runResource r = withRunInIO (\f -> runUnlifting (UnliftIO f) r)

newtype ResourceC m a = ResourceC { runResourceC :: ReaderC (UnliftIO m) m a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans ResourceC where
  lift = ResourceC . lift

runUnlifting :: UnliftIO m -> ResourceC m a -> IO a
runUnlifting h@(UnliftIO handler) = handler . runReader h . runResourceC

instance (Algebra sig m, Member (Lift IO) sig) => Algebra (Resource :+: sig) (ResourceC m) where
  alg (L (Resource acquire release use k)) = do
    handler <- ResourceC ask
    a <- sendM (Exc.bracket
      (runUnlifting handler acquire)
      (runUnlifting handler . release)
      (runUnlifting handler . use))
    k a
  alg (L (OnError  acquire release use k)) = do
    handler <- ResourceC ask
    a <- sendM (Exc.bracketOnError
      (runUnlifting handler acquire)
      (runUnlifting handler . release)
      (runUnlifting handler . use))
    k a
  alg (R other) = ResourceC (alg (R (handleCoercible other)))
