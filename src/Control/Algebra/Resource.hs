{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Resource
( -- * Resource effect
  module Control.Effect.Resource
  -- * Resource carrier
, runResource
, ResourceC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import           Control.Algebra
import           Control.Algebra.Reader
import           Control.Applicative (Alternative(..))
import           Control.Effect.Resource
import qualified Control.Exception as Exc
import           Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class

-- Not exposed due to its potential to silently drop effects (#180).
unliftResource :: (forall x . m x -> IO x) -- ^ "unlifting" function to run the algebra in 'IO'
            -> ResourceC m a
            -> m a
unliftResource handler = runReader (UnliftIO handler) . runResourceC

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
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadUnliftIO m => MonadUnliftIO (ResourceC m) where
  askUnliftIO = ResourceC . ReaderC $ \(UnliftIO h) ->
    withUnliftIO $ \u -> pure (UnliftIO $ \r -> unliftIO u (unliftResource h r))

instance MonadTrans ResourceC where
  lift = ResourceC . lift

runUnlifting :: UnliftIO m -> ResourceC m a -> IO a
runUnlifting h@(UnliftIO handler) = handler . runReader h . runResourceC

instance (Algebra sig m, MonadIO m) => Algebra (Resource :+: sig) (ResourceC m) where
  alg (L (Resource acquire release use k)) = do
    handler <- ResourceC ask
    a <- liftIO (Exc.bracket
      (runUnlifting handler acquire)
      (runUnlifting handler . release)
      (runUnlifting handler . use))
    k a
  alg (L (OnError  acquire release use k)) = do
    handler <- ResourceC ask
    a <- liftIO (Exc.bracketOnError
      (runUnlifting handler acquire)
      (runUnlifting handler . release)
      (runUnlifting handler . use))
    k a
  alg (R other) = ResourceC (alg (R (handleCoercible other)))
