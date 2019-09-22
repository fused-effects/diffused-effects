{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Control.Algebra.Lift
( -- * Lift effect
  module Control.Effect.Lift
  -- * Lift carrier
, runM
, LiftC(..)
  -- * Re-exports
, Has
, run
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Effect.Lift
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
runM :: LiftC m a -> m a
runM = runLiftC

newtype LiftC m a = LiftC { runLiftC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans LiftC where
  lift = LiftC

instance Monad m => Algebra (LiftC m) where
  type Signature (LiftC m) = Lift m
  alg = LiftC . (>>= runLiftC) . unLift

instance MonadUnliftIO m => MonadUnliftIO (LiftC m) where
  askUnliftIO = LiftC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runLiftC))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = LiftC $ withRunInIO $ \run -> inner (run . runLiftC)
  {-# INLINE withRunInIO #-}
