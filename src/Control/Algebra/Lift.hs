{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Control.Algebra.Lift
( -- * Lift effect
  module Control.Effect.Lift
  -- * Lift Algebra
, runM
, LiftC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra.Class
import Control.Effect.Lift
import Control.Monad.Trans.Class

-- | Extract a 'Lift'ed 'Monad'ic action from an effectful computation.
runM :: LiftC m a -> m a
runM = runLiftC

newtype LiftC m a = LiftC { runLiftC :: m a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans LiftC where
  lift = LiftC

instance Monad m => Algebra (Lift m) (LiftC m) where
  alg = LiftC . (>>= runLiftC) . unLift
