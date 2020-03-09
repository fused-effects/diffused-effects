{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.Lift
( -- * Lift carrier
  runLift
, LiftT(..)
  -- * Lift effect
, module Effect.Lift
) where

import Algebra
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Effect.Lift

runLift :: LiftT m a -> m a
runLift (LiftT m) = m

newtype LiftT m a = LiftT { runLiftT :: m a }
  deriving (Applicative, Functor, Monad, MonadFix)

instance MonadTrans LiftT where
  lift = LiftT

instance Monad m => Algebra (LiftT m) where
  type Sig (LiftT m) = Lift m

  alg ctx hdl (LiftWith with k) = LiftT (with ctx (runLift . hdl)) >>= hdl . fmap k
