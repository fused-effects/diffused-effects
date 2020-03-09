{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.Lift
( -- * Lift carrier
  runLift
, LiftC(..)
  -- * Lift effect
, module Effect.Lift
) where

import Algebra
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Effect.Lift

runLift :: LiftC m a -> m a
runLift (LiftC m) = m

newtype LiftC m a = LiftC (m a)
  deriving (Applicative, Functor, Monad, MonadFix)

instance MonadTrans LiftC where
  lift = LiftC

instance Monad m => Algebra (LiftC m) where
  type Sig (LiftC m) = Lift m

  alg ctx hdl (LiftWith with k) = LiftC (with ctx (runLift . hdl)) >>= hdl . fmap k
