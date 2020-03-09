{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Algebra.Lift
( -- * Lift carrier
  runLift
, LiftC(..)
  -- * Lift effect
, module Control.Effect.Lift
) where

import           Control.Algebra
import           Control.Applicative (Alternative)
import           Control.Effect.Lift
import           Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Functor.Identity

runLift :: LiftC m a -> m a
runLift (LiftC m) = m

newtype LiftC m a = LiftC (m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans LiftC where
  lift = LiftC

instance Monad m => Algebra (LiftC m) where
  type Signature (LiftC m) = Lift m

  alg (LiftWith with k) = LiftC (with (Identity ()) (fmap Identity . runLift . runIdentity)) >>= k . runIdentity
