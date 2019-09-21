{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, TypeOperators #-}
module Control.Effect.Lift
( -- * Lift effect
  Lift(..)
, sendM
) where

import Control.Algebra
import GHC.Generics

newtype Lift sig m k = Lift { unLift :: sig (m k) }
  deriving (Functor, Generic1)

instance Functor m => HFunctor (Lift m)
instance Functor m => Effect   (Lift m)

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
sendM :: (Has (Lift n) m, Functor n) => n a -> m a
sendM = send . Lift . fmap pure
