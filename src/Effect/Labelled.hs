{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Effect.Labelled
( runLabelled
, Labelled(..)
, LabelledMember(..)
, HasLabelled
, sendLabelled
, runUnderLabel
, UnderLabel(..)
, module Algebra
) where

import Algebra
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Kind (Type)
import Effect.Sum (L, reassociateSumL)

newtype Labelled (label :: k) (sub :: (Type -> Type) -> (Type -> Type)) m a = Labelled (sub m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadIO, MonadPlus, MonadTrans)

runLabelled :: forall label sub m a . Labelled label sub m a -> sub m a
runLabelled (Labelled l) = l

instance (Algebra (sub m), Sig (sub m) ~ (eff :+: Sig m)) => Algebra (Labelled label sub m) where
  type Sig (Labelled label sub m) = Labelled label (L (Sig (sub m))) :+: Sig m

  alg hdl ctx = \case
    L eff -> Labelled (alg (runLabelled . hdl) ctx (L (runLabelled eff)))
    R sig -> Labelled (alg (runLabelled . hdl) ctx (R sig))
  {-# INLINE alg #-}


class LabelledMember label (sub :: (Type -> Type) -> (Type -> Type)) sup | label sup -> sub where
  injLabelled :: Labelled label sub m a -> sup m a

instance LabelledMember label t (Labelled label t) where
  injLabelled = id
  {-# INLINE injLabelled #-}

instance {-# OVERLAPPABLE #-}
         LabelledMember label t (l1 :+: l2 :+: r)
      => LabelledMember label t ((l1 :+: l2) :+: r) where
  injLabelled = reassociateSumL . injLabelled
  {-# INLINE injLabelled #-}

instance {-# OVERLAPPABLE #-}
         LabelledMember label l (Labelled label l :+: r) where
  injLabelled = L
  {-# INLINE injLabelled #-}

instance {-# OVERLAPPABLE #-}
         LabelledMember label l r
      => LabelledMember label l (l' :+: r) where
  injLabelled = R . injLabelled
  {-# INLINE injLabelled #-}


type HasLabelled label eff m = (LabelledMember label eff (Sig m), Algebra m)

sendLabelled :: forall label eff m a . HasLabelled label eff m => eff m a -> m a
sendLabelled = fmap runIdentity . alg (fmap Identity . runIdentity) (Identity ()) . injLabelled @label . Labelled
{-# INLINABLE sendLabelled #-}


newtype UnderLabel (label :: k) (sub :: (Type -> Type) -> (Type -> Type)) (m :: Type -> Type) a = UnderLabel (m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadIO, MonadPlus)

runUnderLabel :: forall label sub m a . UnderLabel label sub m a -> m a
runUnderLabel (UnderLabel l) = l

instance MonadTrans (UnderLabel sub label) where
  lift = UnderLabel
  {-# INLINE lift #-}

instance HasLabelled label sub m => Algebra (UnderLabel label sub m) where
  type Sig (UnderLabel label sub m) = sub :+: Sig m

  alg hdl ctx = \case
    L sub -> UnderLabel (alg (runUnderLabel . hdl) ctx (injLabelled @label (Labelled sub)))
    R sig -> UnderLabel (alg (runUnderLabel . hdl) ctx sig)
  {-# INLINE alg #-}
