{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Trans
( Algebra(..)
, MonadLift(..)
, liftDefault
, AlgebraTrans(..)
, AlgT(..)
, algDefault
) where

import           Control.Monad ((>=>))
import           Control.Monad.Lift
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.Lower
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S.L
import qualified Control.Monad.Trans.State.Strict as S.S
import           Data.Functor.Compose
import           Data.Kind (Type)
import           Effect.Catch.Internal
import           Effect.Error.Internal
import           Effect.Reader.Internal
import           Effect.State.Internal
import           Effect.Sum
import           Effect.Throw.Internal

class Monad m => Algebra m where
  type Sig m :: (Type -> Type) -> (Type -> Type)

  alg :: Functor ctx => Sig m n a -> ctx () -> LowerT ctx n m (ctx a)


class (MonadLift t, Algebra m) => AlgebraTrans t m where
  type SigT t :: (Type -> Type) -> (Type -> Type)

  algT :: Functor ctx => SigT t n a -> ctx () -> LowerT ctx n (t m) (ctx a)


newtype AlgT t (m :: Type -> Type) a = AlgT { runAlgT :: t m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

instance AlgebraTrans t m => Algebra (AlgT t m) where
  type Sig (AlgT t m) = SigT t :+: Sig m

  alg m = mapLowerT AlgT (runAlgT ~<) . algDefault m

algDefault :: (AlgebraTrans t m, Functor ctx) => (SigT t :+: Sig m) n a -> ctx () -> LowerT ctx n (t m) (ctx a)
algDefault = \case
  L l -> algT l
  R r -> liftWithin (fmap getCompose . alg r)


instance Algebra m => AlgebraTrans (E.ExceptT e) m where
  type SigT (E.ExceptT e) = Error e

  algT = \case
    L (Throw e)     -> const $ lift (E.throwE e)
    R (Catch m h k) -> lowerWith (\ lower -> E.catchE (lower m) (lower . h)) >=> lowerCont k

deriving via AlgT (E.ExceptT e) m instance Algebra m => Algebra (E.ExceptT e m)


instance Algebra m => AlgebraTrans (R.ReaderT r) m where
  type SigT (R.ReaderT r) = Reader r

  algT = \case
    Ask       k -> liftInit R.ask >=> lowerCont k
    Local f m k -> mapLowerT (R.local f) id . lower m >=> lowerCont k

deriving via AlgT (R.ReaderT r) m instance Algebra m => Algebra (R.ReaderT r m)


instance Algebra m => AlgebraTrans (S.L.StateT s) m where
  type SigT (S.L.StateT s) = State s

  algT = \case
    Get   k -> liftInit S.L.get     >=> lowerCont k
    Put s k -> liftInit (S.L.put s) >=> lowerCont (const k)

deriving via AlgT (S.L.StateT s) m instance Algebra m => Algebra (S.L.StateT s m)


instance Algebra m => AlgebraTrans (S.S.StateT s) m where
  type SigT (S.S.StateT s) = State s

  algT = \case
    Get   k -> liftInit S.S.get     >=> lowerCont k
    Put s k -> liftInit (S.S.put s) >=> lowerCont (const k)

deriving via AlgT (S.S.StateT s) m instance Algebra m => Algebra (S.S.StateT s m)
