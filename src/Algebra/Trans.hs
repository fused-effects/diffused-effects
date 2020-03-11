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

  alg :: Functor ctx => ctx () -> Sig m n a -> LowerT ctx n m (ctx a)


class (MonadLift t, Algebra m) => AlgebraTrans t m where
  type SigT t :: (Type -> Type) -> (Type -> Type)

  algT :: Functor ctx => ctx () -> SigT t n a -> LowerT ctx n (t m) (ctx a)


newtype AlgT t (m :: Type -> Type) a = AlgT { runAlgT :: t m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

instance AlgebraTrans t m => Algebra (AlgT t m) where
  type Sig (AlgT t m) = SigT t :+: Sig m

  alg ctx = mapLowerT AlgT (runAlgT ~<) . algDefault ctx

algDefault :: (AlgebraTrans t m, Functor ctx) => ctx () -> (SigT t :+: Sig m) n a -> LowerT ctx n (t m) (ctx a)
algDefault ctx = \case
  L l -> algT ctx l
  R r -> liftWithin (\ ctx -> getCompose <$> alg ctx r) ctx


instance Algebra m => AlgebraTrans (E.ExceptT e) m where
  type SigT (E.ExceptT e) = Error e

  algT ctx = \case
    L (Throw e)     -> lift (E.throwE e)
    R (Catch m h k) -> lowerWith (\ lower -> E.catchE (lower m) (lower . h)) ctx >>= lowerCont k

deriving via AlgT (E.ExceptT e) m instance Algebra m => Algebra (E.ExceptT e m)


instance Algebra m => AlgebraTrans (R.ReaderT r) m where
  type SigT (R.ReaderT r) = Reader r

  algT ctx = \case
    Ask       k -> lift R.ask >>= (`lower` ctx) . k
    Local f m k -> mapLowerT (R.local f) id (lower m ctx) >>= lowerCont k

deriving via AlgT (R.ReaderT r) m instance Algebra m => Algebra (R.ReaderT r m)


instance Algebra m => AlgebraTrans (S.L.StateT s) m where
  type SigT (S.L.StateT s) = State s

  algT ctx = \case
    Get   k -> lift S.L.get     >>= (`lower` ctx) . k
    Put s k -> lift (S.L.put s) >>  lower k ctx

deriving via AlgT (S.L.StateT s) m instance Algebra m => Algebra (S.L.StateT s m)


instance Algebra m => AlgebraTrans (S.S.StateT s) m where
  type SigT (S.S.StateT s) = State s

  algT ctx = \case
    Get   k -> lift S.S.get     >>= (`lower` ctx) . k
    Put s k -> lift (S.S.put s) >>  lower k ctx

deriving via AlgT (S.S.StateT s) m instance Algebra m => Algebra (S.S.StateT s m)
