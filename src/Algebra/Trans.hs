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
, LowerC(..)
, fromLowerT
, fromLowerC
, initialC
, liftInitialC
, contC
, liftC
, mapLowerC
, CtxC(..)
) where

import qualified Control.Arrow as A
import           Control.Category ((<<<), (>>>))
import qualified Control.Category as C
import           Control.Monad ((<=<))
import           Control.Monad.Lift
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.Lower
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S.L
import qualified Control.Monad.Trans.State.Strict as S.S
import           Data.Coerce
import           Data.Functor (($>))
import           Data.Functor.Compose
import           Data.Kind (Type)
import           Effect.Catch.Internal
import           Effect.Error.Internal
import           Effect.Reader.Internal
import           Effect.State.Internal
import           Effect.Sum
import           Effect.Throw.Internal

class Monad m => Algebra m where
  type Sig m :: (* -> *) -> (* -> *)

  alg :: Functor ctx => Sig m n a -> LowerT ctx n m (ctx a)


class (MonadLift t, Algebra m, Monad (t m)) => AlgebraTrans t m where
  type SigT t :: (Type -> Type) -> (Type -> Type)

  algT :: Functor ctx => SigT t n a -> LowerT ctx n (t m) (ctx a)


newtype AlgT t (m :: Type -> Type) a = AlgT { runAlgT :: t m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

instance AlgebraTrans t m => Algebra (AlgT t m) where
  type Sig (AlgT t m) = SigT t :+: Sig m

  alg = mapLowerTDist AlgT (runAlgT ~<) . algDefault

algDefault :: (AlgebraTrans t m, Functor ctx) => (SigT t :+: Sig m) n a -> LowerT ctx n (t m) (ctx a)
algDefault = \case
  L l -> algT l
  R r -> liftWithin (getCompose <$> alg r)


newtype ReaderC r arr a b = ReaderC (r -> arr a b)

instance C.Category arr => C.Category (ReaderC r arr) where
  id = ReaderC $ const C.id

  ReaderC f . ReaderC g = ReaderC $ \ hdl -> f hdl <<< g hdl

instance A.Arrow arr => A.Arrow (ReaderC r arr) where
  arr = ReaderC . const . A.arr

  ReaderC l *** ReaderC r = ReaderC $ \ hdl -> l hdl A.*** r hdl


newtype LowerC ctx m n a b = LowerC (Dist ctx m n -> ctx a -> n (ctx b))

deriving via ReaderC (Dist ctx m n) (CtxC ctx n) instance Monad n => C.Category (LowerC ctx m n)

fromLowerT :: LowerT ctx m n (ctx a) -> LowerC ctx m n () a
fromLowerT = coerce

fromLowerC :: LowerC ctx m n () a -> LowerT ctx m n (ctx a)
fromLowerC = coerce


initialC :: Functor ctx => m a -> LowerC ctx m n () a
initialC m = liftInitialC ($ m)

liftInitialC :: Functor ctx => ((forall a . m a -> n (ctx a)) -> n (ctx b)) -> LowerC ctx m n () b
liftInitialC with = LowerC $ \ hdl ctx -> with (appDist hdl . (<$ ctx))

contC :: Functor ctx => (a -> m b) -> LowerC ctx m n a b
contC k = LowerC $ \ (Dist hdl) ctx -> hdl (k <$> ctx)

liftC :: (Functor ctx, Functor n) => n a -> LowerC ctx m n () a
liftC n = LowerC . const $ (<$> n) . ($>)


mapLowerC :: (n (ctx b) -> n (ctx c)) -> LowerC ctx m n a b -> LowerC ctx m n a c
mapLowerC f (LowerC r) = LowerC $ \ hdl -> f . r hdl


newtype CtxC ctx m a b = CtxC (ctx a -> m (ctx b))

instance Monad m => C.Category (CtxC ctx m) where
  id = CtxC pure

  CtxC f . CtxC g = CtxC $ f <=< g



instance Algebra m => AlgebraTrans (R.ReaderT r) m where
  type SigT (R.ReaderT r) = Reader r

  algT = fromLowerC . \case
    Ask       k -> liftC R.ask >>> contC k
    Local f m k -> mapLowerC (R.local f) (initialC m) >>> contC k

deriving via AlgT (R.ReaderT r) m instance Algebra m => Algebra (R.ReaderT r m)


instance Algebra m => AlgebraTrans (E.ExceptT e) m where
  type SigT (E.ExceptT e) = Error e

  algT = fromLowerC . \case
    L (Throw e)     -> liftC (E.throwE e)
    R (Catch m h k) -> liftInitialC (\ initialC -> E.catchE (initialC m) (initialC . h)) >>> contC k

deriving via AlgT (E.ExceptT e) m instance Algebra m => Algebra (E.ExceptT e m)


instance Algebra m => AlgebraTrans (S.L.StateT s) m where
  type SigT (S.L.StateT s) = State s

  algT = fromLowerC . \case
    Get   k -> liftC S.L.get     >>> contC k
    Put s k -> liftC (S.L.put s) >>> contC (const k)

deriving via AlgT (S.L.StateT s) m instance Algebra m => Algebra (S.L.StateT s m)

instance Algebra m => AlgebraTrans (S.S.StateT s) m where
  type SigT (S.S.StateT s) = State s

  algT = fromLowerC . \case
    Get   k -> liftC S.S.get     >>> contC k
    Put s k -> liftC (S.S.put s) >>> contC (const k)

deriving via AlgT (S.S.StateT s) m instance Algebra m => Algebra (S.S.StateT s m)
