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
, Hom
, runDist
, hom
, (>~>)
, (<~<)
, (~>)
, (<~)
, (>~)
, (~<)
, Dist(..)
, Point(..)
, runLowerT
, LowerT(..)
, initialT
, contT
, mapLowerT
, liftInitialT
, liftWithin
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

import           Control.Category ((>>>))
import qualified Control.Category as C
import           Control.Monad ((<=<))
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S.L
import qualified Control.Monad.Trans.State.Strict as S.S
import           Data.Functor (($>))
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Kind (Type)
import           Data.Tuple (swap)
import           Effect.Catch.Internal
import           Effect.Error.Internal
import           Effect.Reader.Internal
import           Effect.State.Internal
import           Effect.Sum
import           Effect.Throw.Internal

class Monad m => Algebra m where
  type Sig m :: (* -> *) -> (* -> *)

  alg :: Functor ctx => Sig m n a -> LowerT ctx n m (ctx a)


class (Functor (Ctx t), MonadTrans t, forall m . Monad m => Monad (t m)) => MonadLift t where
  type Ctx t :: * -> *
  type Ctx t = Identity

  liftWith :: Monad m => LowerT (Ctx t) (t m) m (Ctx t a) -> t m a

liftDefault :: (MonadLift t, Monad m) => m a -> t m a
liftDefault m = liftWith (LowerT (\ _ ctx -> (<$ ctx) <$> m))


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


type Hom m n = forall x . m x -> n x


runDist :: ctx (m a) -> Dist ctx m n -> n (ctx a)
runDist cm (Dist run) = run cm

hom :: Functor n => Hom m n -> Dist Identity m n
hom hom = Dist (fmap Identity . hom . runIdentity)

(>~>) :: (Functor n, Functor ctx2) => Dist ctx1 l m -> Dist ctx2 m n -> Dist (Compose ctx2 ctx1) l n
Dist hdl1 >~> Dist hdl2 = Dist (fmap Compose . hdl2 . fmap hdl1 . getCompose)

infixr 1 >~>

(<~<) :: (Functor n, Functor ctx2) => Dist ctx2 m n -> Dist ctx1 l m -> Dist (Compose ctx2 ctx1) l n
Dist hdl1 <~< Dist hdl2 = Dist (fmap Compose . hdl1 . fmap hdl2 . getCompose)

infixr 1 <~<

(~>) :: Functor ctx => Hom l m -> Dist ctx m n -> Dist ctx l n
hdl1 ~> Dist hdl2 = Dist (hdl2 . fmap hdl1)

infixr 1 ~>

(<~) :: Functor ctx => Dist ctx m n -> Hom l m -> Dist ctx l n
Dist hdl1 <~ hdl2 = Dist (hdl1 . fmap hdl2)

infixr 1 <~

(>~) :: Dist ctx l m -> Hom m n -> Dist ctx l n
Dist hdl1 >~ hdl2 = Dist (hdl2 . hdl1)

infixr 1 >~

(~<) :: Hom m n -> Dist ctx l m -> Dist ctx l n
hdl1 ~< Dist hdl2 = Dist (hdl1 . hdl2)

infixr 1 ~<

newtype Dist ctx m n = Dist { appDist :: forall x . ctx (m x) -> n (ctx x) }


newtype Point ctx = Point { point :: forall x . x -> ctx x }


runLowerT :: Dist ctx m n -> ctx () -> LowerT ctx m n a -> n a
runLowerT hdl ctx (LowerT m) = m hdl ctx

newtype LowerT ctx m n a = LowerT (Dist ctx m n -> ctx () -> n a)
  deriving (Applicative, Functor, Monad) via R.ReaderT (Dist ctx m n) (R.ReaderT (ctx ()) n)

instance MonadTrans (LowerT ctx m) where
  lift = LowerT . const . const

initialT :: Functor ctx => m a -> LowerT ctx m n (ctx a)
initialT m = liftInitialT ($ m)

contT :: Functor ctx => (a -> m b) -> ctx a -> LowerT ctx m n (ctx b)
contT k ctx = LowerT $ const . runDist (k <$> ctx)

mapLowerT :: (n a -> n b) -> LowerT ctx m n a -> LowerT ctx m n b
mapLowerT f = mapLowerTDist f id

mapLowerTDist :: (n' a -> n b) -> (Dist ctx m n -> Dist ctx m n') -> LowerT ctx m n' a -> LowerT ctx m n b
mapLowerTDist f g = mapLowerTDistCtx f g id

mapLowerTDistCtx :: (n' a -> n b) -> (Dist ctx m n -> Dist ctx' m n') -> (ctx () -> ctx' ()) -> LowerT ctx' m n' a -> LowerT ctx m n b
mapLowerTDistCtx f g h (LowerT m) = LowerT $ \ hdl ctx -> f (m (g hdl) (h ctx))

liftInitialT :: Functor ctx => ((forall a . m a -> n (ctx a)) -> n b) -> LowerT ctx m n b
liftInitialT with = LowerT $ \ hdl ctx -> with (appDist hdl . (<$ ctx))

liftWithin :: (MonadLift t, Monad m) => LowerT (Compose (Ctx t) ctx) n m (Ctx t a) -> LowerT ctx n (t m) a
liftWithin m = LowerT $ \ hdl1 ctx1 -> liftWith $ LowerT $ \ hdl2 ctx2 -> runLowerT (hdl2 <~< hdl1) (Compose (ctx1 <$ ctx2)) m


newtype LowerC ctx m n a b = LowerC (Dist ctx m n -> ctx a -> n (ctx b))

instance Monad n => C.Category (LowerC ctx m n) where
  id = LowerC $ const pure

  LowerC f . LowerC g = LowerC $ \ hdl -> f hdl <=< g hdl

fromLowerT :: LowerT ctx m n (ctx a) -> LowerC ctx m n () a
fromLowerT (LowerT r) = LowerC r

fromLowerC :: LowerC ctx m n () a -> LowerT ctx m n (ctx a)
fromLowerC (LowerC r) = LowerT r


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


instance MonadLift (R.ReaderT r) where
  liftWith m = R.ReaderT $ \ r -> runIdentity <$> runLowerT (hom (`R.runReaderT` r)) (Identity ()) m

instance Algebra m => AlgebraTrans (R.ReaderT r) m where
  type SigT (R.ReaderT r) = Reader r

  algT = fromLowerC . \case
    Ask       k -> liftC R.ask >>> contC k
    Local f m k -> mapLowerC (R.local f) (initialC m) >>> contC k

deriving via AlgT (R.ReaderT r) m instance Algebra m => Algebra (R.ReaderT r m)

instance MonadLift (E.ExceptT e) where
  type Ctx (E.ExceptT e) = Either e

  liftWith = E.ExceptT . runLowerT (Dist (either (pure . Left) E.runExceptT)) (Right ())

instance Algebra m => AlgebraTrans (E.ExceptT e) m where
  type SigT (E.ExceptT e) = Error e

  algT = fromLowerC . \case
    L (Throw e)     -> liftC (E.throwE e)
    R (Catch m h k) -> liftInitialC (\ initialC -> E.catchE (initialC m) (initialC . h)) >>> contC k

deriving via AlgT (E.ExceptT e) m instance Algebra m => Algebra (E.ExceptT e m)

instance MonadLift (S.L.StateT s) where
  type Ctx (S.L.StateT s) = (,) s

  liftWith m = S.L.StateT $ \ s -> swap <$> runLowerT (Dist (fmap swap . uncurry (flip S.L.runStateT))) (s, ()) m

instance Algebra m => AlgebraTrans (S.L.StateT s) m where
  type SigT (S.L.StateT s) = State s

  algT = fromLowerC . \case
    Get   k -> liftC S.L.get     >>> contC k
    Put s k -> liftC (S.L.put s) >>> contC (const k)

deriving via AlgT (S.L.StateT s) m instance Algebra m => Algebra (S.L.StateT s m)

instance MonadLift (S.S.StateT s) where
  type Ctx (S.S.StateT s) = (,) s

  liftWith m = S.S.StateT $ \ s -> swap <$> runLowerT (Dist (fmap swap . uncurry (flip S.S.runStateT))) (s, ()) m

instance Algebra m => AlgebraTrans (S.S.StateT s) m where
  type SigT (S.S.StateT s) = State s

  algT = fromLowerC . \case
    Get   k -> liftC S.S.get     >>> contC k
    Put s k -> liftC (S.S.put s) >>> contC (const k)

deriving via AlgT (S.S.StateT s) m instance Algebra m => Algebra (S.S.StateT s m)
