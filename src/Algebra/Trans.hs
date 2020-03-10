{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
, Hom(..)
, runDist
, homDist
, (>~>)
, (<~<)
, (~>)
, (<~)
, (>~)
, (~<)
, Dist(..)
, runLowerT
, LowerT(..)
, initial
, cont
, mapLowerT
, liftInitial
) where

import qualified Control.Category as C
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S.L
import qualified Control.Monad.Trans.State.Strict as S.S
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

  alg :: Functor ctx => ctx () -> Dist ctx n m -> Sig m n a -> m (ctx a)

class (Functor (Ctx t), MonadTrans t) => MonadLift t where
  type Ctx t :: * -> *
  type Ctx t = Identity

  liftWith :: Monad m => LowerT (Ctx t) (t m) m (Ctx t a) -> t m a

liftDefault :: (MonadLift t, Monad m) => m a -> t m a
liftDefault m = liftWith (LowerT (\ ctx _ -> (<$ ctx) <$> m))

class (MonadLift t, Algebra m, Monad (t m)) => AlgebraTrans t m where
  type SigT t :: (Type -> Type) -> (Type -> Type)

  algT :: Functor ctx => SigT t n a -> LowerT ctx n (t m) (ctx a)


newtype AlgT t (m :: Type -> Type) a = AlgT { runAlgT :: t m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

instance AlgebraTrans t m => Algebra (AlgT t m) where
  type Sig (AlgT t m) = SigT t :+: Sig m

  alg ctx hdl = AlgT . runLowerT ctx (Hom runAlgT ~< hdl) . algDefault

algDefault :: AlgebraTrans t m => Functor ctx => (SigT t :+: Sig m) n a -> LowerT ctx n (t m) (ctx a)
algDefault = \case
  L l -> algT l
  R r -> LowerT $ \ ctx1 hdl1 -> liftWith $ LowerT $ \ ctx2 hdl2 -> getCompose <$> alg (Compose (ctx1 <$ ctx2)) (hdl2 <~< hdl1) r


newtype Hom m n = Hom { appHom :: forall x . m x -> n x }

instance C.Category Hom where
  id = Hom id
  Hom f . Hom g = Hom (f . g)


runDist :: ctx (m a) -> Dist ctx m n -> n (ctx a)
runDist cm (Dist run) = run cm

homDist :: Functor n => Hom m n -> Dist Identity m n
homDist hom = Dist (fmap Identity . appHom hom . runIdentity)

(>~>) :: (Functor n, Functor ctx2) => Dist ctx1 l m -> Dist ctx2 m n -> Dist (Compose ctx2 ctx1) l n
Dist hdl1 >~> Dist hdl2 = Dist (fmap Compose . hdl2 . fmap hdl1 . getCompose)

infixr 1 >~>

(<~<) :: (Functor n, Functor ctx2) => Dist ctx2 m n -> Dist ctx1 l m -> Dist (Compose ctx2 ctx1) l n
Dist hdl1 <~< Dist hdl2 = Dist (fmap Compose . hdl1 . fmap hdl2 . getCompose)

infixr 1 <~<

(~>) :: Functor ctx => Hom l m -> Dist ctx m n -> Dist ctx l n
Hom hdl1 ~> Dist hdl2 = Dist (hdl2 . fmap hdl1)

infixr 1 ~>

(<~) :: Functor ctx => Dist ctx m n -> Hom l m -> Dist ctx l n
Dist hdl1 <~ Hom hdl2 = Dist (hdl1 . fmap hdl2)

infixr 1 <~

(>~) :: Dist ctx l m -> Hom m n -> Dist ctx l n
Dist hdl1 >~ Hom hdl2 = Dist (hdl2 . hdl1)

infixr 1 >~

(~<) :: Hom m n -> Dist ctx l m -> Dist ctx l n
Hom hdl1 ~< Dist hdl2 = Dist (hdl1 . hdl2)

infixr 1 ~<

newtype Dist ctx m n = Dist { appDist :: forall x . ctx (m x) -> n (ctx x) }


runLowerT :: ctx () -> Dist ctx m n -> LowerT ctx m n a -> n a
runLowerT ctx hdl (LowerT m) = m ctx hdl

newtype LowerT ctx m n a = LowerT (ctx () -> Dist ctx m n -> n a)
  deriving (Applicative, Functor, Monad) via R.ReaderT (ctx ()) (R.ReaderT (Dist ctx m n) n)

instance MonadTrans (LowerT ctx m) where
  lift = LowerT . const . const

initial :: Functor ctx => m a -> LowerT ctx m n (ctx a)
initial m = LowerT $ runDist . (m <$)

cont :: Functor ctx => (a -> m b) -> ctx a -> LowerT ctx m n (ctx b)
cont k ctx = LowerT . const $ runDist (k <$> ctx)

mapLowerT :: (n a -> n b) -> LowerT ctx m n a -> LowerT ctx m n b
mapLowerT f (LowerT m) = LowerT $ fmap f <$> m

liftInitial :: Functor ctx => ((forall a . m a -> n (ctx a)) -> n b) -> LowerT ctx m n b
liftInitial with = LowerT $ \ ctx hdl -> with (appDist hdl . (<$ ctx))


instance MonadLift (R.ReaderT r) where
  liftWith m = R.ReaderT $ \ r -> runIdentity <$> runLowerT (Identity ()) (homDist (Hom (`R.runReaderT` r))) m

instance Algebra m => AlgebraTrans (R.ReaderT r) m where
  type SigT (R.ReaderT r) = Reader r

  algT = \case
    Ask       k -> lift R.ask >>= initial . k
    Local f m k -> mapLowerT (R.local f) (initial m) >>= cont k

deriving via AlgT (R.ReaderT r) m instance Algebra m => Algebra (R.ReaderT r m)

instance MonadLift (E.ExceptT e) where
  type Ctx (E.ExceptT e) = Either e

  liftWith = E.ExceptT . runLowerT (Right ()) (Dist (either (pure . Left) E.runExceptT))

instance Algebra m => AlgebraTrans (E.ExceptT e) m where
  type SigT (E.ExceptT e) = Error e

  algT = \case
    L (Throw e)     -> lift (E.throwE e)
    R (Catch m h k) -> liftInitial (\ initial -> E.catchE (initial m) (initial . h)) >>= cont k

deriving via AlgT (E.ExceptT e) m instance Algebra m => Algebra (E.ExceptT e m)

instance MonadLift (S.L.StateT s) where
  type Ctx (S.L.StateT s) = (,) s

  liftWith m = S.L.StateT $ \ s -> swap <$> runLowerT (s, ()) (Dist (fmap swap . uncurry (flip S.L.runStateT))) m

instance Algebra m => AlgebraTrans (S.L.StateT s) m where
  type SigT (S.L.StateT s) = State s

  algT = \case
    Get   k -> lift S.L.get     >>= initial . k
    Put s k -> lift (S.L.put s) >>  initial k

deriving via AlgT (S.L.StateT s) m instance Algebra m => Algebra (S.L.StateT s m)

instance MonadLift (S.S.StateT s) where
  type Ctx (S.S.StateT s) = (,) s

  liftWith m = S.S.StateT $ \ s -> swap <$> runLowerT (s, ()) (Dist (fmap swap . uncurry (flip S.S.runStateT))) m

instance Algebra m => AlgebraTrans (S.S.StateT s) m where
  type SigT (S.S.StateT s) = State s

  algT = \case
    Get   k -> lift S.S.get     >>= initial . k
    Put s k -> lift (S.S.put s) >>  initial k

deriving via AlgT (S.S.StateT s) m instance Algebra m => Algebra (S.S.StateT s m)
