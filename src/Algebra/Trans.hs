{-# LANGUAGE DerivingVia #-}
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
, runDist
, homDist
, composeDist
, Dist(..)
, runLowerT
, runLowerTHom
, mkLowerT
, LowerT(..)
, initial
, cont
, mapLowerT
, liftInitial
) where

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

  alg :: Functor ctx => ctx () -> (forall x . ctx (n x) -> m (ctx x)) -> Sig m n a -> m (ctx a)

class MonadTrans t => MonadLift t where
  liftWith :: Monad m => (forall ctx . Functor ctx => LowerT ctx (t m) m (ctx a)) -> t m a

liftDefault :: (MonadLift t, Monad m) => m a -> t m a
liftDefault m = liftWith (LowerT (\ ctx _ -> (<$ ctx) <$> m))

class (MonadLift t, Algebra m, Monad (t m)) => AlgebraTrans t m where
  type SigT t :: (Type -> Type) -> (Type -> Type)

  algT :: Functor ctx => ctx () -> Dist ctx n (t m) -> SigT t n a -> t m (ctx a)


newtype AlgT t (m :: Type -> Type) a = AlgT { runAlgT :: t m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

instance AlgebraTrans t m => Algebra (AlgT t m) where
  type Sig (AlgT t m) = SigT t :+: Sig m

  alg ctx hdl = AlgT . algDefault ctx (Dist (runAlgT . hdl))

algDefault :: AlgebraTrans t m => Functor ctx => ctx () -> Dist ctx n (t m) -> (SigT t :+: Sig m) n a -> t m (ctx a)
algDefault ctx1 (Dist hdl1) = \case
  L l -> algT ctx1 (Dist hdl1) l
  R r -> liftWith $ LowerT $ \ ctx2 (Dist hdl2) -> getCompose <$> alg (Compose (ctx1 <$ ctx2)) (fmap Compose . hdl2 . fmap hdl1 . getCompose) r


runDist :: ctx (m a) -> Dist ctx m n -> n (ctx a)
runDist cm (Dist run) = run cm

homDist :: Functor n => (forall x . m x -> n x) -> Dist Identity m n
homDist hom = Dist (fmap Identity . hom . runIdentity)

composeDist :: (Functor n, Functor ctx2) => Dist ctx1 l m -> Dist ctx2 m n -> Dist (Compose ctx2 ctx1) l n
composeDist (Dist hdl1) (Dist hdl2) = Dist (fmap Compose . hdl2 . fmap hdl1 . getCompose)

newtype Dist ctx m n = Dist (forall x . ctx (m x) -> n (ctx x))


runLowerT :: ctx () -> Dist ctx m n -> LowerT ctx m n a -> n a
runLowerT ctx hdl (LowerT m) = m ctx hdl

runLowerTHom :: Functor n => (forall x . m x -> n x) -> LowerT Identity m n (Identity a) -> n a
runLowerTHom hom = fmap runIdentity . runLowerT (Identity ()) (Dist (fmap Identity . hom . runIdentity))

mkLowerT :: (ctx () -> (forall x . ctx (m x) -> n (ctx x)) -> n a) -> LowerT ctx m n a
mkLowerT f = LowerT $ \ ctx (Dist hdl) -> f ctx hdl

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
liftInitial with = LowerT $ \ ctx (Dist hdl) -> with (hdl . (<$ ctx))


instance MonadLift (R.ReaderT r) where
  liftWith m = R.ReaderT $ \ r -> runLowerTHom (`R.runReaderT` r) m

instance Algebra m => AlgebraTrans (R.ReaderT r) m where
  type SigT (R.ReaderT r) = Reader r

  algT ctx hdl = runLowerT ctx hdl . \case
    Ask       k -> lift R.ask >>= initial . k
    Local f m k -> mapLowerT (R.local f) (initial m) >>= cont k

deriving via AlgT (R.ReaderT r) m instance Algebra m => Algebra (R.ReaderT r m)

instance MonadLift (E.ExceptT e) where
  liftWith = E.ExceptT . runLowerT (Right ()) (Dist (either (pure . Left) E.runExceptT))

instance Algebra m => AlgebraTrans (E.ExceptT e) m where
  type SigT (E.ExceptT e) = Error e

  algT ctx hdl = runLowerT ctx hdl . \case
    L (Throw e)     -> lift (E.throwE e)
    R (Catch m h k) -> liftInitial (\ initial -> E.catchE (initial m) (initial . h)) >>= cont k

deriving via AlgT (E.ExceptT e) m instance Algebra m => Algebra (E.ExceptT e m)

instance MonadLift (S.L.StateT s) where
  liftWith m = S.L.StateT $ \ s -> swap <$> runLowerT (s, ()) (Dist (fmap swap . uncurry (flip S.L.runStateT))) m

instance Algebra m => AlgebraTrans (S.L.StateT s) m where
  type SigT (S.L.StateT s) = State s

  algT ctx hdl = runLowerT ctx hdl . \case
    Get   k -> lift S.L.get     >>= initial . k
    Put s k -> lift (S.L.put s) >>  initial k

deriving via AlgT (S.L.StateT s) m instance Algebra m => Algebra (S.L.StateT s m)

instance MonadLift (S.S.StateT s) where
  liftWith m = S.S.StateT $ \ s -> swap <$> runLowerT (s, ()) (Dist (fmap swap . uncurry (flip S.S.runStateT))) m

instance Algebra m => AlgebraTrans (S.S.StateT s) m where
  type SigT (S.S.StateT s) = State s

  algT ctx hdl = runLowerT ctx hdl . \case
    Get   k -> lift S.S.get     >>= initial . k
    Put s k -> lift (S.S.put s) >>  initial k

deriving via AlgT (S.S.StateT s) m instance Algebra m => Algebra (S.S.StateT s m)
