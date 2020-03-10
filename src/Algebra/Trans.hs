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
, Dist(..)
, runLowerT
, runLowerT'
, LowerT(..)
, initial
, cont
, mapLowerT
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
  liftWith :: Monad m => (forall ctx . Functor ctx => ctx () -> (forall a . ctx (t m a) -> m (ctx a)) -> m (ctx a)) -> t m a

liftDefault :: (MonadLift t, Monad m) => m a -> t m a
liftDefault m = liftWith (\ ctx _ -> (<$ ctx) <$> m)

class (MonadLift t, Algebra m, Monad (t m)) => AlgebraTrans t m where
  type SigT t :: (Type -> Type) -> (Type -> Type)

  algT :: Functor ctx => ctx () -> (forall a . ctx (n a) -> t m (ctx a)) -> SigT t n a -> t m (ctx a)


newtype AlgT t (m :: Type -> Type) a = AlgT { runAlgT :: t m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

instance AlgebraTrans t m => Algebra (AlgT t m) where
  type Sig (AlgT t m) = SigT t :+: Sig m

  alg ctx hdl = AlgT . algDefault ctx (runAlgT . hdl)

algDefault :: AlgebraTrans t m => Functor ctx => ctx () -> (forall a . ctx (n a) -> t m (ctx a)) -> (SigT t :+: Sig m) n a -> t m (ctx a)
algDefault ctx1 hdl1 = \case
  L l -> algT ctx1 hdl1 l
  R r -> liftWith $ \ ctx2 hdl2 -> getCompose <$> alg (Compose (ctx1 <$ ctx2)) (fmap Compose . hdl2 . fmap hdl1 . getCompose) r


runDist :: ctx (m a) -> Dist ctx m n -> n (ctx a)
runDist cm (Dist run) = run cm

newtype Dist ctx m n = Dist (forall x . ctx (m x) -> n (ctx x))


runLowerT :: ctx () -> (forall x . ctx (m x) -> n (ctx x)) -> LowerT ctx m n a -> n a
runLowerT ctx hdl (LowerT m) = m ctx (Dist hdl)

runLowerT' :: Functor m => LowerT Identity m m a -> m a
runLowerT' = runLowerT (Identity ()) (fmap Identity . runIdentity)

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


instance MonadLift (R.ReaderT r) where
  liftWith f = R.ReaderT $ \ r -> runIdentity <$> f (Identity ()) (fmap Identity . (`R.runReaderT` r) . runIdentity)

instance Algebra m => AlgebraTrans (R.ReaderT r) m where
  type SigT (R.ReaderT r) = Reader r

  algT ctx hdl = runLowerT ctx hdl . \case
    Ask       k -> lift R.ask >>= initial . k
    Local f m k -> mapLowerT (R.local f) (initial m) >>= cont k

deriving via AlgT (R.ReaderT r) m instance Algebra m => Algebra (R.ReaderT r m)

instance MonadLift (E.ExceptT e) where
  liftWith f = E.ExceptT $ f (Right ()) (either (pure . Left) E.runExceptT)

instance Algebra m => AlgebraTrans (E.ExceptT e) m where
  type SigT (E.ExceptT e) = Error e

  algT ctx hdl = runLowerT ctx hdl . \case
    L (Throw e)     -> lift (E.throwE e)
    R (Catch m h k) -> LowerT (\ ctx (Dist hdl) -> E.catchE (runLowerT ctx hdl (initial m)) (runLowerT ctx hdl . initial . h)) >>= cont k

deriving via AlgT (E.ExceptT e) m instance Algebra m => Algebra (E.ExceptT e m)

instance MonadLift (S.L.StateT s) where
  liftWith f = S.L.StateT $ \ s -> swap <$> f (s, ()) (fmap swap . uncurry (flip S.L.runStateT))

instance Algebra m => AlgebraTrans (S.L.StateT s) m where
  type SigT (S.L.StateT s) = State s

  algT ctx hdl = \case
    Get   k -> S.L.get   >>= hdl . (<$ ctx) . k
    Put s k -> S.L.put s >>  hdl (k <$ ctx)

deriving via AlgT (S.L.StateT s) m instance Algebra m => Algebra (S.L.StateT s m)

instance MonadLift (S.S.StateT s) where
  liftWith f = S.S.StateT $ \ s -> swap <$> f (s, ()) (fmap swap . uncurry (flip S.S.runStateT))

instance Algebra m => AlgebraTrans (S.S.StateT s) m where
  type SigT (S.S.StateT s) = State s

  algT ctx hdl = \case
    Get   k -> S.S.get   >>= hdl . (<$ ctx) . k
    Put s k -> S.S.put s >>  hdl (k <$ ctx)

deriving via AlgT (S.S.StateT s) m instance Algebra m => Algebra (S.S.StateT s m)
