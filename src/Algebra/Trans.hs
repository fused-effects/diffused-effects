{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Algebra.Trans
( Algebra(..)
, AlgebraTrans(..)
, AlgT(..)
, algDefault
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
import           Effect.Sum
import           Effect.State.Internal
import           Effect.Throw.Internal

class Monad m => Algebra m where
  type Sig m :: (* -> *) -> (* -> *)

  alg :: Functor ctx => ctx () -> (forall x . ctx (n x) -> m (ctx x)) -> Sig m n a -> m (ctx a)


class (MonadTrans t, Algebra m) => AlgebraTrans t m where
  type SigT t :: (Type -> Type) -> (Type -> Type)

  algT :: Functor ctx => ctx () -> (forall a . ctx (n a) -> t m (ctx a)) -> SigT t n a -> t m (ctx a)

  liftWith :: (forall ctx . Functor ctx => ctx () -> (forall a . ctx (t m a) -> m (ctx a)) -> m (ctx a)) -> t m a


newtype AlgT t (m :: Type -> Type) a = AlgT { runAlgT :: t m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

algDefault :: AlgebraTrans t m => Functor ctx => ctx () -> (forall a . ctx (n a) -> t m (ctx a)) -> (SigT t :+: Sig m) n a -> t m (ctx a)
algDefault ctx1 hdl1 = \case
  L l -> algT ctx1 hdl1 l
  R r -> liftWith $ \ ctx2 hdl2 -> getCompose <$> alg (Compose (ctx1 <$ ctx2)) (fmap Compose . hdl2 . fmap hdl1 . getCompose) r


instance Algebra m => AlgebraTrans (R.ReaderT r) m where
  type SigT (R.ReaderT r) = Reader r

  algT ctx hdl = \case
    Ask       k -> R.ask                      >>= hdl . (<$ ctx) . k
    Local f m k -> R.local f (hdl (m <$ ctx)) >>= hdl . fmap k

  liftWith f = R.ReaderT $ \ r -> runIdentity <$> f (Identity ()) (fmap Identity . (`R.runReaderT` r) . runIdentity)

instance Algebra m => AlgebraTrans (E.ExceptT e) m where
  type SigT (E.ExceptT e) = Error e

  algT ctx hdl = \case
    L (Throw e)     -> E.throwE e
    R (Catch m h k) -> E.catchE (hdl (m <$ ctx)) (hdl . (<$ ctx) . h) >>= hdl . fmap k

  liftWith f = E.ExceptT $ f (Right ()) (either (pure . Left) E.runExceptT)

instance Algebra m => AlgebraTrans (S.L.StateT s) m where
  type SigT (S.L.StateT s) = State s

  algT ctx hdl = \case
    Get   k -> S.L.get   >>= hdl . (<$ ctx) . k
    Put s k -> S.L.put s >>  hdl (k <$ ctx)

  liftWith f = S.L.StateT $ \ s -> swap <$> f (s, ()) (fmap swap . uncurry (flip S.L.runStateT))

instance Algebra m => AlgebraTrans (S.S.StateT s) m where
  type SigT (S.S.StateT s) = State s

  algT ctx hdl = \case
    Get   k -> S.S.get   >>= hdl . (<$ ctx) . k
    Put s k -> S.S.put s >>  hdl (k <$ ctx)

  liftWith f = S.S.StateT $ \ s -> swap <$> f (s, ()) (fmap swap . uncurry (flip S.S.runStateT))
