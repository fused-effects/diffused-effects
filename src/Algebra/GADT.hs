{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Algebra.GADT
( Algebra(..)
, send
, thread
, lower
, cont
) where

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty)
import           Effect.GADT
import           Effect.Sum

class Monad m => Algebra m where
  type Sig m :: (Type -> Type) -> (Type -> Type)

  alg :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> ctx () -> Sig m n a -> m (ctx a)


send :: (Member eff sig, sig ~ Sig m, Algebra m) => eff m a -> m a
send = fmap runIdentity . alg (fmap Identity . runIdentity) (Identity ()) . inj
{-# INLINE send #-}

thread :: (Functor ctx1, Functor ctx2, Algebra m) => (forall x . ctx1 (ctx2 (n x)) -> m (ctx1 (ctx2 x))) -> ctx1 (ctx2 ()) -> Sig m n a -> m (ctx1 (ctx2 a))
thread hdl ctx = fmap getCompose . alg (fmap Compose . hdl . getCompose) (Compose ctx)
{-# INLINE thread #-}


lower :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> ctx () -> n a -> m (ctx a)
lower hdl ctx = hdl . (<$ ctx)
{-# INLINE lower #-}

cont :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> (a -> n b) -> ctx a -> m (ctx b)
cont hdl k = hdl . fmap k
{-# INLINE cont #-}


instance Algebra Maybe where
  type Sig Maybe = Empty

  alg _ _ Empty = Nothing
  {-# INLINE alg #-}

instance Algebra NonEmpty where
  type Sig NonEmpty = Choose

  alg _ ctx Choose = pure (True <$ ctx) <> pure (False <$ ctx)
  {-# INLINE alg #-}

instance Algebra (Either e) where
  type Sig (Either e) = Error e

  alg hdl ctx = \case
    L (Throw e)   -> Left e
    R (Catch m h) -> either (lower hdl ctx . h) pure (lower hdl ctx m)
  {-# INLINE alg #-}

instance Algebra [] where
  type Sig [] = NonDet

  alg _ ctx = \case
    L Empty  -> []
    R Choose -> [True <$ ctx, False <$ ctx]
  {-# INLINE alg #-}

instance Algebra ((->) r) where
  type Sig ((->) r) = Reader r

  alg hdl ctx = \case
    Ask       -> (<$ ctx)
    Local f m -> lower hdl ctx m . f
  {-# INLINE alg #-}


instance Algebra m => Algebra (M.MaybeT m) where
  type Sig (M.MaybeT m) = Empty :+: Sig m

  alg hdl ctx = \case
    L Empty -> M.MaybeT (pure Nothing)
    R other -> M.MaybeT (thread (maybe (pure Nothing) (M.runMaybeT . hdl)) (Just ctx) other)
  {-# INLINE alg #-}

instance Algebra m => Algebra (E.ExceptT e m) where
  type Sig (E.ExceptT e m) = Error e :+: Sig m

  alg hdl ctx = \case
    L (L (Throw e))   -> E.throwE e
    L (R (Catch m h)) -> E.catchE (lower hdl ctx m) (lower hdl ctx . h)
    R other           -> E.ExceptT $ thread (either (pure . Left) (E.runExceptT . hdl)) (Right ctx) other
  {-# INLINE alg #-}

instance Algebra m => Algebra (R.ReaderT r m) where
  type Sig (R.ReaderT r m) = Reader r :+: Sig m

  alg hdl ctx = \case
    L Ask         -> (<$ ctx) <$> R.ask
    L (Local f m) -> R.local f (lower hdl ctx m)
    R other       -> R.ReaderT $ \ r -> alg ((`R.runReaderT` r) . hdl) ctx other
  {-# INLINE alg #-}
