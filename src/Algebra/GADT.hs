{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Algebra.GADT
( Handler
, Algebra(..)
, Has
, send
, thread
, liftInit
, lowerInit
, lowerCont
) where

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S.S
import qualified Control.Monad.Trans.Writer.Strict as W.S
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Tuple (swap)
import           Effect.GADT
import           Effect.Sum

type Handler ctx m n = forall x. ctx (m x) -> n (ctx x)

class Monad m => Algebra m where
  type Sig m :: (Type -> Type) -> (Type -> Type)

  alg :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> ctx () -> Sig m n a -> m (ctx a)


type Has eff m = (Members eff (Sig m), Algebra m)

send :: (Member eff sig, sig ~ Sig m, Algebra m) => eff m a -> m a
send = fmap runIdentity . alg (fmap Identity . runIdentity) (Identity ()) . inj
{-# INLINE send #-}

thread :: (Functor ctx1, Functor ctx2, Algebra m) => (forall x . ctx1 (n x) -> m (ctx1 x)) -> (forall x . ctx2 (o x) -> n (ctx2 x)) -> ctx1 (ctx2 ()) -> Sig m o a -> m (ctx1 (ctx2 a))
thread hdl1 hdl2 ctx = fmap getCompose . alg (fmap Compose . hdl1 . fmap hdl2 . getCompose) (Compose ctx)
{-# INLINE thread #-}


liftInit :: (Functor ctx, Functor m) => ctx () -> m a -> m (ctx a)
liftInit ctx = fmap (<$ ctx)
{-# INLINE liftInit #-}

lowerInit :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> ctx () -> n a -> m (ctx a)
lowerInit hdl ctx = hdl . (<$ ctx)
{-# INLINE lowerInit #-}

lowerCont :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> (a -> n b) -> ctx a -> m (ctx b)
lowerCont hdl k = hdl . fmap k
{-# INLINE lowerCont #-}


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
    R (Catch m h) -> either (lowerInit hdl ctx . h) pure (lowerInit hdl ctx m)
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
    Local f m -> lowerInit hdl ctx m . f
  {-# INLINE alg #-}


instance Algebra m => Algebra (M.MaybeT m) where
  type Sig (M.MaybeT m) = Empty :+: Sig m

  alg hdl ctx = \case
    L Empty -> M.MaybeT (pure Nothing)
    R other -> M.MaybeT (thread (maybe (pure Nothing) M.runMaybeT) hdl (Just ctx) other)
  {-# INLINE alg #-}

instance Algebra m => Algebra (E.ExceptT e m) where
  type Sig (E.ExceptT e m) = Error e :+: Sig m

  alg hdl ctx = \case
    L (L (Throw e))   -> E.throwE e
    L (R (Catch m h)) -> E.catchE (lowerInit hdl ctx m) (lowerInit hdl ctx . h)
    R other           -> E.ExceptT $ thread (either (pure . Left) E.runExceptT) hdl (Right ctx) other
  {-# INLINE alg #-}

instance Algebra m => Algebra (R.ReaderT r m) where
  type Sig (R.ReaderT r m) = Reader r :+: Sig m

  alg hdl ctx = \case
    L Ask         -> liftInit ctx R.ask
    L (Local f m) -> R.local f (lowerInit hdl ctx m)
    R other       -> R.ReaderT $ \ r -> alg ((`R.runReaderT` r) . hdl) ctx other
  {-# INLINE alg #-}

instance Algebra m => Algebra (S.S.StateT s m) where
  type Sig (S.S.StateT s m) = State s :+: Sig m

  alg hdl ctx = \case
    L Get     -> liftInit ctx S.S.get
    L (Put s) -> liftInit ctx (S.S.put s)
    R other   -> S.S.StateT $ \ s -> swap <$> thread (fmap swap . uncurry (flip S.S.runStateT)) hdl (s, ctx) other
  {-# INLINE alg #-}

instance (Monoid w, Algebra m) => Algebra (W.S.WriterT w m) where
  type Sig (W.S.WriterT w m) = Writer w :+: Sig m

  alg hdl ctx = \case
    L (Tell w)     -> liftInit ctx (W.S.tell w)
    L (Listen m)   -> W.S.listen (lowerInit hdl ctx m) >>= \ (a, w') -> pure ((w',) <$> a)
    L (Censor f m) -> W.S.censor f (lowerInit hdl ctx m)
    R other        -> W.S.WriterT $ swap <$> thread (\ (w, x) -> swap . fmap (mappend w) <$> W.S.runWriterT x) hdl (mempty, ctx) other
  {-# INLINE alg #-}
