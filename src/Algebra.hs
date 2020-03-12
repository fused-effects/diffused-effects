{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra
( Algebra(..)
, (:+:)(..)
, Has
, thread
, run
, runLift
, send
, lowering
-- $base
-- $transformers
) where

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Identity as I
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S.L
import qualified Control.Monad.Trans.State.Strict as S.S
import qualified Control.Monad.Trans.Writer.Lazy as W.L
import qualified Control.Monad.Trans.Writer.Strict as W.S
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty)
import           Data.Tuple (swap)
import           Effect.Catch.Internal
import           Effect.Choose.Internal
import           Effect.Empty.Internal
import           Effect.Error.Internal
import           Effect.Lift.Internal
import           Effect.NonDet.Internal
import           Effect.Reader.Internal
import           Effect.State.Internal
import           Effect.Sum
import           Effect.Throw.Internal
import           Effect.Writer.Internal

class Monad m => Algebra m where
  type Sig m :: (* -> *) -> (* -> *)

  alg :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> ctx () -> Sig m n a -> m (ctx a)


type Has eff m = (Members eff (Sig m), Algebra m)


thread :: (Functor ctx1, Functor ctx2, Algebra m) => (forall x . ctx1 (ctx2 (n x)) -> m (ctx1 (ctx2 x))) -> ctx1 (ctx2 ()) -> Sig m n a -> m (ctx1 (ctx2 a))
thread hdl ctx = fmap getCompose . alg (fmap Compose . hdl . getCompose) (Compose ctx)
{-# INLINE thread #-}

run :: Identity a -> a
run = runIdentity
{-# INLINE run #-}

runLift :: I.IdentityT m a -> m a
runLift = I.runIdentityT
{-# INLINE runLift #-}


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff sig, sig ~ Sig m, Algebra m) => eff m a -> m a
send = fmap runIdentity . alg (fmap Identity . runIdentity) (Identity ()) . inj
{-# INLINE send #-}


lowering
  :: Functor ctx
  => ((forall x . m x -> n (ctx x)) -> a)
  -> (forall x . ctx (m x) -> n (ctx x))
  -> ctx ()
  -> a
lowering with hdl ctx = with (hdl . (<$ ctx))
{-# INLINE lowering #-}


-- $base
-- We define 'Algebra' instances for a bunch of monads defined in @base@.

instance Algebra NonEmpty where
  type Sig NonEmpty = Choose

  alg _ ctx Choose = pure (True <$ ctx) <> pure (False <$ ctx)
  {-# INLINE alg #-}

instance Algebra Maybe where
  type Sig Maybe = Empty

  alg _ _ Empty = Nothing
  {-# INLINE alg #-}

instance Algebra (Either e) where
  type Sig (Either e) = Error e

  alg = lowering $ \ init -> \case
    L (Throw e)     -> Left e
    R (Catch m h) -> either (init . h) pure (init m)
  {-# INLINE alg #-}

instance Algebra Identity where
  type Sig Identity = Lift Identity

  alg hdl ctx (LiftWith with) = with hdl ctx
  {-# INLINE alg #-}

instance Algebra IO where
  type Sig IO = Lift IO

  alg hdl ctx (LiftWith with) = with hdl ctx
  {-# INLINE alg #-}

instance Algebra [] where
  type Sig [] = NonDet

  alg _ ctx = \case
    L Empty  -> []
    R Choose -> [ True <$ ctx, False <$ ctx ]
  {-# INLINE alg #-}

instance Algebra ((->) r) where
  type Sig ((->) r) = Reader r

  alg hdl ctx = \case
    Ask       -> (<$ ctx)
    Local f m -> hdl (m <$ ctx) . f
  {-# INLINE alg #-}

instance Monoid w => Algebra ((,) w) where
  type Sig ((,) w) = Writer w

  alg hdl ctx = \case
    Tell w     -> (w, ctx)
    Listen   m -> let (w, a) = hdl (m <$ ctx) in (w, (,) w <$> a)
    Censor f m -> let (w, a) = hdl (m <$ ctx) in (f w, a)
  {-# INLINE alg #-}


-- $transformers
-- We define 'Algebra' instances for a bunch of monad transformers defined in @transformers@.

instance Algebra m => Algebra (M.MaybeT m) where
  type Sig (M.MaybeT m) = Empty :+: Sig m

  alg hdl ctx = \case
    L Empty -> M.MaybeT (pure Nothing)
    R other -> M.MaybeT (thread (maybe (pure Nothing) (M.runMaybeT . hdl)) (Just ctx) other)
  {-# INLINE alg #-}

instance Algebra m => Algebra (E.ExceptT e m) where
  type Sig (E.ExceptT e m) = Error e :+: Sig m

  alg hdl ctx = lowering (\ init -> \case
    L (L (Throw e))   -> E.throwE e
    L (R (Catch m h)) -> E.catchE (init m) (init . h)
    R other           -> E.ExceptT $ thread (either (pure . Left) (E.runExceptT . hdl)) (Right ctx) other) hdl ctx
  {-# INLINE alg #-}

instance Monad m => Algebra (I.IdentityT m) where
  type Sig (I.IdentityT m) = Lift m

  alg hdl ctx (LiftWith with) = I.IdentityT (with (I.runIdentityT . hdl) ctx)
  {-# INLINE alg #-}

instance Algebra m => Algebra (R.ReaderT r m) where
  type Sig (R.ReaderT r m) = Reader r :+: Sig m

  alg hdl ctx = \case
    L Ask         -> R.asks (<$ ctx)
    L (Local f m) -> R.local f (hdl (m <$ ctx))
    R other       -> R.ReaderT $ \ r -> alg ((`R.runReaderT` r) . hdl) ctx other
  {-# INLINE alg #-}

instance Algebra m => Algebra (S.L.StateT s m) where
  type Sig (S.L.StateT s m) = State s :+: Sig m

  alg hdl ctx = \case
    L Get     -> S.L.gets (<$ ctx)
    L (Put s) -> ctx <$ S.L.put s
    R other   -> S.L.StateT $ \ s -> swap <$> thread (\ (s, x) -> swap <$> S.L.runStateT (hdl x) s) (s, ctx) other
  {-# INLINE alg #-}

instance Algebra m => Algebra (S.S.StateT s m) where
  type Sig (S.S.StateT s m) = State s :+: Sig m

  alg hdl ctx = \case
    L Get     -> S.S.gets (<$ ctx)
    L (Put s) -> ctx <$ S.S.put s
    R other   -> S.S.StateT $ \ s -> swap <$> thread (\ (s, x) -> swap <$> S.S.runStateT (hdl x) s) (s, ctx) other
  {-# INLINE alg #-}

swapAndLift :: Functor ctx => (ctx a, w) -> ctx (w, a)
swapAndLift p = (,) (snd p) <$> fst p
{-# INLINE swapAndLift #-}

instance (Algebra m, Monoid w) => Algebra (W.L.WriterT w m) where
  type Sig (W.L.WriterT w m) = Writer w :+: Sig m

  alg hdl ctx = \case
    L (Tell w)     -> ctx <$ W.L.tell w
    L (Listen m)   -> swapAndLift <$> W.L.listen (hdl (m <$ ctx))
    L (Censor f m) -> W.L.censor f (hdl (m <$ ctx))
    R other          -> W.L.WriterT $ swap <$> thread (\ (s, x) -> swap . fmap (mappend s) <$> W.L.runWriterT (hdl x)) (mempty, ctx) other
  {-# INLINE alg #-}

instance (Algebra m, Monoid w) => Algebra (W.S.WriterT w m) where
  type Sig (W.S.WriterT w m) = Writer w :+: Sig m

  alg hdl ctx = \case
    L (Tell w)     -> ctx <$ W.S.tell w
    L (Listen m)   -> swapAndLift <$> W.S.listen (hdl (m <$ ctx))
    L (Censor f m) -> W.S.censor f (hdl (m <$ ctx))
    R other        -> W.S.WriterT $ swap <$> thread (\ (s, x) -> swap . fmap (mappend s) <$> W.S.runWriterT (hdl x)) (mempty, ctx) other
  {-# INLINE alg #-}
