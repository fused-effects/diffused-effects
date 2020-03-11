{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra
( type (~>)
, Algebra(..)
, (:+:)(..)
, (:.:)(..)
, Has
, thread
, run
, runLift
, send
-- $base
-- $transformers
) where

import           Control.Monad (join)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Identity as I
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S.L
import qualified Control.Monad.Trans.State.Strict as S.S
import qualified Control.Monad.Trans.Writer.Lazy as W.L
import qualified Control.Monad.Trans.Writer.Strict as W.S
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

type f ~> g = forall x . f x -> g x

infixr 0 ~>


class Monad m => Algebra m where
  type Sig m :: (* -> *) -> (* -> *)

  alg :: Functor ctx => ctx () -> (forall x . ctx (n x) -> m (ctx x)) -> Sig m n a -> m (ctx a)


type Has eff m = (Members eff (Sig m), Algebra m)


newtype (f :.: g) a = C { runC :: f (g a) }
  deriving (Foldable, Functor, Traversable)

infixr 7 :.:


thread :: (Functor ctx1, Functor ctx2, Algebra m) => ctx1 (ctx2 ()) -> (forall x . ctx1 (ctx2 (n x)) -> m (ctx1 (ctx2 x))) -> Sig m n a -> m (ctx1 (ctx2 a))
thread ctx hdl = fmap runC . alg (C ctx) (fmap C . hdl . runC)
{-# INLINE thread #-}

run :: Identity a -> a
run = runIdentity
{-# INLINE run #-}

runLift :: I.IdentityT m a -> m a
runLift = I.runIdentityT
{-# INLINE runLift #-}


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff sig, sig ~ Sig m, Algebra m) => eff m a -> m a
send = fmap runIdentity . alg (Identity ()) (fmap Identity . runIdentity) . inj
{-# INLINE send #-}


-- $base
-- We define 'Algebra' instances for a bunch of monads defined in @base@.

instance Algebra NonEmpty where
  type Sig NonEmpty = Choose

  alg ctx hdl (Choose k) = hdl (k True <$ ctx) <> hdl (k False <$ ctx)
  {-# INLINE alg #-}

instance Algebra Maybe where
  type Sig Maybe = Empty

  alg _ _ Empty = Nothing
  {-# INLINE alg #-}

instance Algebra (Either e) where
  type Sig (Either e) = Error e

  alg ctx hdl = \case
    L (Throw e)     -> Left e
    R (Catch m h k) -> either (hdl . (<$ ctx) . h) pure (hdl (m <$ ctx)) >>= hdl . fmap k
  {-# INLINE alg #-}

instance Algebra Identity where
  type Sig Identity = Lift Identity

  alg ctx hdl (LiftWith with k) = with hdl ctx >>= hdl . fmap k
  {-# INLINE alg #-}

instance Algebra IO where
  type Sig IO = Lift IO

  alg ctx hdl (LiftWith with k) = with hdl ctx >>= hdl . fmap k
  {-# INLINE alg #-}

instance Algebra [] where
  type Sig [] = NonDet

  alg ctx hdl = \case
    L Empty      -> []
    R (Choose k) -> hdl (k True <$ ctx) <> hdl (k False <$ ctx)
  {-# INLINE alg #-}

instance Algebra ((->) r) where
  type Sig ((->) r) = Reader r

  alg ctx hdl = \case
    Ask       k -> join (hdl . (<$ ctx) . k)
    Local f m k -> hdl (m <$ ctx) . f >>= hdl . fmap k
  {-# INLINE alg #-}

instance Monoid w => Algebra ((,) w) where
  type Sig ((,) w) = Writer w

  alg ctx hdl = \case
    Tell w     k -> join (w, hdl (k <$ ctx))
    Listen m   k -> let (w, a) = hdl (m <$ ctx) ; (w', a') = hdl (fmap (k w) a) in (mappend w w', a')
    Censor f m k -> let (w, a) = hdl (m <$ ctx) ; (w', a') = hdl (fmap k a) in (mappend (f w) w', a')
  {-# INLINE alg #-}


-- $transformers
-- We define 'Algebra' instances for a bunch of monad transformers defined in @transformers@.

instance Algebra m => Algebra (M.MaybeT m) where
  type Sig (M.MaybeT m) = Empty :+: Sig m

  alg ctx hdl = \case
    L Empty -> M.MaybeT (pure Nothing)
    R other -> M.MaybeT (thread (Just ctx) (maybe (pure Nothing) (M.runMaybeT . hdl)) other)
  {-# INLINE alg #-}

instance Algebra m => Algebra (E.ExceptT e m) where
  type Sig (E.ExceptT e m) = Error e :+: Sig m

  alg ctx hdl = \case
    L (L (Throw e))     -> E.throwE e
    L (R (Catch m h k)) -> E.catchE (hdl (m <$ ctx)) (hdl . (<$ ctx) . h) >>= hdl . fmap k
    R other             -> E.ExceptT $ thread (Right ctx) (either (pure . Left) (E.runExceptT . hdl)) other
  {-# INLINE alg #-}

instance Monad m => Algebra (I.IdentityT m) where
  type Sig (I.IdentityT m) = Lift m

  alg ctx hdl (LiftWith with k) = I.IdentityT (with (I.runIdentityT . hdl) ctx) >>= hdl . fmap k
  {-# INLINE alg #-}

instance Algebra m => Algebra (R.ReaderT r m) where
  type Sig (R.ReaderT r m) = Reader r :+: Sig m

  alg ctx hdl = \case
    L (Ask       k) -> R.ask >>= hdl . (<$ ctx) . k
    L (Local f m k) -> R.local f (hdl (m <$ ctx)) >>= hdl . fmap k
    R other         -> R.ReaderT $ \ r -> alg ctx ((`R.runReaderT` r) . hdl) other
  {-# INLINE alg #-}

instance Algebra m => Algebra (S.L.StateT s m) where
  type Sig (S.L.StateT s m) = State s :+: Sig m

  alg ctx hdl = \case
    L (Get   k) -> S.L.get >>= hdl . (<$ ctx) . k
    L (Put s k) -> S.L.put s *> hdl (k <$ ctx)
    R other     -> S.L.StateT $ \ s -> swap <$> thread (s, ctx) (\ (s, x) -> swap <$> S.L.runStateT (hdl x) s) other
  {-# INLINE alg #-}

instance Algebra m => Algebra (S.S.StateT s m) where
  type Sig (S.S.StateT s m) = State s :+: Sig m

  alg ctx hdl = \case
    L (Get   k) -> S.S.get >>= hdl . (<$ ctx) . k
    L (Put s k) -> S.S.put s *> hdl (k <$ ctx)
    R other     -> S.S.StateT $ \ s -> swap <$> thread (s, ctx) (\ (s, x) -> swap <$> S.S.runStateT (hdl x) s) other
  {-# INLINE alg #-}

instance (Algebra m, Monoid w) => Algebra (W.L.WriterT w m) where
  type Sig (W.L.WriterT w m) = Writer w :+: Sig m

  alg ctx hdl = \case
    L (Tell w k)     -> W.L.tell w *> hdl (k <$ ctx)
    L (Listen m k)   -> W.L.listen (hdl (m <$ ctx)) >>= hdl . uncurry (fmap . k) . swap
    L (Censor f m k) -> W.L.censor f (hdl (m <$ ctx)) >>= hdl . fmap k
    R other          -> W.L.WriterT $ swap <$> thread (mempty, ctx) (\ (s, x) -> swap . fmap (mappend s) <$> W.L.runWriterT (hdl x)) other
  {-# INLINE alg #-}

instance (Algebra m, Monoid w) => Algebra (W.S.WriterT w m) where
  type Sig (W.S.WriterT w m) = Writer w :+: Sig m

  alg ctx hdl = \case
    L (Tell w k)     -> W.S.tell w *> hdl (k <$ ctx)
    L (Listen m k)   -> W.S.listen (hdl (m <$ ctx)) >>= hdl . uncurry (fmap . k) . swap
    L (Censor f m k) -> W.S.censor f (hdl (m <$ ctx)) >>= hdl . fmap k
    R other          -> W.S.WriterT $ swap <$> thread (mempty, ctx) (\ (s, x) -> swap . fmap (mappend s) <$> W.S.runWriterT (hdl x)) other
  {-# INLINE alg #-}
