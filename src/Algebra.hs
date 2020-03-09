{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
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
, send
) where

import           Control.Monad (join)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
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

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff (Sig m), Algebra m) => eff m a -> m a
send = fmap runIdentity <$> alg (Identity ()) (fmap Identity . runIdentity) . inj
{-# INLINE send #-}


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

instance Algebra ((->) r) where
  type Sig ((->) r) = Reader r

  alg ctx hdl = \case
    Ask       k -> join (hdl . (<$ ctx) . k)
    Local f m k -> hdl (m <$ ctx) . f >>= hdl . fmap k
  {-# INLINE alg #-}

instance Algebra NonEmpty where
  type Sig NonEmpty = Choose

  alg ctx hdl (Choose k) = hdl (k True <$ ctx) <> hdl (k False <$ ctx)
  {-# INLINE alg #-}

instance Algebra [] where
  type Sig [] = NonDet

  alg ctx hdl = \case
    L Empty      -> []
    R (Choose k) -> hdl (k True <$ ctx) <> hdl (k False <$ ctx)
  {-# INLINE alg #-}

instance Monoid w => Algebra ((,) w) where
  type Sig ((,) w) = Writer w

  alg ctx hdl = \case
    Tell w     k -> join (w, hdl (k <$ ctx))
    Listen m   k -> let (w, a) = hdl (m <$ ctx) ; (w', a') = hdl (fmap (k w) a) in (mappend w w', a')
    Censor f m k -> let (w, a) = hdl (m <$ ctx) ; (w', a') = hdl (fmap k a) in (mappend (f w) w', a')
  {-# INLINE alg #-}

instance Algebra IO where
  type Sig IO = Lift IO

  alg ctx hdl (LiftWith with k) = with ctx hdl >>= hdl . fmap k
  {-# INLINE alg #-}

instance Algebra Identity where
  type Sig Identity = Lift Identity

  alg ctx hdl (LiftWith with k) = with ctx hdl >>= hdl . fmap k
  {-# INLINE alg #-}


instance Algebra m => Algebra (Maybe.MaybeT m) where
  type Sig (Maybe.MaybeT m) = Empty :+: Sig m

  alg ctx hdl = \case
    L Empty -> Maybe.MaybeT (pure Nothing)
    R other -> Maybe.MaybeT (thread (Just ctx) (maybe (pure Nothing) (Maybe.runMaybeT . hdl)) other)
  {-# INLINE alg #-}

instance Algebra m => Algebra (Except.ExceptT e m) where
  type Sig (Except.ExceptT e m) = Error e :+: Sig m

  alg ctx hdl = \case
    L (L (Throw e))     -> Except.throwE e
    L (R (Catch m h k)) -> Except.catchE (hdl (m <$ ctx)) (hdl . (<$ ctx) . h) >>= hdl . fmap k
    R other             -> Except.ExceptT $ thread (Right ctx) (either (pure . Left) (Except.runExceptT . hdl)) other
  {-# INLINE alg #-}

instance Algebra m => Algebra (Reader.ReaderT r m) where
  type Sig (Reader.ReaderT r m) = Reader r :+: Sig m

  alg ctx hdl = \case
    L (Ask       k) -> Reader.ask >>= hdl . (<$ ctx) . k
    L (Local f m k) -> Reader.local f (hdl (m <$ ctx)) >>= hdl . fmap k
    R other         -> Reader.ReaderT $ \ r -> alg ctx ((`Reader.runReaderT` r) . hdl) other
  {-# INLINE alg #-}

instance Algebra m => Algebra (State.Lazy.StateT s m) where
  type Sig (State.Lazy.StateT s m) = State s :+: Sig m

  alg ctx hdl = \case
    L (Get   k) -> State.Lazy.get >>= hdl . (<$ ctx) . k
    L (Put s k) -> State.Lazy.put s *> hdl (k <$ ctx)
    R other     -> State.Lazy.StateT $ \ s -> swap <$> thread (s, ctx) (\ (s, x) -> swap <$> State.Lazy.runStateT (hdl x) s) other
  {-# INLINE alg #-}

instance Algebra m => Algebra (State.Strict.StateT s m) where
  type Sig (State.Strict.StateT s m) = State s :+: Sig m

  alg ctx hdl = \case
    L (Get   k) -> State.Strict.get >>= hdl . (<$ ctx) . k
    L (Put s k) -> State.Strict.put s *> hdl (k <$ ctx)
    R other     -> State.Strict.StateT $ \ s -> swap <$> thread (s, ctx) (\ (s, x) -> swap <$> State.Strict.runStateT (hdl x) s) other
  {-# INLINE alg #-}

instance (Algebra m, Monoid w) => Algebra (Writer.Lazy.WriterT w m) where
  type Sig (Writer.Lazy.WriterT w m) = Writer w :+: Sig m

  alg ctx hdl = \case
    L (Tell w k)     -> Writer.Lazy.tell w *> hdl (k <$ ctx)
    L (Listen m k)   -> Writer.Lazy.listen (hdl (m <$ ctx)) >>= hdl . uncurry (fmap . k) . swap
    L (Censor f m k) -> Writer.Lazy.censor f (hdl (m <$ ctx)) >>= hdl . fmap k
    R other          -> Writer.Lazy.WriterT $ swap <$> thread (mempty, ctx) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Lazy.runWriterT (hdl x)) other
  {-# INLINE alg #-}

instance (Algebra m, Monoid w) => Algebra (Writer.Strict.WriterT w m) where
  type Sig (Writer.Strict.WriterT w m) = Writer w :+: Sig m

  alg ctx hdl = \case
    L (Tell w k)     -> Writer.Strict.tell w *> hdl (k <$ ctx)
    L (Listen m k)   -> Writer.Strict.listen (hdl (m <$ ctx)) >>= hdl . uncurry (fmap . k) . swap
    L (Censor f m k) -> Writer.Strict.censor f (hdl (m <$ ctx)) >>= hdl . fmap k
    R other          -> Writer.Strict.WriterT $ swap <$> thread (mempty, ctx) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Strict.runWriterT (hdl x)) other

  {-# INLINE alg #-}
