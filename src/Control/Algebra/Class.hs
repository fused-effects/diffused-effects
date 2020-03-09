{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Algebra.Class
( Algebra(..)
) where

import           Control.Effect.Catch.Internal
import           Control.Effect.Choose.Internal
import           Control.Effect.Class
import           Control.Effect.Empty.Internal
import           Control.Effect.Error.Internal
import           Control.Effect.Lift.Internal
import           Control.Effect.NonDet.Internal
import           Control.Effect.Reader.Internal
import           Control.Effect.State.Internal
import           Control.Effect.Sum
import           Control.Effect.Throw.Internal
import           Control.Effect.Writer.Internal
import           Control.Monad (join)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty)
import           Data.Tuple (swap)

class (HFunctor (Sig m), Monad m) => Algebra m where
  type Sig m :: (* -> *) -> (* -> *)

  alg :: Sig m m a -> m a

instance Algebra Maybe where
  type Sig Maybe = Empty

  alg Empty = Nothing

instance Algebra (Either e) where
  type Sig (Either e) = Error e

  alg = \case
    L (Throw e)     -> Left e
    R (Catch m h k) -> either h pure m >>= k

instance Algebra ((->) r) where
  type Sig ((->) r) = Reader r

  alg = \case
    Ask       k -> join k
    Local f m k -> m . f >>= k

instance Algebra NonEmpty where
  type Sig NonEmpty = Choose

  alg (Choose m) = m True <> m False

instance Algebra [] where
  type Sig [] = NonDet

  alg = \case
    L Empty      -> []
    R (Choose m) -> m True <> m False

instance Monoid w => Algebra ((,) w) where
  type Sig ((,) w) = Writer w

  alg = \case
    Tell w     k -> join (w, k)
    Listen m   k -> let (w, a) = m ; (w', a') = k w a in (mappend w w', a')
    Censor f m k -> let (w, a) = m ; (w', a') = k   a in (mappend (f w) w', a')

instance Algebra IO where
  type Sig IO = Lift IO

  alg (LiftWith with k) = with (Identity ()) coerce >>= k . runIdentity

instance Algebra Identity where
  type Sig Identity = Lift Identity

  alg (LiftWith with k) = with (Identity ()) coerce >>= k . runIdentity


instance (Algebra m, Effect (Sig m)) => Algebra (Except.ExceptT e m) where
  type Sig (Except.ExceptT e m) = Error e :+: Sig m

  alg = \case
    L (L (Throw e))     -> Except.throwE e
    L (R (Catch m h k)) -> Except.catchE m h >>= k
    R other             -> Except.ExceptT $ alg (handle (Right ()) (either (pure . Left) Except.runExceptT) other)

instance Algebra m => Algebra (Reader.ReaderT r m) where
  type Sig (Reader.ReaderT r m) = Reader r :+: Sig m

  alg = \case
    L (Ask       k) -> Reader.ask >>= k
    L (Local f m k) -> Reader.local f m >>= k
    R other         -> Reader.ReaderT $ \ r -> alg (hmap (`Reader.runReaderT` r) other)

instance (Algebra m, Effect (Sig m)) => Algebra (State.Lazy.StateT s m) where
  type Sig (State.Lazy.StateT s m) = State s :+: Sig m

  alg = \case
    L (Get   k) -> State.Lazy.get >>= k
    L (Put s k) -> State.Lazy.put s *> k
    R other     -> State.Lazy.StateT $ \ s -> swap <$> alg (handle (s, ()) (\ (s, x) -> swap <$> State.Lazy.runStateT x s) other)

instance (Algebra m, Effect (Sig m)) => Algebra (State.Strict.StateT s m) where
  type Sig (State.Strict.StateT s m) = State s :+: Sig m

  alg = \case
    L (Get   k) -> State.Strict.get >>= k
    L (Put s k) -> State.Strict.put s *> k
    R other     -> State.Strict.StateT $ \ s -> swap <$> alg (handle (s, ()) (\ (s, x) -> swap <$> State.Strict.runStateT x s) other)

instance (Algebra m, Effect (Sig m), Monoid w) => Algebra (Writer.Lazy.WriterT w m) where
  type Sig (Writer.Lazy.WriterT w m) = Writer w :+: Sig m

  alg = \case
    L (Tell w k)     -> Writer.Lazy.tell w *> k
    L (Listen m k)   -> Writer.Lazy.listen m >>= uncurry (flip k)
    L (Censor f m k) -> Writer.Lazy.censor f m >>= k
    R other          -> Writer.Lazy.WriterT $ swap <$> alg (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Lazy.runWriterT x) other)

instance (Algebra m, Effect (Sig m), Monoid w) => Algebra (Writer.Strict.WriterT w m) where
  type Sig (Writer.Strict.WriterT w m) = Writer w :+: Sig m

  alg = \case
    L (Tell w k)     -> Writer.Strict.tell w *> k
    L (Listen m k)   -> Writer.Strict.listen m >>= uncurry (flip k)
    L (Censor f m k) -> Writer.Strict.censor f m >>= k
    R other          -> Writer.Strict.WriterT $ swap <$> alg (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.Strict.runWriterT x) other)
