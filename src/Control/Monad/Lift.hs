{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Lift
( MonadLift(..)
, liftDefault
, liftWithin
) where

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.Lower
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S.L
import qualified Control.Monad.Trans.State.Strict as S.S
import qualified Control.Monad.Trans.Writer.Lazy as W.L
import qualified Control.Monad.Trans.Writer.Strict as W.S
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Tuple (swap)

class (Functor (Ctx t), MonadTrans t, forall m . Monad m => Monad (t m)) => MonadLift t where
  type Ctx t :: * -> *
  type Ctx t = Identity

  liftWith :: Monad m => LowerT (Ctx t) (t m) m (Ctx t a) -> t m a

liftDefault :: (MonadLift t, Monad m) => m a -> t m a
liftDefault m = liftWith (lowerT (\ _ ctx -> (<$ ctx) <$> m))

liftWithin :: (MonadLift t, Monad m) => LowerT (Compose (Ctx t) ctx) n m (Ctx t a) -> LowerT ctx n (t m) a
liftWithin m = lowerT $ \ hdl1 ctx1 -> liftWith $ lowerT $ \ hdl2 ctx2 -> runLowerT (hdl2 <~< hdl1) (Compose (ctx1 <$ ctx2)) m


instance MonadLift (E.ExceptT e) where
  type Ctx (E.ExceptT e) = Either e

  liftWith = E.ExceptT . runLowerT (either (pure . Left) E.runExceptT) (Right ())

instance MonadLift M.MaybeT where
  type Ctx M.MaybeT = Maybe

  liftWith = M.MaybeT . runLowerT (maybe (pure Nothing) M.runMaybeT) (Just ())

instance MonadLift (R.ReaderT r) where
  liftWith m = R.ReaderT $ \ r -> runIdentity <$> runLowerT (pureHandler (`R.runReaderT` r)) (Identity ()) m

instance MonadLift (S.L.StateT s) where
  type Ctx (S.L.StateT s) = (,) s

  liftWith m = S.L.StateT $ \ s -> swap <$> runLowerT (fmap swap . uncurry (flip S.L.runStateT)) (s, ()) m

instance MonadLift (S.S.StateT s) where
  type Ctx (S.S.StateT s) = (,) s

  liftWith m = S.S.StateT $ \ s -> swap <$> runLowerT (fmap swap . uncurry (flip S.S.runStateT)) (s, ()) m

instance Monoid w => MonadLift (W.L.WriterT w) where
  type Ctx (W.L.WriterT w) = (,) w

  liftWith m = W.L.WriterT $ swap <$> runLowerT (\ (s, x) -> swap . fmap (mappend s) <$> W.L.runWriterT x) (mempty, ()) m

instance Monoid w => MonadLift (W.S.WriterT w) where
  type Ctx (W.S.WriterT w) = (,) w

  liftWith m = W.S.WriterT $ swap <$> runLowerT (\ (s, x) -> swap . fmap (mappend s) <$> W.S.runWriterT x) (mempty, ()) m
