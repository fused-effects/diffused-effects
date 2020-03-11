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

  liftWith :: Monad m => (Ctx t () -> LowerT (Ctx t) (t m) m (Ctx t a)) -> t m a

liftDefault :: (MonadLift t, Monad m) => m a -> t m a
liftDefault m = liftWith (\ ctx -> lowerT (\ _ -> (<$ ctx) <$> m))

liftWithin :: (MonadLift t, Monad m) => (Compose (Ctx t) ctx () -> LowerT (Compose (Ctx t) ctx) n m (Ctx t a)) -> ctx () -> LowerT ctx n (t m) a
liftWithin f ctx1 = lowerT $ \ hdl1 -> liftWith $ \ ctx2 -> lowerT $ \ hdl2 -> runLowerT (hdl2 <~< hdl1) (f (Compose (ctx1 <$ ctx2)))


instance MonadLift (E.ExceptT e) where
  type Ctx (E.ExceptT e) = Either e

  liftWith = E.ExceptT . runLowerT (either (pure . Left) E.runExceptT) . ($ Right ())

instance MonadLift M.MaybeT where
  type Ctx M.MaybeT = Maybe

  liftWith = M.MaybeT . runLowerT (maybe (pure Nothing) M.runMaybeT) . ($ Just ())

instance MonadLift (R.ReaderT r) where
  liftWith f = R.ReaderT $ \ r -> runIdentity <$> runLowerT (pureHandler (`R.runReaderT` r)) (f (Identity ()))

instance MonadLift (S.L.StateT s) where
  type Ctx (S.L.StateT s) = (,) s

  liftWith f = S.L.StateT $ \ s -> swap <$> runLowerT (fmap swap . uncurry (flip S.L.runStateT)) (f (s, ()))

instance MonadLift (S.S.StateT s) where
  type Ctx (S.S.StateT s) = (,) s

  liftWith f = S.S.StateT $ \ s -> swap <$> runLowerT (fmap swap . uncurry (flip S.S.runStateT)) (f (s, ()))

instance Monoid w => MonadLift (W.L.WriterT w) where
  type Ctx (W.L.WriterT w) = (,) w

  liftWith f = W.L.WriterT $ swap <$> runLowerT (\ (s, x) -> swap . fmap (mappend s) <$> W.L.runWriterT x) (f (mempty, ()))

instance Monoid w => MonadLift (W.S.WriterT w) where
  type Ctx (W.S.WriterT w) = (,) w

  liftWith f = W.S.WriterT $ swap <$> runLowerT (\ (s, x) -> swap . fmap (mappend s) <$> W.S.runWriterT x) (f (mempty, ()))
