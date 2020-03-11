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
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S.L
import qualified Control.Monad.Trans.State.Strict as S.S
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Tuple (swap)

class (Functor (Ctx t), MonadTrans t, forall m . Monad m => Monad (t m)) => MonadLift t where
  type Ctx t :: * -> *
  type Ctx t = Identity

  liftWith :: Monad m => LowerT (Ctx t) (t m) m (Ctx t a) -> t m a

liftDefault :: (MonadLift t, Monad m) => m a -> t m a
liftDefault m = liftWith (LowerT (\ _ ctx -> (<$ ctx) <$> m))

liftWithin :: (MonadLift t, Monad m) => LowerT (Compose (Ctx t) ctx) n m (Ctx t a) -> LowerT ctx n (t m) a
liftWithin m = LowerT $ \ hdl1 ctx1 -> liftWith $ LowerT $ \ hdl2 ctx2 -> runLowerT (hdl2 <~< hdl1) (Compose (ctx1 <$ ctx2)) m


instance MonadLift (E.ExceptT e) where
  type Ctx (E.ExceptT e) = Either e

  liftWith = E.ExceptT . runLowerT (Dist (either (pure . Left) E.runExceptT)) (Right ())

instance MonadLift (R.ReaderT r) where
  liftWith m = R.ReaderT $ \ r -> runIdentity <$> runLowerT (hom (`R.runReaderT` r)) (Identity ()) m

instance MonadLift (S.L.StateT s) where
  type Ctx (S.L.StateT s) = (,) s

  liftWith m = S.L.StateT $ \ s -> swap <$> runLowerT (Dist (fmap swap . uncurry (flip S.L.runStateT))) (s, ()) m

instance MonadLift (S.S.StateT s) where
  type Ctx (S.S.StateT s) = (,) s

  liftWith m = S.S.StateT $ \ s -> swap <$> runLowerT (Dist (fmap swap . uncurry (flip S.S.runStateT))) (s, ()) m
