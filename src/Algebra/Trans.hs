{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.Trans
( AlgebraTrans(..)
) where

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S.L
import qualified Control.Monad.Trans.State.Strict as S.S
import           Data.Functor.Identity
import           Data.Kind (Type)
import           Data.Tuple (swap)
import           Effect.Catch.Internal
import           Effect.Error.Internal
import           Effect.Reader.Internal
import           Effect.Sum
import           Effect.State.Internal
import           Effect.Throw.Internal

-- FIXME: can’t express non-orthogonal algebras

class MonadTrans t => AlgebraTrans t where
  type SigT t :: (Type -> Type) -> (Type -> Type)

  algT :: Monad m => SigT t (t m) a -> t m a

  liftWith :: Monad m => (forall ctx . Functor ctx => ctx () -> (forall a . ctx (t m a) -> m (ctx a)) -> m (ctx a)) -> t m a


instance AlgebraTrans (R.ReaderT r) where
  type SigT (R.ReaderT r) = Reader r

  algT = \case
    Ask       k -> R.ask       >>= k
    Local f m k -> R.local f m >>= k

  liftWith f = R.ReaderT (\ r -> runIdentity <$> f (Identity ()) (fmap Identity . (`R.runReaderT` r) . runIdentity))

instance AlgebraTrans (E.ExceptT e) where
  type SigT (E.ExceptT e) = Error e

  algT = \case
    L (Throw e)     -> E.throwE e
    R (Catch m h k) -> E.catchE m h >>= k

  liftWith f = E.ExceptT $ f (Right ()) (either (pure . Left) E.runExceptT)

instance AlgebraTrans (S.L.StateT s) where
  type SigT (S.L.StateT s) = State s

  algT = \case
    Get   k -> S.L.get   >>= k
    Put s k -> S.L.put s >>  k

  liftWith f = S.L.StateT $ \ s -> swap <$> f (s, ()) (fmap swap . uncurry (flip S.L.runStateT))

instance AlgebraTrans (S.S.StateT s) where
  type SigT (S.S.StateT s) = State s

  algT = \case
    Get   k -> S.S.get   >>= k
    Put s k -> S.S.put s >>  k

  liftWith f = S.S.StateT $ \ s -> swap <$> f (s, ()) (fmap swap . uncurry (flip S.S.runStateT))
