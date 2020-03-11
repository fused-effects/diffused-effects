{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Algebra.GADT
( Algebra(..)
, send
, thread
) where

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


instance Algebra Maybe where
  type Sig Maybe = Empty

  alg _ _ Empty = Nothing

instance Algebra NonEmpty where
  type Sig NonEmpty = Choose

  alg _ ctx Choose = pure (True <$ ctx) <> pure (False <$ ctx)

instance Algebra (Either e) where
  type Sig (Either e) = Error e

  alg hdl ctx = \case
    L (Throw e)   -> Left e
    R (Catch m h) -> either (lower . h) pure (lower m)
    where
    lower = hdl . (<$ ctx)
  {-# INLINE alg #-}

instance Algebra [] where
  type Sig [] = NonDet

  alg _ ctx = \case
    L Empty  -> []
    R Choose -> [True <$ ctx, False <$ ctx]

instance Algebra ((->) r) where
  type Sig ((->) r) = Reader r

  alg hdl ctx = \case
    Ask       -> (<$ ctx)
    Local f m -> lower m . f
    where
    lower = hdl . (<$ ctx)


instance Algebra m => Algebra (R.ReaderT r m) where
  type Sig (R.ReaderT r m) = Reader r :+: Sig m

  alg hdl ctx = \case
    L Ask         -> (<$ ctx) <$> R.ask
    L (Local f m) -> R.local f (lower m)
    R other       -> R.ReaderT $ \ r -> alg ((`R.runReaderT` r) . hdl) ctx other
    where
    lower = hdl . (<$ ctx)
