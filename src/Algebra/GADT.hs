{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.GADT
( Algebra(..)
, send
) where

import Data.Functor.Identity
import Data.Kind (Type)
import Effect.GADT
import Effect.Sum

class Monad m => Algebra m where
  type Sig m :: (Type -> Type) -> (Type -> Type)

  alg :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> ctx () -> Sig m n a -> m (ctx a)


send :: (Member eff sig, sig ~ Sig m, Algebra m) => eff m a -> m a
send = fmap runIdentity . alg (fmap Identity . runIdentity) (Identity ()) . inj
{-# INLINE send #-}

instance Algebra (Either e) where
  type Sig (Either e) = Error e

  alg hdl ctx = \case
    Throw e   -> Left e
    Catch m h -> either (init . h) pure (init m)
    where
    init = hdl . (<$ ctx)
  {-# INLINE alg #-}
