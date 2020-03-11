{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.GADT
( Algebra(..)
, send
, lowering
) where

import Data.Functor.Identity
import Data.Kind (Type)
import Effect.Sum

class Monad m => Algebra m where
  type Sig m :: (Type -> Type) -> (Type -> Type)

  alg :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> ctx () -> Sig m n a -> m (ctx a)


send :: (Member eff sig, sig ~ Sig m, Algebra m) => eff m a -> m a
send = fmap runIdentity . alg (fmap Identity . runIdentity) (Identity ()) . inj
{-# INLINE send #-}

lowering
  :: Functor ctx
  => ((forall x . m x -> n (ctx x)) -> (forall x y . (x -> m y) -> ctx x -> n (ctx y)) -> a)
  -> (forall x . ctx (m x) -> n (ctx x))
  -> ctx ()
  -> a
lowering with hdl ctx = with (hdl . (<$ ctx)) (\ k -> hdl . fmap k)
{-# INLINE lowering #-}
