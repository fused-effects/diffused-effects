{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.GADT
( Algebra(..)
) where

import Data.Kind (Type)

class Monad m => Algebra m where
  type Sig m :: (Type -> Type) -> (Type -> Type)

  alg :: Functor ctx => (forall x . ctx (n x) -> m (ctx x)) -> ctx () -> Sig m n a -> m (ctx a)
