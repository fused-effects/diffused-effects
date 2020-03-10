{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.Trans
( AlgebraTrans(..)
) where

import Control.Monad.Trans.Class

class MonadTrans t => AlgebraTrans t where
  type SigT t :: (* -> *) -> (* -> *)

  algT :: (Functor ctx, Monad m) => ctx () -> (forall x . ctx (n x) -> t m (ctx x)) -> SigT t n a -> t m (ctx a)
