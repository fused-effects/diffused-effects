{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.Trans
( AlgebraTrans(..)
) where

import qualified Algebra as A
import           Control.Monad.Trans.Class

class MonadTrans t => AlgebraTrans t where
  type SigT t :: (* -> *) -> (* -> *)

  algT :: A.Algebra m => SigT t (t m) a -> t m a
