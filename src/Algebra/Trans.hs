{-# LANGUAGE TypeFamilies #-}
module Algebra.Trans
( AlgebraTrans(..)
) where

import Control.Monad.Trans.Class

class MonadTrans t => AlgebraTrans t where
  type SigT t :: (* -> *) -> (* -> *)
