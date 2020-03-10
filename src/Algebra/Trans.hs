{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.Trans
( AlgebraTrans(..)
) where

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import           Effect.Reader.Internal

class MonadTrans t => AlgebraTrans t where
  type SigT t :: (* -> *) -> (* -> *)

  algT :: Monad m => SigT t (t m) a -> t m a

instance AlgebraTrans (R.ReaderT r) where
  type SigT (R.ReaderT r) = Reader r

  algT = \case
    Ask       k -> R.ask       >>= k
    Local f m k -> R.local f m >>= k
