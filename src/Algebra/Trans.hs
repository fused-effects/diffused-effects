{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.Trans
( AlgebraTrans(..)
) where

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S.S
import           Data.Kind (Type)
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

instance AlgebraTrans (R.ReaderT r) where
  type SigT (R.ReaderT r) = Reader r

  algT = \case
    Ask       k -> R.ask       >>= k
    Local f m k -> R.local f m >>= k

instance AlgebraTrans (E.ExceptT e) where
  type SigT (E.ExceptT e) = Error e

  algT = \case
    L (Throw e)     -> E.throwE e
    R (Catch m h k) -> E.catchE m h >>= k

instance AlgebraTrans (S.S.StateT s) where
  type SigT (S.S.StateT s) = State s

  algT = \case
    Get   k -> S.S.get   >>= k
    Put s k -> S.S.put s >>  k
