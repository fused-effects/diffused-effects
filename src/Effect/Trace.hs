{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts #-}
module Effect.Trace
( -- * Trace effect
  Trace(..)
, trace
) where

import Algebra
import GHC.Generics (Generic1)

data Trace m k = Trace
  { traceMessage :: String
  , traceCont    :: m k
  }
  deriving (Functor, Generic1)

instance HFunctor Trace
instance Effect   Trace

-- | Append a message to the trace log.
trace :: Has Trace m => String -> m ()
trace message = send (Trace message (pure ()))
