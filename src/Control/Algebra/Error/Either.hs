{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Error.Either
( -- * Error effect
  module Control.Effect.Error
  -- * Error Algebra
, runError
, ErrorC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Applicative (liftA2)
import Control.Algebra.Class
import Control.Effect.Error
import Control.Monad ((<=<))
import Control.Monad.Trans.Class

-- | Run an 'Error' effect, returning uncaught errors in 'Left' and successful computations’ values in 'Right'.
--
--   prop> run (runError (pure a)) === Right @Int @Int a
runError :: ErrorC exc m a -> m (Either exc a)
runError = runErrorC

newtype ErrorC e m a = ErrorC { runErrorC :: m (Either e a) }
  deriving (Functor)

instance Applicative m => Applicative (ErrorC e m) where
  pure a = ErrorC (pure (Right a))
  {-# INLINE pure #-}
  ErrorC f <*> ErrorC a = ErrorC (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC (a >>= either (pure . Left) (runError . f))
  {-# INLINE (>>=) #-}

instance MonadTrans (ErrorC e) where
  lift = ErrorC . fmap Right
  {-# INLINE lift #-}

instance (Algebra sig m, Effect sig) => Algebra (Error e :+: sig) (ErrorC e m) where
  alg (L (Throw e))     = ErrorC (pure (Left e))
  alg (L (Catch m h k)) = ErrorC (runError m >>= either (either (pure . Left) (runError . k) <=< runError . h) (runError . k))
  alg (R other)         = ErrorC (alg (handle (Right ()) (either (pure . Left) runError) other))
  {-# INLINE alg #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
