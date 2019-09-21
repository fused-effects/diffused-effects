{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Algebra.Error.Either
( -- * Error effect
  module Control.Effect.Error
  -- * Error carrier
, runError
, ErrorC(..)
  -- * Re-exports
, Algebra
, Member
, run
) where

import Control.Algebra
import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Error
import Control.Monad (MonadPlus(..), (<=<))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
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

instance Alternative m => Alternative (ErrorC e m) where
  empty = ErrorC empty
  {-# INLINE empty #-}
  ErrorC l <|> ErrorC r = ErrorC (l <|> r)
  {-# INLINE (<|>) #-}

instance Monad m => Monad (ErrorC e m) where
  ErrorC a >>= f = ErrorC (a >>= either (pure . Left) (runError . f))
  {-# INLINE (>>=) #-}

instance MonadFix m => MonadFix (ErrorC e m) where
  mfix f = ErrorC (mfix (runError . either (error "mfix (ErrorC): function returned failure") f))
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ErrorC e m) where
  liftIO io = ErrorC (Right <$> liftIO io)
  {-# INLINE liftIO #-}

instance Fail.MonadFail m => Fail.MonadFail (ErrorC e m) where
  fail s = ErrorC (Fail.fail s)
  {-# INLINE fail #-}

instance (Alternative m, Monad m) => MonadPlus (ErrorC e m)

instance MonadTrans (ErrorC e) where
  lift = ErrorC . fmap Right
  {-# INLINE lift #-}

instance (Algebra m, Effect (Signature m)) => Algebra (ErrorC e m) where
  type Signature (ErrorC e m) = Error e :+: Signature m
  alg (L (Throw e))     = ErrorC (pure (Left e))
  alg (L (Catch m h k)) = ErrorC (runError m >>= either (either (pure . Left) (runError . k) <=< runError . h) (runError . k))
  alg (R other)         = ErrorC (alg (handle (Right ()) (either (pure . Left) runError) other))
  {-# INLINE alg #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
