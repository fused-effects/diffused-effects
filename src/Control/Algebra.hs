{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances #-}
module Control.Algebra
( Effects(..)
) where

import           Control.Algebra.Class
import qualified Control.Effect.Fail as Effect
import qualified Control.Monad.Fail as Fail

newtype Effects m a = Effects { runEffects :: m a }
  deriving (Applicative, Functor, Monad)

instance (Algebra sig m, Member Effect.Fail sig) => Fail.MonadFail (Effects m) where
  fail = Effects . Effect.fail
