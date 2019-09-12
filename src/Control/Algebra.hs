{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances #-}
module Control.Algebra
( Effects(..)
) where

import qualified Control.Applicative as Alternative
import           Control.Algebra.Class
import qualified Control.Effect.Choose as Effect
import qualified Control.Effect.Empty as Effect
import qualified Control.Effect.Fail as Effect
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Trans.Class

newtype Effects m a = Effects { runEffects :: m a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans Effects where
  lift = Effects

instance (Algebra sig m, Member Effect.Fail sig) => Fail.MonadFail (Effects m) where
  fail = lift . Effect.fail

instance (Algebra sig m, Member Effect.Choose sig, Member Effect.Empty sig) => Alternative.Alternative (Effects m) where
  empty = lift Effect.empty
  Effects l <|> Effects r = Effects (l Effect.<|> r)
