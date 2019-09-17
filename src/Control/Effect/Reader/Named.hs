{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
module Control.Effect.Reader.Named
( -- * Reader effect
  Reader(..)
, ask
, asks
, local
) where

import Control.Algebra
import Control.Effect.Sum.Named
import Control.Effect.Reader (Reader(..))

-- | Retrieve the environment value.
--
--   prop> run (runReader a ask) === a
ask :: forall name r m sig . (NamedMember name (Reader r) sig, Algebra sig m) => m r
ask = sendNamed @name (Ask pure)

-- | Project a function out of the current environment value.
--
--   prop> snd (run (runReader a (asks (applyFun f)))) === applyFun f a
asks :: forall name r m a sig . (NamedMember name (Reader r) sig, Algebra sig m) => (r -> a) -> m a
asks f = sendNamed @name (Ask (pure . f))

-- | Run a computation with an environment value locally modified by the passed function.
--
--   prop> run (runReader a (local (applyFun f) ask)) === applyFun f a
--   prop> run (runReader a ((,,) <$> ask <*> local (applyFun f) ask <*> ask)) === (a, applyFun f a, a)
local :: forall name r m a sig . (NamedMember name (Reader r) sig, Algebra sig m) => (r -> r) -> m a -> m a
local f m = sendNamed @name (Local f m pure)
