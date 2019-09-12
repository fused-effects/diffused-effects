{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.Choose
( -- * Choose effect
  Choose(..)
, choose
, optional
, many
, some
, some1
, oneOf
-- * Choosing semigroup
, Choosing(..)
) where

import Control.Carrier.Class
import Control.Effect.Empty
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic1)

data Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance HFunctor Choose
instance Effect   Choose

-- | Nondeterministically choose between two computations.
choose :: (Carrier sig m, Member Choose sig) => m a -> m a -> m a
choose a b = send (Choose (bool b a))

-- | Select between 'Just' the result of an operation, and 'Nothing'.
optional :: (Carrier sig m, Member Choose sig) => m a -> m (Maybe a)
optional a = choose (Just <$> a) (pure Nothing)

-- | Zero or more.
many :: (Carrier sig m, Member Choose sig) => m a -> m [a]
many a = go where go = choose ((:) <$> a <*> go) (pure [])

-- | One or more.
some :: (Carrier sig m, Member Choose sig) => m a -> m [a]
some a = (:) <$> a <*> many a

-- | One or more, returning a 'NonEmpty' list of the results.
some1 :: (Carrier sig m, Member Choose sig) => m a -> m (NonEmpty a)
some1 a = (:|) <$> a <*> many a


-- | Nondeterministically choose an element from a 'Foldable' collection.
-- This can be used to emulate the style of nondeterminism associated with
-- programming in the list monad:
-- @
--   pythagoreanTriples = do
--     a <- oneOf [1..10]
--     b <- oneOf [1..10]
--     c <- oneOf [1..10]
--     guard (a^2 + b^2 == c^2)
--     pure (a, b, c)
-- @
oneOf :: (Foldable t, Carrier sig m, Member Choose sig, Member Empty sig) => t a -> m a
oneOf = getChoosing . foldMap (Choosing . pure)

newtype Choosing m a = Choosing { getChoosing :: m a }

instance (Carrier sig m, Member Choose sig) => Semigroup (Choosing m a) where
  Choosing m1 <> Choosing m2 = Choosing (choose m1 m2)

instance (Carrier sig m, Member Choose sig, Member Empty sig) => Monoid (Choosing m a) where
  mempty = Choosing (send Empty)
