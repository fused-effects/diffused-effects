{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
module Control.Effect.Choose
( -- * Choose effect
  Choose(..)
, (<|>)
, optional
, many
, some
, some1
, oneOf
-- * Choosing semigroup
, Choosing(..)
) where

import Control.Algebra
import Control.Effect.Empty
import Data.Bool (bool)
import Data.Coerce
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic1)

data Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance HFunctor Choose
instance Effect   Choose

-- | Nondeterministically choose between two computations.
(<|>) :: Has Choose m => m a -> m a -> m a
(<|>) a b = send (Choose (bool b a))

infixl 3 <|>

-- | Select between 'Just' the result of an operation, and 'Nothing'.
optional :: Has Choose m => m a -> m (Maybe a)
optional a = Just <$> a <|> pure Nothing

-- | Zero or more.
many :: Has Choose m => m a -> m [a]
many a = go where go = (:) <$> a <*> go <|> pure []

-- | One or more.
some :: Has Choose m => m a -> m [a]
some a = (:) <$> a <*> many a

-- | One or more, returning a 'NonEmpty' list of the results.
some1 :: Has Choose m => m a -> m (NonEmpty a)
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
oneOf :: (Foldable t, Has Choose m, Has Empty m) => t a -> m a
oneOf = getChoosing #. foldMap (Choosing #. pure)

newtype Choosing m a = Choosing { getChoosing :: m a }

instance Has Choose m => Semigroup (Choosing m a) where
  Choosing m1 <> Choosing m2 = Choosing (m1 <|> m2)

instance (Has Choose m, Has Empty m) => Monoid (Choosing m a) where
  mempty = Choosing (send Empty)


(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}
