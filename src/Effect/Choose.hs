{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Effect.Choose
( -- * Choose effect
  Choose(..)
, (<|>)
, optional
, many
, some
, some1
  -- * Choosing semigroup
, Choosing(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Algebra
import           Effect.Choose.Internal (Choose(..))
import           Effect.Empty
import           Data.Bool (bool)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Semigroup as S

(<|>) :: Has Choose m => m a -> m a -> m a
(<|>) a b = send (Choose (bool b a))

infixl 3 <|>

optional :: Has Choose m => m a -> m (Maybe a)
optional a = Just <$> a <|> pure Nothing

many :: Has Choose m => m a -> m [a]
many a = go where go = (:) <$> a <*> go <|> pure []

some :: Has Choose m => m a -> m [a]
some a = (:) <$> a <*> many a

some1 :: Has Choose m => m a -> m (NonEmpty a)
some1 a = (:|) <$> a <*> many a


newtype Choosing m a = Choosing { getChoosing :: m a }

instance Has Choose m => S.Semigroup (Choosing m a) where
  Choosing m1 <> Choosing m2 = Choosing (m1 <|> m2)

instance (Has Choose m, Has Empty m) => Monoid (Choosing m a) where
  mempty = Choosing empty
  mappend = (S.<>)
