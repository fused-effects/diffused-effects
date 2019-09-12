{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Algebra
( Effects(..)
) where

newtype Effects m a = Effects { runEffects :: m a }
  deriving (Applicative, Functor, Monad)
