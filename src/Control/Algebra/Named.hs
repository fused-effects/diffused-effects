{-# LANGUAGE PolyKinds #-}
module Control.Algebra.Named
( NamedC(..)
) where

newtype NamedC (name :: k) m a = NamedC { runNamed :: m a }
