{-# LANGUAGE KindSignatures #-}
module Effect.Throw.Internal
( Throw(..)
) where

newtype Throw e (m :: * -> *) k = Throw e
