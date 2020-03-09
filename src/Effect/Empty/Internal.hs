{-# LANGUAGE KindSignatures #-}
module Effect.Empty.Internal
( Empty(..)
) where

data Empty (m :: * -> *) k = Empty
