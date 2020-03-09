{-# LANGUAGE ExistentialQuantification #-}
module Effect.Reader.Internal
( Reader(..)
) where

data Reader r m k
  = Ask (r -> m k)
  | forall b . Local (r -> r) (m b) (b -> m k)
