{-# LANGUAGE GADTs #-}
module Effect.Reader.Internal
( Reader(..)
) where

data Reader r m k where
  Ask   ::                    Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a
