module Effect.State.Internal
( State(..)
) where

data State s m k
  = Get (s -> m k)
  | Put s (m k)
