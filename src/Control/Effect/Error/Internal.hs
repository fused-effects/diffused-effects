{-# LANGUAGE TypeOperators #-}
module Control.Effect.Error.Internal
( Error
) where

import Control.Effect.Catch.Internal (Catch)
import Control.Effect.Sum ((:+:))
import Control.Effect.Throw.Internal (Throw)

type Error e = Throw e :+: Catch e
