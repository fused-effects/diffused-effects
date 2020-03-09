{-# LANGUAGE TypeOperators #-}
module Effect.Error.Internal
( Error
) where

import Effect.Catch.Internal (Catch)
import Effect.Sum ((:+:))
import Effect.Throw.Internal (Throw)

type Error e = Throw e :+: Catch e
