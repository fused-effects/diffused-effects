module Effect.Error
( -- * Error effects
  Error
, module Effect.Throw
, module Effect.Catch
) where

import Effect.Catch
import Effect.Error.Internal (Error)
import Effect.Throw
