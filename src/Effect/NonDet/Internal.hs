{-# LANGUAGE TypeOperators #-}
module Effect.NonDet.Internal
( NonDet
) where

import Effect.Choose.Internal (Choose)
import Effect.Empty.Internal (Empty)
import Effect.Sum

type NonDet = Empty :+: Choose
