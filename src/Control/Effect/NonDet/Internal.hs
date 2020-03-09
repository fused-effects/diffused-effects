{-# LANGUAGE TypeOperators #-}
module Control.Effect.NonDet.Internal
( NonDet
) where

import Control.Effect.Choose.Internal (Choose)
import Control.Effect.Empty.Internal (Empty)
import Control.Effect.Sum

type NonDet = Empty :+: Choose
