{-# LANGUAGE TypeOperators #-}
module Control.Effect.NonDet
( -- * NonDet effects
  module Control.Effect.Choose
, module Control.Effect.Empty
, NonDet
) where

import Control.Effect.Choose
import Control.Effect.Empty
import Control.Effect.Sum

type NonDet = Choose :+: Empty
