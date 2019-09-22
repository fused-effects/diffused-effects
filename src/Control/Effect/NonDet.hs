{-# LANGUAGE TypeOperators #-}
module Control.Effect.NonDet
( -- * NonDet effects
  module Control.Effect.Choose
, module Control.Effect.Empty
, NonDet
, Alternative(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Choose hiding (many, some)
import Control.Effect.Empty hiding (empty)
import Control.Effect.Sum

type NonDet = Empty :+: Choose
