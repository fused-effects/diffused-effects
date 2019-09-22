{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Teletype
( spec
, runTeletypeIO
) where

import Prelude hiding (read)

import Control.Algebra
import Control.Algebra.State.Strict
import Control.Algebra.Writer.Strict
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "teletype" $ do
  prop "reads" $
    \ line -> run (runTeletypeRet [line] read) `shouldBe` ([], ([], line))

  prop "writes" $
    \ input output -> run (runTeletypeRet input (write output)) `shouldBe` ([output], (input, ()))

  prop "writes multiple things" $
    \ input output1 output2 -> run (runTeletypeRet input (write output1 >> write output2)) `shouldBe` ([output1, output2], (input, ()))

data Teletype m k
  = Read (String -> m k)
  | Write String (m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

read :: Has Teletype m => m String
read = send (Read pure)

write :: Has Teletype m => String -> m ()
write s = send (Write s (pure ()))


runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra m) => Algebra (TeletypeIOC m) where
  type Signature (TeletypeIOC m) = Teletype :+: Signature m
  alg (L (Read    k)) = liftIO getLine      >>= k
  alg (L (Write s k)) = liftIO (putStrLn s) >>  k
  alg (R other)       = TeletypeIOC (alg (handleCoercible other))


runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: StateC [String] (WriterC [String] m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (Algebra m, Effect (Signature m)) => Algebra (TeletypeRetC m) where
  type Signature (TeletypeRetC m) = Teletype :+: Signature m
  alg (L (Read    k)) = do
    i <- TeletypeRetC get
    case i of
      []  -> k ""
      h:t -> TeletypeRetC (put t) *> k h
  alg (L (Write s k)) = TeletypeRetC (tell [s]) *> k
  alg (R other)       = TeletypeRetC (alg (R (R (handleCoercible other))))
