{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Teletype
( spec
, runTeletypeIO
) where

import Prelude hiding (read)

import Control.Effect.Algebra
import Control.Effect.State
import Control.Effect.Writer
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

read :: (Member Teletype sig, Algebra sig m) => m String
read = send (Read pure)

write :: (Member Teletype sig, Algebra sig m) => String -> m ()
write s = send (Write s (pure ()))


runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
  alg (L (Read    k)) = liftIO getLine      >>= k
  alg (L (Write s k)) = liftIO (putStrLn s) >>  k
  alg (R other)       = TeletypeIOC (alg (handleCoercible other))


runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: StateC [String] (WriterC [String] m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (Algebra sig m, Effect sig) => Algebra (Teletype :+: sig) (TeletypeRetC m) where
  alg (L (Read    k)) = do
    i <- TeletypeRetC get
    case i of
      []  -> k ""
      h:t -> TeletypeRetC (put t) *> k h
  alg (L (Write s k)) = TeletypeRetC (tell [s]) *> k
  alg (R other)       = TeletypeRetC (alg (R (R (handleCoercible other))))
