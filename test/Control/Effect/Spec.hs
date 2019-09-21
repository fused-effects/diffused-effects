{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, MultiWayIf, TemplateHaskell, TypeApplications, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -O2 -fplugin Test.Inspection.Plugin #-}
module Control.Effect.Spec
( spec
) where

import Control.Effect.Algebra
import Control.Effect.Error
import Control.Effect.Fail
import Control.Effect.Reader
import Control.Effect.State
import Prelude hiding (fail)
import Test.Hspec
import Test.Inspection as Inspection

spec :: Spec
spec = do
  inference
  reinterpretation
  interposition
  fusion

inference :: Spec
inference = describe "inference" $ do
  it "can be wrapped for better type inference" $
    run (runHasEnv (runEnv "i" ((++) <$> askEnv <*> askEnv))) `shouldBe` "ii"

askEnv :: (Member (Reader env) sig, Algebra m) => HasEnv env m env
askEnv = ask

runEnv :: env -> HasEnv env (ReaderC env m) a -> HasEnv env m a
runEnv r = HasEnv . runReader r . runHasEnv


newtype HasEnv env m a = HasEnv { runHasEnv :: m a }
  deriving (Applicative, Functor, Monad)

instance Algebra m => Algebra sig (HasEnv env m) where
  alg = HasEnv . alg . handleCoercible


reinterpretation :: Spec
reinterpretation = describe "reinterpretation" $ do
  it "can reinterpret effects into other effects" $
    run (runState "a" ((++) <$> reinterpretReader (local ('b':) ask) <*> get)) `shouldBe` ("a", "baa")

reinterpretReader :: ReinterpretReaderC r m a -> StateC r m a
reinterpretReader = runReinterpretReaderC

newtype ReinterpretReaderC r m a = ReinterpretReaderC { runReinterpretReaderC :: StateC r m a }
  deriving (Applicative, Functor, Monad, MonadFail)

instance (Algebra m, Effect sig) => Algebra (Reader r :+: sig) (ReinterpretReaderC r m) where
  alg (L (Ask       k)) = ReinterpretReaderC get >>= k
  alg (L (Local f m k)) = do
    a <- ReinterpretReaderC get
    ReinterpretReaderC (put (f a))
    v <- m
    ReinterpretReaderC (put a)
    k v
  alg (R other)         = ReinterpretReaderC (alg (R (handleCoercible other)))


interposition :: Spec
interposition = describe "interposition" $ do
  it "can interpose handlers without changing the available effects" $
    run (runFail (interposeFail (fail "world"))) `shouldBe` (Left "hello, world" :: Either String Int)

  it "interposition only intercepts effects in its scope" $ do
    run (runFail (fail "world" *> interposeFail (pure (0 :: Int)))) `shouldBe` Left "world"
    run (runFail (interposeFail (pure (0 :: Int)) <* fail "world")) `shouldBe` Left "world"

interposeFail :: InterposeC m a -> m a
interposeFail = runInterposeC

newtype InterposeC m a = InterposeC { runInterposeC :: m a }
  deriving (Applicative, Functor, Monad)

instance (Algebra m, Member Fail sig) => MonadFail (InterposeC m) where
  fail s = send (Fail s)

instance (Algebra m, Member Fail sig) => Algebra sig (InterposeC m) where
  alg op
    | Just (Fail s) <- prj op = InterposeC (send (Fail ("hello, " ++ s)))
    | otherwise               = InterposeC (alg (handleCoercible op))


shouldSucceed :: Inspection.Result -> Expectation
shouldSucceed (Success _) = pure ()
shouldSucceed (Failure f) = expectationFailure f

fusion :: Spec
fusion = describe "fusion" $ do
  it "eliminates StateCs" $ do
    shouldSucceed $(inspectTest $ 'countDown `doesNotUse` ''StateC)

  it "eliminates nested StateCs" $ do
    shouldSucceed $(inspectTest $ 'countBoth `doesNotUse` ''StateC)

  it "eliminates catch and throw" $ do
    shouldSucceed $(inspectTest $ 'throwing `doesNotUse` ''ErrorC)

  it "eliminates calls to alg" $ do
    shouldSucceed $(inspectTest $ 'countDown `doesNotUse` 'alg)

countDown :: Int -> (Int, Int)
countDown start = run . runState start $ go
  where go = get >>= \n -> if n <= 0 then pure n else modify @Int pred *> go

countBoth :: Int -> (Int, (Float, ()))
countBoth n = run . runState n . runState (fromIntegral n) $ go where
  go = do
    n <- get @Int
    if
      | n == 0         -> pure ()
      | n `mod` 2 == 0 -> modify @Float (+ 1) *> modify @Int pred *> go
      | otherwise      -> modify @Int pred    *> go

throwing :: Int -> Either Int String
throwing n = run $ runError go
  where go = if n > 10 then throwError @Int 42 else pure "fine"
