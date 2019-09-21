{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Control.Algebra.Interpret
( runInterpret
, runInterpretState
, InterpretC(..)
, Reifies
, Handler
  -- * Re-exports
, Handles
, run
) where

import Control.Algebra
import Control.Algebra.State.Strict
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Unsafe.Coerce (unsafeCoerce)


-- | A @Handler@ is a function that interprets effects described by @sig@ into the Algebra monad @m@.
newtype Handler sig m =
  Handler { runHandler :: forall s x . sig (InterpretC s sig m) x -> InterpretC s sig m x }


newtype Tagged a b =
  Tagged { unTag :: b }


class Reifies s a | s -> a where
  reflect :: Tagged s a


data Skolem


-- | @Magic@ captures the GHC implementation detail of how single method type classes are implemented.
newtype Magic a r =
  Magic (Reifies Skolem a => Tagged Skolem r)


-- For more information on this technique, see the @reflection@ library. We use the formulation described in https://github.com/ekmett/reflection/issues/31 for better inlining.
--
-- Essentially we can view @k@ as internally a function of type @Reifies s a -> Tagged s r@, whch we can again view as just @a -> Tagged s r@ through @unsafeCoerce@. After this coercion, we just apply the function to @a@.
reify :: forall a r . a -> (forall s . Reifies s a => Tagged s r) -> r
reify a k =
  unsafeCoerce (Magic @a k) a


-- | Interpret an effect using a higher-order function.
--
-- Note that due to the higher-rank type, you have to use either '$' or explicit application when applying this interpreter. That is, you will need to write @runInterpret f (runInterpret g myPrgram)@ or @runInterpret f $ runInterpret g $ myProgram@. If you try and write @runInterpret f . runInterpret g@, you will unfortunately get a rather scary type error!
--
--   prop> run (runInterpret (\ op -> case op of { Get k -> k a ; Put _ k -> k }) get) === a
runInterpret
  :: forall alg m a.
     (HFunctor alg, Monad m)
  => (forall x . alg m x -> m x)
  -> (forall s . Reifies s (Handler alg m) => InterpretC s alg m a)
  -> m a
runInterpret f m =
  reify (Handler handler) (go m)

  where

    handler :: forall s x . alg (InterpretC s alg m) x -> InterpretC s alg m x
    handler e =
      InterpretC (f (handleCoercible e))

    go
      :: forall x s .
         InterpretC s alg m x
      -> Tagged s (m x)
    go m =
      Tagged (runInterpretC m)


-- | Interpret an effect using a higher-order function with some state variable.
--
--   prop> run (runInterpretState (\ s op -> case op of { Get k -> runState s (k s) ; Put s' k -> runState s' k }) a get) === a
runInterpretState
  :: (HFunctor alg, Monad m)
  => (forall x . s -> alg (StateC s m) x -> m (s, x))
  -> s
  -> (forall t. Reifies t (Handler alg (StateC s m)) => InterpretC t alg (StateC s m) a)
  -> m (s, a)
runInterpretState handler state m =
  runState state $
  runInterpret
    (\e -> StateC (\s -> handler s e))
    m


newtype InterpretC s (sig :: (* -> *) -> * -> *) m a =
  InterpretC { runInterpretC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)


instance MonadTrans (InterpretC s sig) where
  lift = InterpretC


instance (HFunctor alg, Reifies s (Handler alg m), Monad m, Algebra m) => Algebra (InterpretC s alg m) where
  type Signature (InterpretC s alg m) = alg :+: Signature m
  alg (L alg) =
    runHandler (unTag (reflect @s)) alg
  alg (R other) =
    InterpretC (alg (handleCoercible other))


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Effect.State
