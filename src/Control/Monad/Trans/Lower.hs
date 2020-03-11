{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Trans.Lower
( -- * Lowering monad transformer
  runLowerT
, LowerT(..)
, initial
, lowerCont
, mapLowerT
, liftInitial
  -- * Distributive laws
, Hom
, runDist
, hom
, (>~>)
, (<~<)
, (~>)
, (<~)
, (>~)
, (~<)
, Dist(..)
) where

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import           Data.Functor.Compose
import           Data.Functor.Identity

runLowerT :: Dist ctx m n -> ctx () -> LowerT ctx m n a -> n a
runLowerT hdl ctx (LowerT m) = m hdl ctx

newtype LowerT ctx m n a = LowerT (Dist ctx m n -> ctx () -> n a)
  deriving (Applicative, Functor, Monad) via R.ReaderT (Dist ctx m n) (R.ReaderT (ctx ()) n)

instance MonadTrans (LowerT ctx m) where
  lift = LowerT . const . const

initial :: Functor ctx => m a -> LowerT ctx m n (ctx a)
initial m = liftInitial ($ m)

lowerCont :: Functor ctx => (a -> m b) -> ctx a -> LowerT ctx m n (ctx b)
lowerCont k ctx = LowerT $ const . runDist (k <$> ctx)

mapLowerT :: (n' a -> n b) -> (Dist ctx m n -> Dist ctx' m n') -> (ctx () -> ctx' ()) -> LowerT ctx' m n' a -> LowerT ctx m n b
mapLowerT f g h (LowerT m) = LowerT $ \ hdl ctx -> f (m (g hdl) (h ctx))

liftInitial :: Functor ctx => ((forall a . m a -> n (ctx a)) -> n b) -> LowerT ctx m n b
liftInitial with = LowerT $ \ hdl ctx -> with (appDist hdl . (<$ ctx))


type Hom m n = forall x . m x -> n x


runDist :: ctx (m a) -> Dist ctx m n -> n (ctx a)
runDist cm (Dist run) = run cm

hom :: Functor n => Hom m n -> Dist Identity m n
hom hom = Dist (fmap Identity . hom . runIdentity)

(>~>) :: (Functor n, Functor ctx2) => Dist ctx1 l m -> Dist ctx2 m n -> Dist (Compose ctx2 ctx1) l n
Dist hdl1 >~> Dist hdl2 = Dist (fmap Compose . hdl2 . fmap hdl1 . getCompose)

infixr 1 >~>

(<~<) :: (Functor n, Functor ctx2) => Dist ctx2 m n -> Dist ctx1 l m -> Dist (Compose ctx2 ctx1) l n
Dist hdl1 <~< Dist hdl2 = Dist (fmap Compose . hdl1 . fmap hdl2 . getCompose)

infixr 1 <~<

(~>) :: Functor ctx => Hom l m -> Dist ctx m n -> Dist ctx l n
hdl1 ~> Dist hdl2 = Dist (hdl2 . fmap hdl1)

infixr 1 ~>

(<~) :: Functor ctx => Dist ctx m n -> Hom l m -> Dist ctx l n
Dist hdl1 <~ hdl2 = Dist (hdl1 . fmap hdl2)

infixr 1 <~

(>~) :: Dist ctx l m -> Hom m n -> Dist ctx l n
Dist hdl1 >~ hdl2 = Dist (hdl2 . hdl1)

infixr 1 >~

(~<) :: Hom m n -> Dist ctx l m -> Dist ctx l n
hdl1 ~< Dist hdl2 = Dist (hdl1 . hdl2)

infixr 1 ~<

newtype Dist ctx m n = Dist { appDist :: forall x . ctx (m x) -> n (ctx x) }
