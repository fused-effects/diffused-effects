{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Trans.Lower
( -- * Lowering monad transformer
  runLowerT
, LowerT(..)
, lowerWith
, lower
, lowerCont
, mapLowerT
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
, Handler(..)
) where

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import           Data.Functor.Compose
import           Data.Functor.Identity

runLowerT :: Handler ctx m n -> ctx () -> LowerT ctx m n a -> n a
runLowerT hdl ctx (LowerT m) = m hdl ctx

newtype LowerT ctx m n a = LowerT (Handler ctx m n -> ctx () -> n a)
  deriving (Applicative, Functor, Monad) via R.ReaderT (Handler ctx m n) (R.ReaderT (ctx ()) n)

instance MonadTrans (LowerT ctx m) where
  lift = LowerT . const . const

lowerWith :: Functor ctx => ((forall a . m a -> n (ctx a)) -> n b) -> LowerT ctx m n b
lowerWith with = LowerT $ \ hdl ctx -> with (appDist hdl . (<$ ctx))

lower :: Functor ctx => m a -> LowerT ctx m n (ctx a)
lower m = lowerWith ($ m)

lowerCont :: Functor ctx => (a -> m b) -> ctx a -> LowerT ctx m n (ctx b)
lowerCont k ctx = LowerT $ const . runDist (k <$> ctx)

mapLowerT :: (n' a -> n b) -> (Handler ctx m n -> Handler ctx' m n') -> (ctx () -> ctx' ()) -> LowerT ctx' m n' a -> LowerT ctx m n b
mapLowerT f g h (LowerT m) = LowerT $ \ hdl ctx -> f (m (g hdl) (h ctx))


type Hom m n = forall x . m x -> n x


runDist :: ctx (m a) -> Handler ctx m n -> n (ctx a)
runDist cm (Handler run) = run cm

hom :: Functor n => Hom m n -> Handler Identity m n
hom hom = Handler (fmap Identity . hom . runIdentity)

(>~>) :: (Functor n, Functor ctx2) => Handler ctx1 l m -> Handler ctx2 m n -> Handler (Compose ctx2 ctx1) l n
Handler hdl1 >~> Handler hdl2 = Handler (fmap Compose . hdl2 . fmap hdl1 . getCompose)

infixr 1 >~>

(<~<) :: (Functor n, Functor ctx2) => Handler ctx2 m n -> Handler ctx1 l m -> Handler (Compose ctx2 ctx1) l n
Handler hdl1 <~< Handler hdl2 = Handler (fmap Compose . hdl1 . fmap hdl2 . getCompose)

infixr 1 <~<

(~>) :: Functor ctx => Hom l m -> Handler ctx m n -> Handler ctx l n
hdl1 ~> Handler hdl2 = Handler (hdl2 . fmap hdl1)

infixr 1 ~>

(<~) :: Functor ctx => Handler ctx m n -> Hom l m -> Handler ctx l n
Handler hdl1 <~ hdl2 = Handler (hdl1 . fmap hdl2)

infixr 1 <~

(>~) :: Handler ctx l m -> Hom m n -> Handler ctx l n
Handler hdl1 >~ hdl2 = Handler (hdl2 . hdl1)

infixr 1 >~

(~<) :: Hom m n -> Handler ctx l m -> Handler ctx l n
hdl1 ~< Handler hdl2 = Handler (hdl1 . hdl2)

infixr 1 ~<

newtype Handler ctx m n = Handler { appDist :: forall x . ctx (m x) -> n (ctx x) }
