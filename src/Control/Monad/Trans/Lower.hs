{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.Trans.Lower
( -- * Lowering monad transformer
  runLowerT
, lowerT
, LowerT
, lowerWith
, lower
, lowerCont
, mapLowerT
  -- * Pure handlers
, PureHandler
, pureHandler
  -- * Handlers
, Handler
, (>~>)
, (<~<)
, (~>)
, (<~)
, (>~)
, (~<)
  -- * Wrapped handlers
, runHandler
, WrappedHandler(..)
) where

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import           Data.Functor.Compose
import           Data.Functor.Identity

runLowerT :: Handler ctx m n -> LowerT ctx m n a -> n a
runLowerT hdl (LowerT m) = m (WrappedHandler hdl)

lowerT :: (Handler ctx m n -> n a) -> LowerT ctx m n a
lowerT f = LowerT $ \ hdl -> f (unwrapHandler hdl)

newtype LowerT ctx m n a = LowerT (WrappedHandler ctx m n -> n a)
  deriving (Applicative, Functor, Monad) via R.ReaderT (WrappedHandler ctx m n) n

instance MonadTrans (LowerT ctx m) where
  lift = LowerT . const

lowerWith :: Functor ctx => ((forall a . m a -> n (ctx a)) -> n b) -> ctx () -> LowerT ctx m n b
lowerWith with ctx = lowerT $ \ hdl -> with (hdl . (<$ ctx))

lower :: Functor ctx => m a -> ctx () -> LowerT ctx m n (ctx a)
lower m = lowerWith ($ m)

lowerCont :: Functor ctx => (a -> m b) -> ctx a -> LowerT ctx m n (ctx b)
lowerCont k ctx = LowerT $ runHandler (k <$> ctx)

mapLowerT :: (n' a -> n b) -> (Handler ctx m n -> Handler ctx m n') -> LowerT ctx m n' a -> LowerT ctx m n b
mapLowerT f g m = lowerT $ \ hdl -> f (runLowerT (g hdl) m)


type PureHandler m n = forall x . m x -> n x

pureHandler :: Functor n => PureHandler m n -> Handler Identity m n
pureHandler hom = fmap Identity . hom . runIdentity


type Handler ctx m n = forall x . ctx (m x) -> n (ctx x)

(>~>) :: (Functor n, Functor ctx2) => Handler ctx1 l m -> Handler ctx2 m n -> Handler (Compose ctx2 ctx1) l n
hdl1 >~> hdl2 = fmap Compose . hdl2 . fmap hdl1 . getCompose

infixr 1 >~>

(<~<) :: (Functor n, Functor ctx2) => Handler ctx2 m n -> Handler ctx1 l m -> Handler (Compose ctx2 ctx1) l n
hdl1 <~< hdl2 = fmap Compose . hdl1 . fmap hdl2 . getCompose

infixr 1 <~<

(~>) :: Functor ctx => PureHandler l m -> Handler ctx m n -> Handler ctx l n
hdl1 ~> hdl2 = hdl2 . fmap hdl1

infixr 1 ~>

(<~) :: Functor ctx => Handler ctx m n -> PureHandler l m -> Handler ctx l n
hdl1 <~ hdl2 = hdl1 . fmap hdl2

infixr 1 <~

(>~) :: Handler ctx l m -> PureHandler m n -> Handler ctx l n
hdl1 >~ hdl2 = hdl2 . hdl1

infixr 1 >~

(~<) :: PureHandler m n -> Handler ctx l m -> Handler ctx l n
hdl1 ~< hdl2 = hdl1 . hdl2

infixr 1 ~<


runHandler :: ctx (m a) -> WrappedHandler ctx m n -> n (ctx a)
runHandler cm (WrappedHandler run) = run cm

newtype WrappedHandler ctx m n = WrappedHandler { unwrapHandler :: Handler ctx m n }
