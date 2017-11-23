module Data.Functor.Contravariant where

import Prelude hiding (Functor, (.), id)

import Control.Applicative
import Control.Applicative.Backwards
import Control.Arrow
import Control.Category
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Function (on)
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum
import Data.Monoid (Alt (..))
import Data.Proxy

class Functor f where
    gmap :: (a -> b) -> f b -> f a
    (>$) :: b -> f b -> f a
    (>$) = gmap . const

(>$<) :: Functor f => (a -> b) -> f b -> f a
(>$<) = gmap

instance Functor (Op1 a) where gmap f (Op1 g) = Op1 (g . f)
instance Functor (Op2 a) where gmap f (Op2 g) = Op2 (g `on` f)
instance Functor (Const a) where gmap _ (Const a) = Const a
instance Functor Proxy where gmap _ Proxy = Proxy
instance (Functor f, Functor g) => Functor (Product f g) where
    gmap f (Pair x y) = Pair (f >$< x) (f >$< y)
instance (Functor f, Functor g) => Functor (Sum f g) where
    gmap f (InL x) = InL (f >$< x)
    gmap f (InR y) = InR (f >$< y)
deriving instance Functor f => Functor (Reverse f)
deriving instance Functor f => Functor (Backwards f)
deriving instance Functor f => Functor (Alt f)
instance Functor f => Functor (ExceptT e f) where gmap f = ExceptT . gmap (fmap f) . runExceptT
instance Functor f => Functor (ReaderT r f) where gmap f = ReaderT . fmap (gmap f) . runReaderT
instance Functor f => Functor (StateT  s f) where
    gmap f = StateT  . (fmap . gmap) (f *** id) . runStateT
instance Functor f => Functor (WriterT w f) where gmap f = WriterT . gmap (f *** id) . runWriterT

newtype Op1 b a = Op1 { op1 :: a -> b }
newtype Op2 b a = Op2 { op2 :: a -> a -> b }

instance Category Op1 where
    id = Op1 id
    Op1 f . Op1 g = Op1 (g . f)

instance Semigroup a => Semigroup (Op1 a b) where
    Op1 f <> Op1 g = Op1 (liftA2 (<>) f g)

instance Monoid a => Monoid (Op1 a b) where
    mempty = Op1 (pure mempty)

instance Semigroup a => Semigroup (Op2 a b) where
    Op2 f <> Op2 g = Op2 ((liftA2 . liftA2) (<>) f g)

instance Monoid a => Monoid (Op2 a b) where
    mempty = Op2 ((pure . pure) mempty)
