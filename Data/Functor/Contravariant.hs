module Data.Functor.Contravariant
  ( Functor (..), (>$<), phantom, Zip (..), Cozip (..), Op1 (..), Op2 (..) )
  where

import Prelude hiding (Functor, (.), id, zip, zipWith)
import qualified Prelude as Base

import Control.Applicative
import Control.Applicative.Backwards
import Control.Arrow
import Control.Category
import Control.Category.Dual (Dual (..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Function (on)
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum
import Data.Semigroup (Semigroup (..))
import Data.Monoid (Alt (..))
import Data.Profunctor (Profunctor (..))
import Data.Proxy
import Data.Void (Void)

infixl 4 >$
infixl 4 >$<
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
instance Functor f => Functor (MaybeT    f) where gmap f = MaybeT  . gmap (fmap f) . runMaybeT
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

instance (Semigroup a, Monoid a) => Monoid (Op1 a b) where
    mempty = Op1 (pure mempty)
    mappend = (<>)

instance Semigroup a => Semigroup (Op2 a b) where
    Op2 f <> Op2 g = Op2 ((liftA2 . liftA2) (<>) f g)

instance (Semigroup a, Monoid a) => Monoid (Op2 a b) where
    mempty = Op2 ((pure . pure) mempty)
    mappend = (<>)

phantom :: (Base.Functor f, Functor f) => f a -> f b
phantom = (() >$) . (() <$)

instance Profunctor p => Functor (Dual p a) where
    gmap f = Dual . lmap f . dual

infixr 3 >|<
class Functor f => Zip f where
    {-# MINIMAL (impure | unit), (zip | zipWith) #-}
    impure :: f a
    unit :: f ()
    zip :: f a -> f b -> f (a, b)
    zipWith :: (a -> (b, c)) -> f b -> f c -> f a
    (>|<) :: f a -> f a -> f a

    impure = () >$ unit
    unit = impure
    zip = zipWith id
    zipWith f x y = f >$< zip x y
    (>|<) = zipWith (\ a -> (a, a))

instance Monoid a => Zip (Op1 a) where
    impure = Op1 (pure mempty)
    zip (Op1 f) (Op1 g) = Op1 (\ (a, b) -> f a <> g b)

instance Monoid a => Zip (Op2 a) where
    impure = Op2 (\ _ _ -> mempty)
    zip (Op2 f) (Op2 g) = Op2 (\ (a₁, b₁) (a₂, b₂) -> f a₁ a₂ <> g b₁ b₂)

instance Monoid a => Zip (Dual (->) a) where
    impure = Dual (pure mempty)
    zip (Dual f) (Dual g) = Dual (\ (a, b) -> f a <> g b)

instance Monoid a => Zip (Const a) where
    impure = Const mempty
    zip (Const a) (Const b) = Const (a <> b)

instance (Zip f, Zip g) => Zip (Product f g) where
    impure = Pair impure impure
    zip (Pair x₁ y₁) (Pair x₂ y₂) = Pair (zip x₁ x₂) (zip y₁ y₂)

instance (Zip f) => Zip (ExceptT e f) where
    impure = ExceptT impure
    zip (ExceptT x) (ExceptT y) = ExceptT (zipWith (liftA2 (,) (id +++ fst) (id +++ snd)) x y)

instance (Zip f) => Zip (MaybeT f) where
    impure = MaybeT impure
    zip (MaybeT x) (MaybeT y) = MaybeT (zipWith (liftA2 (,) (fmap fst) (fmap snd)) x y)

instance (Zip f) => Zip (ReaderT r f) where
    impure = ReaderT (\ _ -> impure)
    zip (ReaderT x) (ReaderT y) = ReaderT (liftA2 zip x y)
    zipWith f (ReaderT x) (ReaderT y) = ReaderT (liftA2 (zipWith f) x y)

instance (Zip f) => Zip (StateT s f) where
    impure = StateT (\ _ -> impure)
    zip (StateT x) (StateT y) = StateT (liftA2 (zipWith separate) x y)

instance (Zip f) => Zip (WriterT w f) where
    impure = WriterT impure
    zip (WriterT x) (WriterT y) = WriterT (zipWith separate x y)

separate :: ((a, b), c) -> ((a, c), (b, c))
separate ((a, b), c) = ((a, c), (b, c))

class Functor f => Cozip f where
    {-# MINIMAL full, (cozip | cozipWith) #-}
    full :: f Void
    cozip :: f a -> f b -> f (Either a b)
    cozipWith :: (a -> Either b c) -> f b -> f c -> f a
    cozip = cozipWith id
    cozipWith f x y = f >$< (cozip x y)

instance Cozip (Op1 a) where
    full = Op1 (\ case)
    cozip (Op1 f) (Op1 g) = Op1 (either f g)

instance (Cozip f, Cozip g) => Cozip (Product f g) where
    full = Pair full full
    cozip (Pair x₁ y₁) (Pair x₂ y₂) = Pair (cozip x₁ x₂) (cozip y₁ y₂)

instance (Cozip f) => Cozip (ReaderT r f) where
    full = ReaderT (\ _ -> full)
    cozip (ReaderT x) (ReaderT y) = ReaderT (liftA2 cozip x y)

instance (Cozip f) => Cozip (StateT s f) where
    full = StateT (\ _ -> fst >$< full)
    cozip (StateT x) (StateT y) = StateT (liftA2 (cozipWith coseparate) x y)

instance (Cozip f) => Cozip (WriterT w f) where
    full = WriterT (fst >$< full)
    cozip (WriterT x) (WriterT y) = WriterT (cozipWith coseparate x y)

coseparate :: (Either a b, c) -> Either (a, c) (b, c)
coseparate = uncurry . flip $ \ c -> flip (,) c +++ flip (,) c
