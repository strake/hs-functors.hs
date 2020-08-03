{-# LANGUAGE UndecidableInstances #-}

module Control.Apply
  ( Apply (..)
  , Bind (..)
  , (<**>)
  , (=<<)
  , (>=>)
  , (<=<)
  , liftA3
  , apDefault
  ) where

import Prelude (Functor (..), (<$>), Eq, Ord, Read, Show, Foldable, Traversable, flip, uncurry, ($))
import qualified Control.Applicative as Base
import Control.Category (Category (id))
import qualified Control.Monad as Base
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Writer (WriterT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State (StateT (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (..))
import Data.Semigroup (Semigroup (..))

infixl 4 <*>, *>, <*, <**>
-- | Laws:
--
-- @(.) <$> a <*> b <*> c = a <*> (b <*> c)@
-- @a <*> (f <$> b) = (. f) <$> a <*> b@
-- @f <$> (a <*> b) = (f .) <$> a <*> b@
class Functor f => Apply f where
    {-# MINIMAL (<*>) | liftA2 #-}

    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id

    (*>) :: f a -> f b -> f b
    (*>) = liftA2 (\ _ b -> b)

    (<*) :: f a -> f b -> f a
    (<*) = liftA2 (\ a _ -> a)

    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f a b = f <$> a <*> b

(<**>) :: Apply f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip id)

liftA3 :: Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

infixl 1 >>=
class Apply m => Bind m where
    {-# MINIMAL (>>=) | join #-}

    (>>=) :: m a -> (a -> m b) -> m b
    x >>= f = join (f <$> x)

    join :: m (m a) -> m a
    join = (>>= id)

infixr 1 =<<, >=>, <=<
(=<<) :: Bind m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(>=>) :: Bind m => (a -> m b) -> (b -> m c) -> a -> m c
(f >=> g) x = f x >>= g

(<=<) :: Bind m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = flip (>=>)

instance {-# OVERLAPPABLE #-} (Functor f, Base.Applicative f) => Apply f where
    (<*>) = (Base.<*>)
    (<*) = (Base.<*)
    (*>) = (Base.*>)
    liftA2 = Base.liftA2

instance {-# OVERLAPPABLE #-} (Functor m, Base.Monad m) => Bind m where
    (>>=) = (Base.>>=)

instance (Apply f, Apply g) => Apply (Compose f g) where
    Compose x <*> Compose y = Compose (liftA2 (<*>) x y)

instance (Apply f, Apply g) => Apply (Product f g) where
    Pair u x <*> Pair v y = Pair (u <*> v) (x <*> y)

instance (Bind f, Bind g) => Bind (Product f g) where
    join (Pair x y) = Pair (x >>= \ (Pair x _) -> x) (y >>= \ (Pair _ y) -> y)

instance Semigroup a => Apply ((,) a) where
    (a, f) <*> (b, x) = (a <> b, f x)

instance Semigroup a => Bind ((,) a) where
    join (a, (b, x)) = (a <> b, x)

data Lift f a = Pure a | Other (f a)
  deriving (Eq, Ord, Read, Show, Foldable, Functor, Traversable)

instance Apply f => Apply (Lift f) where
    Pure f <*> x = f <$> x
    f <*> Pure x = ($ x) <$> f
    Other f <*> Other x = Other (f <*> x)

instance Apply f => Base.Applicative (Lift f) where
    pure = Pure
    (<*>) = (<*>)

instance Apply f => Apply (IdentityT f) where
    IdentityT x <*> IdentityT y = IdentityT (x <*> y)

instance Apply f => Apply (ExceptT e f) where
    ExceptT x <*> ExceptT y = ExceptT (liftA2 (<*>) x y)

instance Apply f => Apply (ReaderT r f) where
    ReaderT x <*> ReaderT y = ReaderT (liftA2 (<*>) x y)

instance (Semigroup w, Apply f) => Apply (WriterT w f) where
    WriterT x <*> WriterT y = WriterT (liftA2 (\ (f, a) (x, b) -> (f x, a <> b)) x y)

instance Bind m => Apply (StateT s m) where
    StateT x <*> StateT y = StateT $ x >=> \ (a, s) -> (\ (b, t) -> (a b, t)) <$> y s

instance Bind m => Bind (IdentityT m) where
    join (IdentityT x) = IdentityT (x >>= runIdentityT)

instance Bind m => Bind (ReaderT r m) where
    join (ReaderT x) = ReaderT $ (>>=) <$> x <*> flip runReaderT

instance (Semigroup w, Bind m) => Bind (WriterT w m) where
    join (WriterT x) = WriterT $ x >>= \ (WriterT y, w) -> fmap (w <>) <$> y

instance Bind m => Bind (StateT s m) where
    join (StateT x) = StateT $ x >=> uncurry runStateT

apDefault :: Bind m => m (a -> b) -> m a -> m b
apDefault f x = f >>= (<$> x)
