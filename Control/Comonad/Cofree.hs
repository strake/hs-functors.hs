module Control.Comonad.Cofree where

import Prelude hiding ((.), id, map)

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Monad
import Data.Cotraversable
import Data.Functor.Classes
import Data.Profunctor
import Data.Semigroup ((<>))
import Text.Read (Read (..))

data Cofree f a = Cofree { head :: a, tail :: f (Cofree f a) }
  deriving (Functor, Foldable, Traversable)

instance Alternative f => Applicative (Cofree f) where
    pure = flip Cofree empty
    (<*>) = ap

instance Alternative f => Monad (Cofree f) where
    x >>= f = join (f <$> x) where join (Cofree (Cofree a s) t) = Cofree a (s <|> join <$> t)

instance Functor f => Comonad (Cofree f) where
    copure (Cofree a _) = a
    cut ɯ@(Cofree _ t) = Cofree ɯ (cut <$> t)

instance Cotraversable f => Cotraversable (Cofree f) where
    cosequence = Cofree <$> fmap (\ (Cofree a _) -> a)
                        <*> fmap cosequence . collect (\ (Cofree _ t) -> t)

instance Eq1 f => Eq1 (Cofree f) where
    liftEq (==) (Cofree a s) (Cofree b t) = a == b && (liftEq . liftEq) (==) s t

instance Ord1 f => Ord1 (Cofree f) where
    liftCompare cmp (Cofree a s) (Cofree b t) = a `cmp` b <> (liftCompare . liftCompare) cmp s t

instance Read1 f => Read1 (Cofree f) where
    liftReadPrec rp rl =
        readBinaryWith rp (liftReadPrec (liftReadPrec rp rl)
                                        (liftReadListPrec rp rl)) "Cofree"  Cofree

instance Show1 f => Show1 (Cofree f) where
    liftShowsPrec sp sl n (Cofree a t) =
        showsBinaryWith sp (liftShowsPrec (liftShowsPrec sp sl)
                                          (liftShowList sp sl)) "Cofree" n a t

instance (Eq1 f, Eq a) => Eq (Cofree f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Cofree f a) where compare = compare1
instance (Read1 f, Read a) => Read (Cofree f a) where readPrec = readPrec1
instance (Show1 f, Show a) => Show (Cofree f a) where showsPrec = showsPrec1

raise :: Comonad ɯ => ɯ a -> Cofree ɯ a
raise = liftA2 Cofree copure (raise <<=)

lower :: Functor ɯ => Cofree ɯ a -> ɯ a
lower (Cofree _ t) = copure <$> t

coiter :: Functor f => (a -> f a) -> a -> Cofree f a
coiter f = unfold (id &&& f)

coiterW :: (Comonad ɯ, Functor f) => (ɯ a -> f (ɯ a)) -> ɯ a -> Cofree f a
coiterW f ɯ = copure ɯ `Cofree` (coiterW f <$> f ɯ)

unfold :: Functor f => (a -> (b, f a)) -> a -> Cofree f b
unfold f = f >>> \ (b, af) -> Cofree b (unfold f <$> af)

unfoldM :: (Traversable f, Monad m) => (a -> m (b, f a)) -> a -> m (Cofree f b)
unfoldM f = f >=> \ (b, af) -> Cofree b <$> unfoldM f `traverse` af

unfoldW :: (Cotraversable f, Comonad ɯ) => (ɯ a -> (b, f a)) -> ɯ a -> Cofree f b
unfoldW f = f =>= \ ɯ -> Cofree (fst (copure ɯ)) (unfoldW f `cotraverse` (snd <$> ɯ))

map :: Functor f => (∀ a . f a -> g a) -> Cofree f a -> Cofree g a
map f (Cofree a t) = Cofree a (f (map f <$> t))
