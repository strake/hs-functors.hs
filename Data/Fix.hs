{-# LANGUAGE RankNTypes #-}

module Data.Fix where

import Control.Comonad
import Data.Cotraversable
import Data.Function (on)
import Data.Functor.Classes
import Text.Read

data Fix f = Fix { unFix :: f (Fix f) }

instance Eq1 f => Eq (Fix f) where (==) = eq1 `on` unFix
instance Ord1 f => Ord (Fix f) where compare = compare1 `on` unFix
instance Read1 f => Read (Fix f) where readPrec = Fix <$> readPrec1
instance Show1 f => Show (Fix f) where showsPrec n = showsPrec1 n . unFix

mapFix :: Functor f => (∀ a . f a -> g a) -> Fix f -> Fix g
mapFix f (Fix x) = Fix (f (mapFix f <$> x))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f (Fix x) = f (cata f <$> x)

cataM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataM f (Fix x) = traverse (cataM f) x >>= f

ana :: Functor f => (a -> f a) -> a -> Fix f
ana f x = Fix (ana f <$> f x)

anaW :: (Cotraversable f, Comonad ɯ) => (ɯ a -> f a) -> ɯ a -> Fix f
anaW f x = Fix (cotraverse (anaW f) (x =>> f))
