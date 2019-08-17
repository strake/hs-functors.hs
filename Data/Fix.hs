module Data.Fix where

import Control.Arrow
import Control.Comonad
import Control.Monad
import Data.Cotraversable
import Data.Function (on)
import Data.Functor.Classes
import Text.Read

newtype Fix f = Fix { unFix :: f (Fix f) }

instance Eq1 f => Eq (Fix f) where (==) = eq1 `on` unFix
instance Ord1 f => Ord (Fix f) where compare = compare1 `on` unFix
instance Read1 f => Read (Fix f) where readPrec = Fix <$> readPrec1
instance Show1 f => Show (Fix f) where showsPrec n = showsPrec1 n . unFix

mapFix :: Functor f => (∀ a . f a -> g a) -> Fix f -> Fix g
mapFix f = Fix . f . fmap (mapFix f) . unFix

traverseFix :: (Traversable f, Monad m) => (∀ a . f a -> m (g a)) -> Fix f -> m (Fix g)
traverseFix f = fmap Fix . f <=< traverse (traverseFix f) . unFix

cotraverseFix :: (Cotraversable f, Comonad ɯ) => (∀ a . ɯ (f a) → g a) -> ɯ (Fix f) -> Fix g
cotraverseFix f = Fix . f =<= cotraverse (cotraverseFix f) . fmap unFix

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

cataM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataM f = f <=< traverse (cataM f) <<< unFix

cataW :: (Cotraversable f, Comonad ɯ) => (ɯ (f a) -> a) -> ɯ (Fix f) -> a
cataW f = f =<= cotraverse (cataW f) <<< fmap unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana f = Fix . fmap (ana f) . f

anaM :: (Traversable f, Monad m) => (a -> m (f a)) -> a -> m (Fix f)
anaM f = fmap Fix <<< traverse (anaM f) <=< f

anaW :: (Cotraversable f, Comonad ɯ) => (ɯ a -> f a) -> ɯ a -> Fix f
anaW f = Fix <<< cotraverse (anaW f) =<= f
