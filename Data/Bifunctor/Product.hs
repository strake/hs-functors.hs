module Data.Bifunctor.Product where

import Control.Biapplicative (Biapplicable (..), Bipointed (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))

data Product f g a b = Pair { fst :: f a b, snd :: g a b }

instance (Bifoldable f, Bifoldable g) => Bifoldable (Product f g) where
    bifoldMap f g (Pair x y) = bifoldMap f g x <> bifoldMap f g y

instance (Bifunctor f, Bifunctor g) => Bifunctor (Product f g) where
    bimap f g (Pair x y) = Pair (bimap f g x) (bimap f g y)

instance (Bitraversable f, Bitraversable g) => Bitraversable (Product f g) where
    bitraverse f g (Pair x y) = Pair <$> bitraverse f g x <*> bitraverse f g y

instance (Biapplicable f, Biapplicable g) => Biapplicable (Product f g) where
    Pair f g ≪*≫ Pair x y = Pair (f ≪*≫ x) (g ≪*≫ y)

instance (Bipointed f, Bipointed g) => Bipointed (Product f g) where
    bipure a b = Pair (bipure a b) (bipure a b)

fstL :: Functor φ => (f a b -> φ (f' a b)) -> Product f g a b -> φ (Product f' g a b)
fstL f (Pair x y) = flip Pair y <$> f x

sndL :: Functor φ => (g a b -> φ (g' a b)) -> Product f g a b -> φ (Product f g' a b)
sndL g (Pair x y) = Pair x <$> g y
