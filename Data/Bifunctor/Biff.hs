module Data.Bifunctor.Biff where

import Control.Biapplicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Bicotraversable
import Data.Cotraversable
import Data.Functor.Classes
import Text.Read (Read (..))

newtype Biff s f g a b = Biff { unBiff :: f a `s` g b }
  deriving (Functor, Foldable)
instance (Traversable (s (f a)), Traversable g) => Traversable (Biff s f g a) where
    traverse f = fmap Biff . (traverse . traverse) f . unBiff
instance (Eq2 s, Eq1 f, Eq1 g, Eq a, Eq b) => Eq (Biff s f g a b) where (==) = eq2
instance (Ord2 s, Ord1 f, Ord1 g, Ord a, Ord b) => Ord (Biff s f g a b) where compare = compare2
instance (Read2 s, Read1 f, Read1 g, Read a, Read b) => Read (Biff s f g a b) where readPrec = readPrec2
instance (Show2 s, Show1 f, Show1 g, Show a, Show b) => Show (Biff s f g a b) where showsPrec = showsPrec2
instance (Eq2 s, Eq1 f, Eq1 g) => Eq2 (Biff s f g) where
    liftEq2 f g (Biff x) (Biff y) = liftEq2 (liftEq f) (liftEq g) x y
instance (Ord2 s, Ord1 f, Ord1 g) => Ord2 (Biff s f g) where
    liftCompare2 f g (Biff x) (Biff y) = liftCompare2 (liftCompare f) (liftCompare g) x y
instance (Read2 s, Read1 f, Read1 g) => Read2 (Biff s f g) where
    liftReadPrec2 f fs g gs = Biff <$> liftReadPrec2 (liftReadPrec f fs) (liftReadListPrec f fs) (liftReadPrec g gs) (liftReadListPrec g gs)
instance (Show2 s, Show1 f, Show1 g) => Show2 (Biff s f g) where
    liftShowsPrec2 f fs g gs n = liftShowsPrec2 (liftShowsPrec f fs) (liftShowList f fs) (liftShowsPrec g gs) (liftShowList g gs) n . unBiff
instance (Bifunctor s, Functor f, Functor g) => Bifunctor (Biff s f g) where
    bimap f g = Biff . bimap (fmap f) (fmap g) . unBiff
instance (Bifoldable s, Foldable f, Foldable g) => Bifoldable (Biff s f g) where
    bifoldMap f g = bifoldMap (foldMap f) (foldMap g) . unBiff
instance (Bitraversable s, Traversable f, Traversable g) => Bitraversable (Biff s f g) where
    bitraverse f g = fmap Biff . bitraverse (traverse f) (traverse g) . unBiff
instance (Bicotraversable s, Cotraversable f, Cotraversable g) => Bicotraversable (Biff s f g) where
    bicotraverse f g = Biff . bicotraverse (cotraverse f) (cotraverse g) . fmap unBiff
instance (Biapplicable s, Applicative f, Applicative g) => Biapplicable (Biff s f g) where
    Biff f ≪*≫ Biff x = Biff (bimap (<*>) (<*>) f ≪*≫ x)
instance (Bipointed s, Applicative f, Applicative g) => Bipointed (Biff s f g) where
    bipure a b = Biff (bipure (pure a) (pure b))
