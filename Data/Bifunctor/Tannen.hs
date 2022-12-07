module Data.Bifunctor.Tannen where

import Control.Biapplicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Bicotraversable
import Data.Cotraversable
import Data.Functor.Classes
import Text.Read (Read (..))

newtype Tannen f s a b = Tannen { unTannen :: f (s a b) }
  deriving (Functor, Foldable)
deriving instance (Traversable f, Traversable (s a)) => Traversable (Tannen f s a)
instance (Eq1 f, Eq2 s, Eq a, Eq b) => Eq (Tannen f s a b) where (==) = eq2
instance (Ord1 f, Ord2 s, Ord a, Ord b) => Ord (Tannen f s a b) where compare = compare2
instance (Read1 f, Read2 s, Read a, Read b) => Read (Tannen f s a b) where readPrec = readPrec2
instance (Show1 f, Show2 s, Show a, Show b) => Show (Tannen f s a b) where showsPrec = showsPrec2
instance (Eq1 f, Eq2 s, Eq a) => Eq1 (Tannen f s a) where liftEq f (Tannen x) (Tannen y) = liftEq (liftEq f) x y
instance (Ord1 f, Ord2 s, Ord a) => Ord1 (Tannen f s a) where liftCompare f (Tannen x) (Tannen y) = liftCompare (liftCompare f) x y
instance (Read1 f, Read2 s, Read a) => Read1 (Tannen f s a) where liftReadPrec rp rlp = Tannen <$> liftReadPrec (liftReadPrec rp rlp) (liftReadListPrec rp rlp)
instance (Show1 f, Show2 s, Show a) => Show1 (Tannen f s a) where liftShowsPrec sp sl n = liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl) n . unTannen
instance (Eq1 f, Eq2 s) => Eq2 (Tannen f s) where liftEq2 f g (Tannen x) (Tannen y) = liftEq (liftEq2 f g) x y
instance (Ord1 f, Ord2 s) => Ord2 (Tannen f s) where liftCompare2 f g (Tannen x) (Tannen y) = liftCompare (liftCompare2 f g) x y
instance (Read1 f, Read2 s) => Read2 (Tannen f s) where liftReadPrec2 rpa rlpa rpb rlpb = Tannen <$> liftReadPrec (liftReadPrec2 rpa rlpa rpb rlpb) (liftReadListPrec2 rpa rlpa rpb rlpb)
instance (Show1 f, Show2 s) => Show2 (Tannen f s) where liftShowsPrec2 spa sla spb slb n = liftShowsPrec (liftShowsPrec2 spa sla spb slb) (liftShowList2 spa sla spb slb) n . unTannen
instance (Functor f, Bifunctor s) => Bifunctor (Tannen f s) where
    bimap f g = Tannen . fmap (bimap f g) . unTannen
instance (Foldable f, Bifoldable s) => Bifoldable (Tannen f s) where
    bifoldMap f g = foldMap (bifoldMap f g) . unTannen
instance (Traversable f, Bitraversable s) => Bitraversable (Tannen f s) where
    bitraverse f g = fmap Tannen . traverse (bitraverse f g) . unTannen
instance (Cotraversable f, Bicotraversable s) => Bicotraversable (Tannen f s) where
    bicotraverse f g = Tannen . cotraverse (bicotraverse f g) . fmap unTannen
instance (Applicative f, Biapplicable s) => Biapplicable (Tannen f s) where
    Tannen f ≪*≫ Tannen x = Tannen ((≪*≫) <$> f <*> x)
instance (Applicative f, Bipointed s) => Bipointed (Tannen f s) where
    bipure a b = Tannen (pure (bipure a b))
