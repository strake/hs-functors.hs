module Data.Functor.Join where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Bicotraversable
import Data.Cotraversable
import Data.Functor.Classes
import Text.Read (Read (..))

newtype Join p a = Join { unJoin :: p a a }
instance Eq2 p => Eq1 (Join p) where liftEq eq (Join x) (Join y) = liftEq2 eq eq x y
instance Ord2 p => Ord1 (Join p) where liftCompare cmp (Join x) (Join y) = liftCompare2 cmp cmp x y
instance Read2 p => Read1 (Join p) where liftReadPrec rp rlp = Join <$> liftReadPrec2 rp rlp rp rlp
instance Show2 p => Show1 (Join p) where liftShowsPrec sp sl n = liftShowsPrec2 sp sl sp sl n . unJoin
instance (Eq2 p, Eq a) => Eq (Join p a) where (==) = eq1
instance (Ord2 p, Ord a) => Ord (Join p a) where compare = compare1
instance (Read2 p, Read a) => Read (Join p a) where readPrec = readPrec1
instance (Show2 p, Show a) => Show (Join p a) where showsPrec = showsPrec1
instance Bifunctor p => Functor (Join p) where fmap f = Join . bimap f f . unJoin
instance Bifoldable p => Foldable (Join p) where foldMap f = bifoldMap f f . unJoin
instance Bitraversable p => Traversable (Join p) where traverse f = fmap Join . bitraverse f f . unJoin
instance Bicotraversable p => Cotraversable (Join p) where cotraverse f = Join . bicotraverse f f . fmap unJoin
