module Control.Monad.Free where

import Control.Applicative
import Data.Functor.Classes

data Free f a = Pure a | Free (f (Free f a))
  deriving (Functor, Foldable, Traversable)

instance Functor f => Applicative (Free f) where
    pure = Pure
    Pure a <*> Pure b = Pure (a b)
    Pure a <*> Free t = Free (fmap a <$> t)
    Free s <*> y      = Free ((<*> y) <$> s)

instance Functor f => Monad (Free f) where
    Pure a >>= f = f a
    Free t >>= f = Free ((>>= f) <$> t)

instance Eq1 f => Eq1 (Free f) where
    liftEq (==) = (≡)
      where Pure a ≡ Pure b = a == b
            Free s ≡ Free t = liftEq (≡) s t
            _      ≡ _      = False

instance Ord1 f => Ord1 (Free f) where
    liftCompare compare = cmp
      where Pure a `cmp` Pure b = a `compare` b
            Pure _ `cmp` Free _ = LT
            Free _ `cmp` Pure _ = GT
            Free s `cmp` Free t = liftCompare cmp s t

instance Read1 f => Read1 (Free f) where
    liftReadPrec rp rl =
        readUnaryWith rp                                      "Pure" Pure <|>
        readUnaryWith (liftReadPrec (liftReadPrec rp rl)
                                    (liftReadListPrec rp rl)) "Free" Free

instance Show1 f => Show1 (Free f) where
    liftShowsPrec sp sl n = \ case
        Pure a -> showsUnaryWith sp                                   "Pure" n a
        Free t -> showsUnaryWith (liftShowsPrec (liftShowsPrec sp sl)
                                                (liftShowList sp sl)) "Free" n t

instance (Eq a, Eq1 f) => Eq (Free f a) where (==) = liftEq (==)
instance (Ord a, Ord1 f) => Ord (Free f a) where compare = liftCompare compare
instance (Read a, Read1 f) => Read (Free f a) where readsPrec = readsPrec1
instance (Show a, Show1 f) => Show (Free f a) where showsPrec = showsPrec1
