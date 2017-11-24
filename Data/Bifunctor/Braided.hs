module Data.Bifunctor.Braided where

class Braided f where braid :: f a b -> f b a

instance Braided (,) where braid (x, y) = (y, x)
instance Braided Either where
    braid (Left  x) = Right x
    braid (Right y) = Left  y
