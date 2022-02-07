module Data.List.Infinite where

import Prelude hiding ((!!), (++), head, tail)
import Data.Filtrable (Filtrable (..))
import Data.Foldable (toList)
import Numeric.Natural (Natural)

infixr 5 :.
data Infinite a = (:.) { head :: a, tail :: Infinite a }
  deriving (Foldable, Functor, Traversable)

instance Filtrable Infinite where
    mapMaybe f (a:.as) = maybe id (:.) (f a) (mapMaybe f as)

instance Applicative Infinite where
    pure a = a :. pure a
    f:.fs <*> a:.as = f a :. (fs <*> as)

instance Monad Infinite where
    x >>= f = join (f <$> x)
      where
        join (a:.as) = head a :. join (tail <$> as)

infixr 5 ++
(++) :: Foldable f => f a -> Infinite a -> Infinite a
(++) = flip (foldr (:.))

infixl 4 ≤*>, <*≥
(<*≥) :: Foldable f => Infinite (a -> b) -> f a -> Infinite b
f:.fs <*≥ as = (f <$> toList as) ++ (fs <*≥ as)
{-# SPECIALIZE (<*≥) :: Infinite (a -> b) -> [a] -> Infinite b #-}

(≤*>) :: Foldable f => f (a -> b) -> Infinite a -> Infinite b
fs ≤*> a:.as = fmap ($ a) (toList fs) ++ (fs ≤*> as)
{-# SPECIALIZE (≤*>) :: [a -> b] -> Infinite a -> Infinite b #-}

unfoldr :: (b -> (a, b)) -> b -> Infinite a
unfoldr f b = case f b of (a, b') -> a :. unfoldr f b'

(!!) :: Infinite a -> Natural -> a
(a:._) !! 0 = a
(_:.as) !! n = as !! (n-1)

at :: Functor f => Natural -> (a -> f a) -> Infinite a -> f (Infinite a)
at 0 f (a:.as) = (:.as) <$> f a
at n f (a:.as) = (a:.) <$> at (n-1) f as
