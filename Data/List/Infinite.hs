module Data.List.Infinite where

import Prelude (($), (<$>), (-), Applicative (..), Bool (..), Foldable, Functor (..), Monad (..), Traversable (..), flip, maybe, otherwise, seq)
import Control.Category (Category (..))
import Data.Bifunctor (first)
import Data.Filtrable (Filtrable (mapMaybe))
import Data.Foldable (toList)
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
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
(++) = flip (F.foldr (:.))

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

groupBy :: (a -> a -> Bool) -> Infinite a -> Infinite (NonEmpty a)
groupBy eq (a:.as) = (a:|bs) :. groupBy eq cs
  where (bs, cs) = span (eq a) as

break, span :: (a -> Bool) -> Infinite a -> ([a], Infinite a)
break p as@(a:.as')
  | p a = ([], as)
  | otherwise = let (bs, cs) = break p as' in (a:bs, cs)
span p as@(a:.as')
  | p a = let (bs, cs) = span p as' in (a:bs, cs)
  | otherwise = ([], as)

scanl :: (b -> a -> b) -> b -> Infinite a -> Infinite b
scanl f z (x:.xs) = z :. scanl f (f z x) xs

tails :: Infinite a -> Infinite (Infinite a)
tails as@(_:.bs) = as:.tails bs

drop :: Natural -> Infinite a -> Infinite a
drop 0 = id
drop n = drop (n-1) . tail

dropWhile :: (a -> Bool) -> Infinite a -> Infinite a
dropWhile f as@(a:.as')
  | f a = dropWhile f as'
  | otherwise = as

splitAt :: Natural -> Infinite a -> ([a], Infinite a)
splitAt 0 as = ([], as)
splitAt n (a:.as) = first (a:) (splitAt (n-1) as)

iterate, iterate' :: (a -> a) -> a -> Infinite a
iterate f a = a :. iterate f (f a)
iterate' f a = let a' = f a in a' `seq` (a :. iterate' f a')

zipWith :: (a -> b -> c) -> Infinite a -> Infinite b -> Infinite c
zipWith f (a:.as) (b:.bs) = f a b :. zipWith f as bs

zip :: Infinite a -> Infinite b -> Infinite (a, b)
zip = zipWith (,)

infixl 4 `zap`
zap :: Infinite (a -> b) -> Infinite a -> Infinite b
zap = zipWith id

cycle :: NonEmpty a -> Infinite a
cycle xs = xs' where xs' = xs ++ xs'

concatMap :: Foldable f => (a -> f b) -> Infinite a -> Infinite b
concatMap f (a:.as) = f a ++ concatMap f as

foldr :: (a -> b -> b) -> Infinite a -> b
foldr f (a:.as) = f a (foldr f as)
