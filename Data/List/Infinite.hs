{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fstatic-argument-transformation #-}

module Data.List.Infinite
  ( Infinite (..), head, tail
  , (++), (<*≥), (≤*>), zap, zip, zipWith
  , (!!), at
  , break, span, spanJust, splitAt, drop, dropWhile, groupBy
  , concatMap, foldr, unfoldr, iterate, iterate', cycle, scanl, tails
  ) where

import Prelude (($), (<$>), (-), Applicative (..), Bool (..), Foldable, Functor (..), Maybe (..), Monad (..), Traversable (..), flip, maybe, otherwise, seq)
import Control.Category (Category (..))
import Control.Comonad (Comonad (..))
import Data.Bifunctor (first)
import Data.Filtrable (Filtrable (mapMaybe))
import Data.Foldable (toList)
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import Util.Magic

infixr 5 :.
data Infinite a = a :. Infinite a
  deriving (Foldable, Functor, Traversable)

head :: Infinite a -> a
head (a:._) = a
{-# NOINLINE [1] head #-}

tail :: Infinite a -> Infinite a
tail (_:.as) = as
{-# NOINLINE [1] tail #-}

{-# RULES
"head/build" ∀ (g :: ∀ b . (a -> b -> b) -> b) . head (build g) = g (\ x _ -> x)
#-}

instance Filtrable Infinite where
    mapMaybe f (a:.as) = maybe id (:.) (f a) (mapMaybe f as)

instance Applicative Infinite where
    pure a = a :. pure a
    f:.fs <*> a:.as = f a :. (fs <*> as)

instance Monad Infinite where
    x >>= f = join (f <$> x)
      where
        join (a:.as) = head a :. join (tail <$> as)

instance Comonad Infinite where
    copure = head
    cut = tails

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
unfoldr f b = build \ c -> let go b = case f b of (a, b') -> a `c` go b' in go b
{-# INLINE unfoldr #-}

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
{-# NOINLINE [1] scanl #-}

{-# RULES
"scanl" [~1] ∀ f a bs . scanl f a bs = build \ c -> a `c` foldr (scanlFB f c) bs a
"scanlList" [1] ∀ f (a :: a) bs . foldr (scanlFB f (:.)) bs a = tail (scanl f a bs)
#-}

scanlFB :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB f c = \ b g -> oneShot \ x -> let b' = f x b in b' `c` g b'
{-# INLINE [0] scanlFB #-}

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

{-# NOINLINE [1] iterate #-}
{-# NOINLINE [1] iterate' #-}

{-# RULES
"iterate" [~1] ∀ f a . iterate f a = build (\ c -> iterateFB c f a)
"iterate'" [~1] ∀ f a . iterate' f a = build (\ c -> iterateFB' c f a)
"iterateFB" [1] iterateFB (:.) = iterate
"iterateFB'" [1] iterateFB' (:.) = iterate'
#-}

iterateFB, iterateFB' :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f x₀ = go x₀
  where go x = x `c` go (f x)
iterateFB' c f x₀ = go x₀
  where go x = x' `seq` (x `c` go x') where x' = f x

{-# INLINE [0] iterateFB #-}
{-# INLINE [0] iterateFB' #-}

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
{-# NOINLINE [1] concatMap #-}

{-# RULES "concatMap" ∀ f as . concatMap f as = build \ c -> foldr (\ x b -> F.foldr c b (f x)) as #-}

spanJust :: (a -> Maybe b) -> Infinite a -> ([b], Infinite a)
spanJust f as@(a:.as')
  | Just b <- f a = case spanJust f as' of (bs, cs) -> (b:bs, cs)
  | otherwise = ([], as)

foldr :: (a -> b -> b) -> Infinite a -> b
foldr f (a:.as) = f a (foldr f as)
{-# INLINE [0] foldr #-}

build :: (∀ b . (a -> b -> b) -> b) -> Infinite a
build g = g (:.)
{-# INLINE [1] build #-}

{-# RULES
"foldr/build" ∀ f (g :: ∀ b . (a -> b -> b) -> b) . foldr f (build g) = g f
"foldr/id" foldr (:.) = id

"foldr/cons/build" ∀ f a (g :: ∀ b . (a -> b -> b) -> b) . foldr f (a:.build g) = f a (g f)
#-}

{-# RULES
"map" [~1] ∀ f as . fmap f as = build \ c -> foldr (mapFB c f) as
"mapFB" ∀ c f g . mapFB (mapFB c f) g = mapFB c (f . g)
"mapFB/id" ∀ c . mapFB c (\ x -> x) = c
#-}

mapFB :: (b -> c -> c) -> (a -> b) -> a -> c -> c
mapFB c f = \ x ys -> c (f x) ys
{-# INLINE [0] mapFB #-}
