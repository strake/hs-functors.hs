{-# LANGUAGE ScopedTypeVariables #-}

module Data.FnList where

import Prelude hiding (reverse, tail, zip)
import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..), tail)
import Data.Profunctor

data FnList a b c = Done c | More a (FnList a b (b -> c))
  deriving (Functor)

instance Profunctor (FnList a) where
    dimap _ g (Done c) = Done (g c)
    dimap f g (More a z) = More a $ dimap f (dimap f g) z

arguments :: FnList a b c -> [a]
arguments (Done _) = []
arguments (More a z) = a : arguments z

answer :: (a -> b) -> FnList a b c -> c
answer _ (Done c) = c
answer f (More a z) = answer f z (f a)

instance Foldable (FnList a a) where foldMap φ = φ . answer id

instance Applicative (FnList a b) where
    pure = Done
    Done b <*> c = b <$> c
    More a z <*> c = More a (flip <$> z <*> c)

zip :: FnList a₁ a₁ c₁ -> FnList a₂ a₂ c₂ -> FnList (a₁, a₂) (a₁, a₂) (c₁, c₂)
zip (Done b₁) y = Done (b₁, answer id y)
zip x (Done b₂) = Done (answer id x, b₂)
zip (More a₁ x) (More a₂ y) = More (a₁, a₂) (uncurry (***) <$> zip x y)

argumentsL :: Applicative p => (a -> p a') -> FnList a b c -> p (FnList a' b c)
argumentsL _ (Done c) = pure (Done c)
argumentsL f (More a z) = More <$> f a <*> argumentsL f z

singleton :: a -> FnList a b b
singleton = More `flip` Done id

untraverse :: Applicative f => (a -> f b) -> FnList a b c -> f c
untraverse _ (Done c) = pure c
untraverse f (More a z) = f a <**> untraverse f z

permutations :: FnList a b c -> NonEmpty (FnList a b c)
permutations = (:|) <*> tail . go where
    go (Done c) = pure (Done c)
    go (More a z) = permutations z >>= \ z' ->
        More a z' :| fmap (\ (b, f) -> More b (f a)) (holes z')

holes :: FnList a b c -> [(a, a -> FnList a b c)]
holes (Done _) = []
holes (More a z) = (a, flip More z) : (fmap . fmap . fmap) (More a) (holes z)

merge :: Ord a => FnList a b (c -> d) -> FnList a b c -> FnList a b d
merge = mergeBy compare

mergeBy :: ∀ a b c d . (a -> a -> Ordering) -> FnList a b (c -> d) -> FnList a b c -> FnList a b d
mergeBy cmp = go where
    go :: ∀ b c d . FnList a b (c -> d) -> FnList a b c -> FnList a b d
    go (Done b) c = b <$> c
    go b (Done c) = ($ c) <$> b
    go (More a x) (More b y) = case cmp a b of
        GT -> More b (go ((.) <$> More a x) y)
        _  -> More a (go (flip <$> x) (More b y))

consEnd :: a -> FnList a b (b -> c) -> FnList a b c
consEnd a = \ case
    Done c -> More a (Done c)
    More b l -> More b (consEnd a l)

reverse :: FnList a b c -> FnList a b c
reverse = \ case
    Done c -> Done c
    More a l -> consEnd a (reverse l)
