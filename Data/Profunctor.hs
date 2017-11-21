module Data.Profunctor where

import Prelude hiding ((.), id)

import Control.Arrow (Kleisli (..))
import Control.Category
import Control.Monad

class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap g = dimap id g

instance Profunctor (->) where
    dimap f g a = g . a . f

instance Monad m => Profunctor (Kleisli m) where
    dimap f g (Kleisli a) = Kleisli (fmap g . a . f)
