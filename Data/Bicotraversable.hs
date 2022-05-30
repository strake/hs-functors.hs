module Data.Bicotraversable where

import Control.Arrow
import Data.Bifunctor
import Data.Functor.Const
import Data.Tagged (Tagged (..))

class Bifunctor f => Bicotraversable f where
    {-# MINIMAL bicosequence | bicotraverse #-}

    bicollect :: Functor g => (a -> f b c) -> g a -> f (g b) (g c)
    bicollect f = bicosequence . fmap f

    bicosequence :: Functor g => g (f a b) -> f (g a) (g b)
    bicosequence = bicotraverse id id

    bicotraverse :: Functor g => (g a -> b) -> (g c -> d) -> g (f a c) -> f b d
    bicotraverse f g = bimap f g . bicosequence

instance Bicotraversable (,) where
    bicotraverse f g = f . fmap fst &&& g . fmap snd

instance Bicotraversable Const where
    bicotraverse f _ = Const . f . fmap getConst

instance Bicotraversable Tagged where
    bicotraverse _ g = Tagged . g . fmap unTagged
