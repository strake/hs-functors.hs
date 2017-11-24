{-# LANGUAGE RankNTypes #-}

module Data.Fix where

import Data.Function (on)
import Data.Functor.Classes
import Text.Read

data Fix f = Fix { unFix :: f (Fix f) }

instance Eq1 f => Eq (Fix f) where (==) = eq1 `on` unFix
instance Ord1 f => Ord (Fix f) where compare = compare1 `on` unFix
instance Read1 f => Read (Fix f) where readPrec = Fix <$> readPrec1
instance Show1 f => Show (Fix f) where showsPrec n = showsPrec1 n . unFix

mapFix :: Functor f => (∀ a . f a -> g a) -> Fix f -> Fix g
mapFix f (Fix x) = Fix (f (mapFix f <$> x))
