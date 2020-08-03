{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Functor.Kan.L where

import Control.Comonad
import Control.Comonad.Trans.Class (ComonadTrans (..))
import Data.Functor.Identity (Identity)
import Data.Functor.Join (Join1 (..))

data L f g a = ∀ b . L (f b -> a) (g b)
deriving instance Functor (L f g)

instance (Functor f, Applicative g) => Applicative (L f g) where
    pure a = L (pure a) (pure ())
    L f x <*> L g y = L (f . fmap fst <*> g . fmap snd) ((,) <$> x <*> y)

to :: Functor k => (∀ a . g a -> k (f a)) -> L f g a -> k a
to φ (L f x) = fmap f (φ x)

from :: (∀ a . L f g a -> k a ) -> g a -> k (f a)
from = (. g)

g :: g a -> L f g (f a)
g = L id

type Coyoneda = L Identity

type Density = Join1 L
pattern Density :: (f b -> a) -> f b -> Density f a
pattern Density f x = Join1 (L f x)
{-# COMPLETE Density #-}

deriving instance Functor (Join1 L f)
deriving instance Applicative f => Applicative (Density f)

instance Comonad (Density f) where
    copure (Density f x) = f x
    cut ɯ@(Density _ x) = Density (pure ɯ) x

instance ComonadTrans Density where
    colift (Density f x) = f <<= x
