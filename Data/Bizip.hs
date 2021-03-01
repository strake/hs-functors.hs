{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Bizip where

import Control.Applicative (liftA2)
import Control.Arrow ((***), (+++), (<<<), Kleisli (..))
import Control.Category.Dual (Dual (..))
import Data.Bitraversable (bisequenceA)
import Data.Functor.Const (Const (..))
import Data.Tagged (Tagged (..))

class Bizip g f where
    bizip :: f a₁ b₁ -> f a₂ b₂ -> f (g a₁ a₂) (g b₁ b₂)

instance Bizip (,) (->) where
    bizip = (***)

instance Applicative p => Bizip (,) (Kleisli p) where
    bizip (Kleisli f) (Kleisli g) = Kleisli $ \ (a, b) -> liftA2 (,) (f a) (g b)

instance Bizip Either (->) where
    bizip = (+++)

instance Applicative p => Bizip Either (Kleisli p) where
    bizip (Kleisli f) (Kleisli g) = Kleisli $ bisequenceA <<< f +++ g

instance Bizip (,) (,) where
    bizip (a₁, b₁) (a₂, b₂) = ((a₁, a₂), (b₁, b₂))

instance Semigroup a => Bizip (,) ((,,) a) where
    bizip (a₁, b₁, c₁) (a₂, b₂, c₂) = (a₁ <> a₂, (b₁, b₂), (c₁, c₂))

instance (Semigroup a, Semigroup b) => Bizip (,) ((,,,) a b) where
    bizip (a₁, b₁, c₁, d₁) (a₂, b₂, c₂, d₂) = (a₁ <> a₂, b₁ <> b₂, (c₁, c₂), (d₁, d₂))

instance (Semigroup a, Semigroup b, Semigroup c) => Bizip (,) ((,,,,) a b c) where
    bizip (a₁, b₁, c₁, d₁, e₁) (a₂, b₂, c₂, d₂, e₂) = (a₁ <> a₂, b₁ <> b₂, c₁ <> c₂, (d₁, d₂), (e₁, e₂))

instance Bizip (,) Const where
    bizip (Const a) (Const b) = Const (a, b)

instance Bizip (,) Tagged where
    bizip (Tagged a) (Tagged b) = Tagged (a, b)

instance Bizip g f => Bizip g (Dual f) where
    bizip (Dual a) (Dual b) = Dual (bizip a b)
