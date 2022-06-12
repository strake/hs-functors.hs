module Control.Biapplicative where

import Control.Category.Dual (Dual (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Const (Const (..))
import Data.Tagged (Tagged (..))

class Bifunctor p => Biapplicable p where
    (≪*≫) :: p (a₁ -> b₁) (a₂ -> b₂) -> p a₁ a₂ -> p b₁ b₂
    (≪*≫) = biliftA2 id id

    biliftA2 :: (a₁ -> b₁ -> c₁) -> (a₂ -> b₂ -> c₂) -> p a₁ a₂ -> p b₁ b₂ -> p c₁ c₂
    biliftA2 f g x y = bimap f g x ≪*≫ y

instance Biapplicable (,) where
    (f₁, f₂) ≪*≫ (x₁, x₂) = (f₁ x₁, f₂ x₂)

instance Biapplicable Const where
    Const f ≪*≫ Const a = Const (f a)

instance Biapplicable Tagged where
    Tagged f ≪*≫ Tagged a = Tagged (f a)

instance (Semigroup a) => Biapplicable ((,,) a) where
    (a₁, φ₁, φ₂) ≪*≫ (a₂, χ₁, χ₂) = (a₁ <> a₂, φ₁ χ₁, φ₂ χ₂)

instance (Semigroup a, Semigroup b) => Biapplicable ((,,,) a b) where
    (a₁, b₁, φ₁, φ₂) ≪*≫ (a₂, b₂, χ₁, χ₂) = (a₁ <> a₂, b₁ <> b₂, φ₁ χ₁, φ₂ χ₂)

instance (Semigroup a, Semigroup b, Semigroup c) => Biapplicable ((,,,,) a b c) where
    (a₁, b₁, c₁, φ₁, φ₂) ≪*≫ (a₂, b₂, c₂, χ₁, χ₂) = (a₁ <> a₂, b₁ <> b₂, c₁ <> c₂, φ₁ χ₁, φ₂ χ₂)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Biapplicable ((,,,,,) a b c d) where
    (a₁, b₁, c₁, d₁, φ₁, φ₂) ≪*≫ (a₂, b₂, c₂, d₂, χ₁, χ₂) = (a₁ <> a₂, b₁ <> b₂, c₁ <> c₂, d₁ <> d₂, φ₁ χ₁, φ₂ χ₂)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Biapplicable ((,,,,,,) a b c d e) where
    (a₁, b₁, c₁, d₁, e₁, φ₁, φ₂) ≪*≫ (a₂, b₂, c₂, d₂, e₂, χ₁, χ₂) = (a₁ <> a₂, b₁ <> b₂, c₁ <> c₂, d₁ <> d₂, e₁ <> e₂, φ₁ χ₁, φ₂ χ₂)

instance Biapplicable p => Biapplicable (Dual p) where
    Dual f ≪*≫ Dual x = Dual (f ≪*≫ x)

class Bipointed p where
    bipure :: a -> b -> p a b

instance Bipointed (,) where
    bipure = (,)

instance Bipointed Const where
    bipure a _ = Const a

instance Bipointed Tagged where
    bipure _ b = Tagged b

instance (Monoid a) => Bipointed ((,,) a) where
    bipure = (,,) mempty

instance (Monoid a, Monoid b) => Bipointed ((,,,) a b) where
    bipure = (,,,) mempty mempty

instance (Monoid a, Monoid b, Monoid c) => Bipointed ((,,,,) a b c) where
    bipure = (,,,,) mempty mempty mempty

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Bipointed ((,,,,,) a b c d) where
    bipure = (,,,,,) mempty mempty mempty mempty

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Bipointed ((,,,,,,) a b c d e) where
    bipure = (,,,,,,) mempty mempty mempty mempty mempty

instance Bipointed p => Bipointed (Dual p) where
    bipure a b = Dual (bipure b a)
