{-# LANGUAGE ExistentialQuantification #-}
module Data.Functor.Day where

import Prelude hiding ((.), id)
import Control.Category
import Control.Comonad
import Control.Comonad.Trans.Class (ComonadTrans (..))
import Data.Bifunctor.Tannen (Tannen (..))
import Data.Functor.Identity (Identity (..))
import Data.Profunctor.Compose (Procompose (..))

data Day f g a = ∀ b c . Day (f b) (g c) (b -> c -> a)
deriving instance Functor (Day f g)

instance (Applicative f, Applicative g) => Applicative (Day f g) where
    pure a = Day (pure ()) (pure ()) (\ () () -> a)
    Day x₁ y₁ φ₁ <*> Day x₂ y₂ φ₂ =
        Day ((,) <$> x₁ <*> x₂) ((,) <$> y₁ <*> y₂) (\ (a₁, a₂) (b₁, b₂) -> φ₁ a₁ b₁ $ φ₂ a₂ b₂)

instance (Comonad f, Comonad g) => Comonad (Day f g) where
    copure (Day x y φ) = φ (copure x) (copure y)
    cut (Day x y φ) = Day (cut x) (cut y) (\ a b -> Day a b φ)

day :: f (a -> b) -> g a -> Day f g b
day x y = Day x y id

dap :: Applicative f => Day f f a -> f a
dap (Day x y φ) = φ <$> x <*> y

instance Comonad f => ComonadTrans (Day f) where
    colift (Day x y φ) = φ (copure x) <$> y

assoc :: Day f (Day g h) a -> Day (Day f g) h a
assoc (Day x (Day y z φ) χ) = Day (Day x y (,)) z $ \ (a, b) c -> χ a (φ b c)

disassoc :: Day (Day f g) h a -> Day f (Day g h) a
disassoc (Day (Day x y φ) z χ) = Day x (Day y z (,)) $ \ a (b, c) -> χ (φ a b) c

swap :: Day f g a -> Day g f a
swap (Day x y φ) = Day y x (flip φ)

liftR :: g a -> Day Identity g a
liftR y = Day (Identity ()) y (\ () -> id)

liftL :: f a -> Day f Identity a
liftL x = Day x (Identity ()) (flip $ \ () -> id)

unliftR :: Functor g => Day Identity g a -> g a
unliftR (Day (Identity a) y φ) = φ a <$> y

unliftL :: Functor f => Day f Identity a -> f a
unliftL (Day x (Identity b) φ) = flip φ b <$> x

mapR :: (∀ a . g a -> h a) -> Day f g a -> Day f h a
mapR g (Day x y φ) = Day x (g y) φ

mapL :: (∀ a . f a -> h a) -> Day f g a -> Day h g a
mapL f (Day x y φ) = Day (f x) y φ

cayley :: Procompose (Tannen f p) (Tannen g q) a b -> Tannen (Day f g) (Procompose p q) a b
cayley (Procompose (Tannen p) (Tannen q)) = Tannen (Day p q Procompose)

dayley :: Category p => Procompose (Tannen f p) (Tannen g p) a b -> Tannen (Day f g) p a b
dayley (Procompose (Tannen p) (Tannen q)) = Tannen (Day p q (.))
