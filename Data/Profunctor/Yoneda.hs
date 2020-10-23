{-# LANGUAGE RankNTypes #-}
{- LANGUAGE QuantifiedConstraints -}
{- LANGUAGE ConstraintKinds -}
{-# LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE UndecidableInstances -}

module Data.Profunctor.Yoneda where

import Prelude hiding ((.), id)
import Control.Category
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Profunctor
import Util (compose2)

newtype Yoneda p a b = Yoneda { unYoneda :: ∀ α β . (α -> a) -> (b -> β) -> p α β }

map'' :: (∀ a b . p a b -> q a b) -> Yoneda p a b -> Yoneda q a b
map'' t (Yoneda φ) = Yoneda (\ f g -> t (φ f g))

pure'' :: Profunctor p => p a b -> Yoneda p a b
pure'' x = Yoneda (\ f g -> dimap f g x)

join'' :: Profunctor p => Yoneda (Yoneda p) a b -> Yoneda p a b
join'' (Yoneda φ) = Yoneda (\ f g -> copure'' (φ f g))

copure'' :: Yoneda p a b -> p a b
copure'' (Yoneda φ) = φ id id

cut'' :: Yoneda p a b -> Yoneda (Yoneda p) a b
cut'' y = Yoneda (\ f g -> dimap f g y)

instance (Category p, Profunctor p) => Category (Yoneda p) where
    id = Yoneda (\ f g -> dimap f g id)
    Yoneda φ . Yoneda χ = Yoneda (\ f g -> φ id g . χ f id)

instance Profunctor (Yoneda p) where
    dimap f g (Yoneda φ) = Yoneda (compose2 φ (f .) (. g))

instance Lift f p => Lift f (Yoneda p) where
    lift (Yoneda φ) = Yoneda (\ f g -> dimap f g (lift (φ id id)))

{-
newtype CofreeLiftC c p a b = CofreeLiftC { unCofreeLiftC :: ∀ f . c f => p (f a) (f b) }

instance (Profunctor p, ∀ g . c g => Functor g) => Profunctor (CofreeLiftC c p) where
    dimap f g (CofreeLiftC φ) = CofreeLiftC (dimap (fmap f) (fmap g) φ)

instance (Profunctor p, ∀ g . c g => Functor g, ∀ g . c g => c (Compose g f)) => Lift f (CofreeLiftC c p) where
    lift (CofreeLiftC φ) = CofreeLiftC (dimap Compose getCompose φ)

unCofreeLiftC' :: (Profunctor p, c Identity) => CofreeLiftC c p a b -> p a b
unCofreeLiftC' (CofreeLiftC φ) = dimap Identity runIdentity φ
-}
