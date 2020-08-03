{-# LANGUAGE ExistentialQuantification #-}
module Control.Applicative.Free where

import Prelude hiding (map)
import Control.Comonad

data Free f a = Pure a | ∀ b . Free (f b) (Free f (b -> a))

instance Functor (Free f) where
    fmap φ (Pure a) = Pure (φ a)
    fmap φ (Free x f) = Free x (fmap φ <$> f)

instance Applicative (Free f) where
    pure = Pure
    Pure f <*> y = f <$> y
    Free x f <*> y = Free x (flip <$> f <*> y)

instance Comonad ɯ => Comonad (Free ɯ) where
    copure (Pure a) = a
    copure (Free x f) = copure f $ copure x
    cut (Pure a) = Pure (Pure a)
    cut (Free x f) = Free (cut x) (flip Free <<= f)

fold :: Applicative p => (∀ a . f a -> p a) -> Free f a -> p a
fold _ (Pure a) = pure a
fold φ (Free x f) = flip id <$> φ x <*> fold φ f

lift :: f a -> Free f a
lift x = Free x (Pure id)

iter :: Functor f => (f a -> a) -> Free f a -> a
iter _ (Pure a) = a
iter φ (Free x f) = φ (iter φ . (f <*>) . pure <$> x)

map :: (∀ a . f a -> g a) -> Free f a -> Free g a
map _ (Pure a) = Pure a
map φ (Free x f) = Free (φ x) (map φ f)
