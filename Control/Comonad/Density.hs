{-# LANGUAGE ExistentialQuantification #-}

module Control.Comonad.Density where

import Control.Comonad
import Control.Comonad.Trans.Class

data Density f a = ∀ b . Density (f b -> a) (f b)

deriving instance Functor (Density f)

instance Applicative f => Applicative (Density f) where
    pure a = Density (pure a) (pure ())
    Density f x <*> Density g y = pure ((f x) (g y))

instance Comonad (Density f) where
    copure (Density f x) = f x
    cut ɯ@(Density _ x) = Density (pure ɯ) x

instance ComonadTrans Density where
    colift (Density f x) = f <<= x

lift :: Comonad ɯ => ɯ a -> Density ɯ a
lift = Density copure
