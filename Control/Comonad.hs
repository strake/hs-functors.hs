{-# LANGUAGE NoImplicitPrelude #-}

module Control.Comonad where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Identity
import Data.Function (($), fix, flip)
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Semigroup (Arg (..), Semigroup (..))
import Data.Monoid (Monoid (..))

infixl 1 =>>
infixr 1 <<=, =>=, =<=

class Functor ɯ => Comonad ɯ where
    {-# MINIMAL copure, (cut | (<<=)) #-}

    copure :: ɯ a -> a

    cut :: ɯ a -> ɯ (ɯ a)
    cut = (<<=) id

    (<<=) :: (ɯ a -> b) -> ɯ a -> ɯ b
    (<<=) f = fmap f . cut

(=>>) :: Comonad ɯ => ɯ a -> (ɯ a -> b) -> ɯ b
(=>>) = flip (<<=)

(=>=) :: Comonad ɯ => (ɯ a -> b) -> (ɯ b -> c) -> ɯ a -> c
f =>= g = g . (<<=) f

(=<=) :: Comonad ɯ => (ɯ b -> c) -> (ɯ a -> b) -> ɯ a -> c
(=<=) = flip (=>=)

wfix :: Comonad ɯ => (ɯ a -> a) -> ɯ a
wfix f = fix (fmap f . cut)

instance Comonad Identity where
    copure = runIdentity
    cut = Identity

instance Comonad NonEmpty where
    copure = head
    cut (x:|xs) = (x:|xs) :| go xs
      where go [] = []
            go (x:xs) = (x:|xs) : go xs

instance (Semigroup m, Monoid m) => Comonad ((->) m) where
    copure = ($ mempty)
    cut f x y = f (x <> y)

instance Comonad ((,) a) where
    copure (_, b) = b
    cut (a, b) = (a, (a, b))

instance Comonad (Arg a) where
    copure (Arg _ b) = b
    cut (Arg a b) = Arg a (Arg a b)

instance Comonad ɯ => Comonad (IdentityT ɯ) where
    copure = copure . runIdentityT
    cut (IdentityT x) = IdentityT (IdentityT <$> cut x)

newtype Cokleisli ɯ a b = Cokleisli { runCokleisli :: ɯ a -> b }
  deriving stock (Functor)
  deriving (Applicative, Monad) via ((->) (ɯ a))

instance Comonad ɯ => Category (Cokleisli ɯ) where
    id = Cokleisli copure
    Cokleisli f . Cokleisli g = Cokleisli (f =<= g)
