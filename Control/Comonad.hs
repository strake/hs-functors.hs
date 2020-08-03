{-# LANGUAGE NoImplicitPrelude #-}

module Control.Comonad where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Identity
import Control.Semigroupoid (Semigroupoid)
import qualified Control.Semigroupoid as S
import Data.Function (($), fix, flip)
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Semigroup (Arg (..), Semigroup (..))
import Data.Monoid (Monoid (..))

infixl 1 =>>
infixr 1 <<=, =>=, =<=

class Functor ɯ => Cobind ɯ where
    {-# MINIMAL (<<=) | cut #-}

    (<<=) :: (ɯ a -> b) -> ɯ a -> ɯ b
    (<<=) f = fmap f . cut

    cut :: ɯ a -> ɯ (ɯ a)
    cut = (=>> id)

class Cobind ɯ => Comonad ɯ where
    copure :: ɯ a -> a

(=>>) :: Cobind ɯ => ɯ a -> (ɯ a -> b) -> ɯ b
(=>>) = flip (<<=)

(=>=) :: Cobind ɯ => (ɯ a -> b) -> (ɯ b -> c) -> ɯ a -> c
f =>= g = g . (<<=) f

(=<=) :: Cobind ɯ => (ɯ b -> c) -> (ɯ a -> b) -> ɯ a -> c
(=<=) = flip (=>=)

wfix :: Cobind ɯ => (ɯ a -> a) -> ɯ a
wfix f = fix (fmap f . cut)

instance Cobind Identity where cut = Identity
instance Comonad Identity where copure = runIdentity

instance Cobind NonEmpty where
    cut (x:|xs) = (x:|xs) :| go xs
      where go [] = []
            go (x:xs) = (x:|xs) : go xs

instance Comonad NonEmpty where
    copure = head

instance (Semigroup m) => Cobind ((->) m) where
    cut f x y = f (x <> y)

instance (Monoid m) => Comonad ((->) m) where
    copure = ($ mempty)

instance Cobind ((,) a) where
    cut (a, b) = (a, (a, b))

instance Comonad ((,) a) where
    copure (_, b) = b

instance Cobind (Arg a) where
    cut (Arg a b) = Arg a (Arg a b)

instance Comonad (Arg a) where
    copure (Arg _ b) = b

instance Cobind ɯ => Cobind (IdentityT ɯ) where
    cut (IdentityT x) = IdentityT (IdentityT <$> cut x)

instance Comonad ɯ => Comonad (IdentityT ɯ) where
    copure = copure . runIdentityT

newtype Cokleisli ɯ a b = Cokleisli { runCokleisli :: ɯ a -> b }
    deriving (Functor, Applicative, Monad)

instance Cobind ɯ => Semigroupoid (Cokleisli ɯ) where
    Cokleisli f . Cokleisli g = Cokleisli (f =<= g)

instance Comonad ɯ => Category (Cokleisli ɯ) where
    id = Cokleisli copure
    (.) = (S..)
