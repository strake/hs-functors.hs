module Control.Comonad.Trans.Cofree where

import Control.Applicative
import Control.Comonad

newtype CofreeT f ɯ a = CofreeT { cofreeT :: ɯ (a, f (CofreeT f ɯ a)) }
  deriving (Functor, Foldable, Traversable)

instance (Functor f, Comonad ɯ) => Comonad (CofreeT f ɯ) where
    copure = fst . copure . cofreeT
    cut = CofreeT . (<<=) (liftA2 (,) CofreeT $ fmap cut . snd . copure) . cofreeT
