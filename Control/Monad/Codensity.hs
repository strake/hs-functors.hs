module Control.Monad.Codensity (Codensity, unlift) where

import Data.Functor.Kan.R

unlift :: Applicative p => Codensity p a -> p a
unlift = flip codensity pure
