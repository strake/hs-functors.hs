{-# LANGUAGE ExistentialQuantification #-}

module Control.Comonad.Density (Density, lift) where

import Control.Comonad
import Data.Functor.Kan.L

lift :: Comonad ɯ => ɯ a -> Density ɯ a
lift = Density copure
