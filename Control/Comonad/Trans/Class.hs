module Control.Comonad.Trans.Class where

import Control.Comonad
import Control.Monad.Trans.Identity

class ComonadTrans t where
    colift :: Comonad ɯ => t ɯ a -> ɯ a

instance ComonadTrans IdentityT where colift = runIdentityT
