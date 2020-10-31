module Data.Bifunctor.Associative where

import Control.Arrow ((|||))
import Data.Bifunctor (Bifunctor (..))

class Bifunctor p => Associative p where
    assoc :: p (p a b) c -> p a (p b c)
    disassoc :: p a (p b c) -> p (p a b) c

instance Associative (,) where
    assoc ((a, b), c) = (a, (b, c))
    disassoc (a, (b, c)) = ((a, b), c)

instance Associative Either where
    assoc = (Left ||| Right . Left) ||| Right . Right
    disassoc = Left . Left ||| (Left . Right ||| Right)
