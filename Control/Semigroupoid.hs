module Control.Semigroupoid where

import Prelude ()
import qualified Control.Category as Base
import Control.Category.Dual (Dual (..))
import Data.Functor.Const (Const (..))
import Data.Tagged (Tagged (..))

-- | 'Category' sans 'id'
--
-- Laws:
--
-- * @f '.' (g '.' h) = (f '.' g) '.' h@
class Semigroupoid κ where
    (.) :: κ b c -> κ a b -> κ a c

instance {-# OVERLAPPABLE #-} Base.Category κ => Semigroupoid κ where
    (.) = (Base..)

instance Semigroupoid (,) where
    (_, c) . (a, _) = (a, c)

instance Semigroupoid Const where
    _ . Const a = Const a

instance Semigroupoid Tagged where
    Tagged c . _ = Tagged c

instance Semigroupoid κ => Semigroupoid (Dual κ) where
    Dual f . Dual g = Dual (g . f)
