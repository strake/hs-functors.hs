{-# LANGUAGE PatternSynonyms #-}
module Data.Functor.Kan.R where

import Prelude hiding (fail)
import Control.Applicative
import Control.Monad (MonadPlus (..))
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor.Identity (Identity)
import Data.Functor.Join (Join1 (..))

data R f g a = R { unR :: ∀ b . (a -> f b) -> g b }
deriving instance Functor (R f g)

to :: Functor k => (∀ a . k (f a) -> g a) -> k a -> R f g a
to f x = R (f . (<$> x))

from :: (∀ a . k a -> R f g a) -> k (f a) -> g a
from = (flip unR id .)

g :: R f g (f a) -> g a
g = flip id id . unR

type Yoneda = R Identity

type Codensity = Join1 R
pattern Codensity :: (∀ b . (a -> f b) -> f b) -> Codensity f a
pattern Codensity { codensity } = Join1 (R codensity)
{-# COMPLETE Codensity #-}

deriving instance Functor (Join1 R f)

instance Applicative (Join1 R f) where
    pure a = Codensity ($ a)
    Codensity f <*> Codensity g = Codensity (\ x -> f (\ y -> g (x . y)))

instance Monad (Join1 R f) where
    Codensity x >>= f = Codensity (x . flip (codensity . f))

instance MonadFail f => MonadFail (Codensity f) where
    fail xs = Codensity (\ _ -> fail xs)

instance Alternative p => Alternative (Codensity p) where
    empty = Codensity (pure empty)
    Codensity x <|> Codensity y = Codensity (liftA2 (<|>) x y)

instance Alternative p => MonadPlus (Codensity p)

instance MonadTrans Codensity where lift xm = Codensity (xm >>=)
