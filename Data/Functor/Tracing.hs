module Data.Functor.Tracing where

import Data.Functor.Identity (Identity (..))
import Data.Tagged (Tagged (..))

class Tracing f where
    trace :: (f a -> f b) -> a -> b

deriving via Identity instance Tracing (Tagged t)

instance Tracing Identity where
    trace f = runIdentity . f . Identity

instance Tracing ((,) c) where
    trace f = \ a -> let (c, b) = f (c, a) in b

instance Tracing (Either c) where
    trace f = go . Right where go = either (go . Left) id . f
