{-# LANGUAGE RankNTypes #-}

module Control.Monad.Codensity where

import Prelude hiding (fail)
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.Trans.Class

newtype Codensity m a = Codensity { codensity :: ∀ b . (a -> m b) -> m b }

deriving instance Functor (Codensity m)

instance Applicative (Codensity p) where
    pure x = Codensity ($ x)
    Codensity x <*> Codensity y = Codensity (\ g -> x (\ f -> y (g . f)))

instance Monad (Codensity m) where
    Codensity x >>= f = Codensity (x . flip (codensity . f))

instance MonadFail f => MonadFail (Codensity f) where
    fail xs = Codensity (\ _ -> fail xs)

instance Alternative p => Alternative (Codensity p) where
    empty = Codensity (pure empty)
    Codensity x <|> Codensity y = Codensity (liftA2 (<|>) x y)

instance Alternative p => MonadPlus (Codensity p)

instance MonadTrans Codensity where lift xm = Codensity (xm >>=)

unlift :: Applicative p => Codensity p a -> p a
unlift = flip codensity pure
