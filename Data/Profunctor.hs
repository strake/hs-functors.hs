{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Profunctor where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Arrow (Kleisli (..))
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Data.Bifunctor.Braided

class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap g = dimap id g

instance Profunctor (->) where
    dimap f g a = g . a . f

instance Monad m => Profunctor (Kleisli m) where
    dimap f g (Kleisli a) = Kleisli (fmap g . a . f)

class Profunctor p => Strong f p where
    strong :: p a₁ b₁ -> p a₂ b₂ -> p (f a₁ a₂) (f b₁ b₂)

infixr 3 ***
(***) :: Strong (,) p => p a₁ b₁ -> p a₂ b₂ -> p (a₁, a₂) (b₁, b₂)
(***) = strong

infixr 2 +++
(+++) :: Strong Either p => p a₁ b₁ -> p a₂ b₂ -> p (Either a₁ a₂) (Either b₁ b₂)
(+++) = strong

instance Strong (,) (->) where strong f g (x, y) = (f x, g y)

instance Monad m => Strong (,) (Kleisli m) where
    strong (Kleisli f) (Kleisli g) = Kleisli $ \ (x, y) -> liftA2 (,) (f x) (g y)

instance Strong Either (->) where
    strong f _ (Left x)  = Left (f x)
    strong _ g (Right y) = Right (g y)

instance Monad m => Strong Either (Kleisli m) where
    strong (Kleisli f) (Kleisli g) = Kleisli $ \ case Left  x -> Left  <$> f x
                                                      Right y -> Right <$> g y

class Profunctor p => Costrong f p where
    costrongL :: p (f a c) (f b c) -> p a b
    costrongR :: p (f a b) (f a c) -> p b c

    default costrongL :: Braided f => p (f a c) (f b c) -> p a b
    costrongL = costrongR . dimap braid braid

    default costrongR :: Braided f => p (f a b) (f a c) -> p b c
    costrongR = costrongL . dimap braid braid

instance Costrong (,) (->) where
    costrongL f a = let (b, c) = f (a, c) in b

instance MonadFix m => Costrong (,) (Kleisli m) where
    costrongL (Kleisli f) = Kleisli $ \ a -> fst <$> mfix (f . (,) a . snd)

instance Costrong Either (->) where
    costrongL f = let go = either id (go . f . Right) in go . f . Left

instance Monad m => Costrong Either (Kleisli m) where
    costrongL (Kleisli f) = let go = either pure (go <=< f . Right) in Kleisli (go <=< f . Left)

class Profunctor p => Closed p where
    closed :: p a b -> p (c -> a) (c -> b)

instance Closed (->) where closed = (.)
