{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Profunctor where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Arrow (Kleisli (..))
import Control.Category
import Control.Comonad
import Control.Monad
import Control.Monad.Fix
import Data.Bifunctor.Biff
import Data.Bifunctor.Braided
import Data.Bifunctor.Tannen
import Data.Cotraversable

class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap g = dimap id g

infixr 1 ^>>, >>^, <<^, ^<<

(^>>) :: Profunctor p => (a -> b) -> p b c -> p a c
(^>>) = lmap

(>>^) :: Profunctor p => p a b -> (b -> c) -> p a c
(>>^) = flip rmap

(<<^) :: Profunctor p => p b c -> (a -> b) -> p a c
(<<^) = flip lmap

(^<<) :: Profunctor p => (b -> c) -> p a b -> p a c
(^<<) = rmap

instance Profunctor (->) where
    dimap f g a = g . a . f

instance Functor f => Profunctor (Kleisli f) where
    dimap f g (Kleisli a) = Kleisli (fmap g . a . f)

instance Functor f => Profunctor (Cokleisli f) where
    dimap f g (Cokleisli a) = Cokleisli (g . a . fmap f)

instance (Profunctor p, Functor f, Functor g) => Profunctor (Biff p f g) where
    dimap f g = Biff . dimap (fmap f) (fmap g) . unBiff

instance (Functor f, Profunctor p) => Profunctor (Tannen f p) where
    dimap f g = Tannen . fmap (dimap f g) . unTannen


class Profunctor p => Lift f p where
    lift :: p a b -> p (f a) (f b)

instance Functor f => Lift f (->) where lift = fmap

instance (Traversable f, Applicative p) => Lift f (Kleisli p) where
    lift = Kleisli . traverse . runKleisli

instance (Cotraversable f, Functor ɯ) => Lift f (Cokleisli ɯ) where
    lift = Cokleisli . cotraverse . runCokleisli

instance (Lift φ p, Functor f, Applicative g, Traversable φ, Cotraversable φ) => Lift φ (Biff p f g) where
    lift = Biff . dimap cosequence sequenceA . lift . unBiff

instance (Cotraversable m) => Lift ((->) a) (Kleisli m) where
    lift (Kleisli afb) = Kleisli $ \ xa -> cosequence $ afb . xa

instance (Functor f) => Lift ((->) a) (Cokleisli f) where
    lift (Cokleisli f) = Cokleisli $ \ fs a -> f $ ($ a) <$> fs

instance (Functor f, Cotraversable g, Lift ((->) a) p) => Lift ((->) a) (Biff p f g) where
    lift = Biff . dimap (flip $ fmap . flip id) cosequence . lift . unBiff

instance (Lift f p, Functor g) => Lift f (Tannen g p) where
    lift = Tannen . fmap lift . unTannen

instance Applicative f => Lift f Tagged where
    lift = Tagged . pure . unTagged


class Profunctor p => Colift f p where
    colift :: p (f a) (f b) -> p a b

instance Colift ((,) c) (->) where
    colift f a = let (c, b) = f (c, a) in b

instance MonadFix m => Colift ((,) c) (Kleisli m) where
    colift (Kleisli f) = Kleisli $ \ a -> snd <$> mfix (f . flip (,) a . fst)

instance Functor ɯ => Colift ((,) c) (Cokleisli ɯ) where
    colift (Cokleisli f) = Cokleisli $ \ a -> snd $ fix (f . flip fmap a . (,) . fst)

instance Colift (Either c) (->) where
    colift f = let go = either (go . f . Left) id in go . f . Right

instance Monad m => Colift (Either c) (Kleisli m) where
    colift (Kleisli f) = let go = either (go <=< f . Left) pure in Kleisli (go <=< f . Right)

instance Functor f => Colift (Either c) (Cokleisli f) where
    colift (Cokleisli f) = Cokleisli (go . fmap Right)
      where go ɯ = case f ɯ of Left  b -> go (Left  b <$ ɯ)
                               Right c -> c

instance (Colift f p, Functor g) => Colift f (Tannen g p) where
    colift = Tannen . fmap colift . unTannen


{-# DEPRECATED #-}
class Profunctor p => Strong f p where
    strong :: p a₁ b₁ -> p a₂ b₂ -> p (f a₁ a₂) (f b₁ b₂)

infixr 3 ***, &&&

(***) :: Strong (,) p => p a₁ b₁ -> p a₂ b₂ -> p (a₁, a₂) (b₁, b₂)
(***) = strong

(&&&) :: Strong (,) p => p a b₁ -> p a b₂ -> p a (b₁, b₂)
f &&& g = f *** g <<^ join (,)

infixr 2 +++, |||

(+++) :: Strong Either p => p a₁ b₁ -> p a₂ b₂ -> p (Either a₁ a₂) (Either b₁ b₂)
(+++) = strong

(|||) :: Strong Either p => p a₁ b -> p a₂ b -> p (Either a₁ a₂) b
f ||| g = either id id ^<< f +++ g

instance Strong (,) (->) where strong f g (x, y) = (f x, g y)

instance Applicative p => Strong (,) (Kleisli p) where
    strong (Kleisli f) (Kleisli g) = Kleisli $ \ (x, y) -> liftA2 (,) (f x) (g y)

instance Strong Either (->) where
    strong f _ (Left x)  = Left (f x)
    strong _ g (Right y) = Right (g y)

instance Functor f => Strong Either (Kleisli f) where
    strong (Kleisli f) (Kleisli g) = Kleisli $ \ case Left  x -> Left  <$> f x
                                                      Right y -> Right <$> g y

instance Comonad ɯ => Strong Either (Cokleisli ɯ) where
    strong (Cokleisli f) (Cokleisli g) =
        (\ a -> Left  . f . (a <$)) |||
        (\ a -> Right . g . (a <$)) ^>> Cokleisli (copure <*> void)


{-# DEPRECATED #-}
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

instance Functor f => Costrong Either (Cokleisli f) where
    costrongL (Cokleisli f) = Cokleisli (go . fmap Left)
      where go ɯ = case f ɯ of Left  b -> b
                               Right c -> go (Right c <$ ɯ)


{-# DEPRECATED #-}
class Profunctor p => Closed f p where
    closed :: p a b -> p (f a) (f b)

instance Functor f => Closed f (->) where closed = fmap

instance (Traversable f, Applicative p) => Closed f (Kleisli p) where
    closed = Kleisli . traverse . runKleisli

instance (Cotraversable f, Functor ɯ) => Closed f (Cokleisli ɯ) where
    closed = Cokleisli . cotraverse . runCokleisli
