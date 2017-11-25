module Control.Monad.Free.Church where

import Prelude hiding (map)

import Control.Comonad
import Control.Monad
import qualified Control.Monad.Free as NonChurch
import Data.Cotraversable
import qualified Data.Foldable as F (fold)
import Data.Functor.Classes

newtype Free f a = Free { free :: ∀ b . (a -> b) -> (f b -> b) -> b }
  deriving (Functor)

instance Foldable f => Foldable (Free f) where foldMap f (Free x) = x f F.fold
instance Traversable f => Traversable (Free f) where
    traverse f = fmap toChurch . traverse f . fromChurch

instance Functor f => Applicative (Free f) where
    pure a = Free (\ f _ -> f a)
    Free x <*> Free y = Free $ \ f g -> x (\ a -> y (f . a) g) g

instance Functor f => Monad (Free f) where
    Free x >>= f = x f wrap

instance Eq1 f => Eq1 (Free f) where
    liftEq (==) x y = liftEq (==) (fromChurch x) (fromChurch y)

instance Ord1 f => Ord1 (Free f) where
    liftCompare cmp x y = liftCompare cmp (fromChurch x) (fromChurch y)

toChurch :: Functor f => NonChurch.Free f a -> Free f a
toChurch = \ x -> Free (\ f g -> go f g x)
  where go f g = \ case NonChurch.Pure a -> f a
                        NonChurch.Free t -> g (go f g <$> t)

fromChurch :: Free f a -> NonChurch.Free f a
fromChurch (Free x) = x NonChurch.Pure NonChurch.Free

wrap :: Functor f => f (Free f a) -> Free f a
wrap x = Free $ \ f g -> g ((\ (Free y) -> y f g) <$> x)

lift :: Functor f => f a -> Free f a
lift = wrap . fmap pure

map :: Functor g => (∀ a . f a -> g a) -> Free f a -> Free g a
map f (Free x) = x pure (wrap . f)

fold :: Monad m => (∀ a . f a -> m a) -> Free f a -> m a
fold f (Free x) = x pure (join . f)

iter :: Functor f => (f a -> a) -> Free f a -> a
iter f (Free x) = x id f

iterA :: (Functor f, Applicative p) => (f (p a) -> p a) -> Free f a -> p a
iterA f (Free x) = x pure f

unfold :: Functor f => (b -> Either a (f b)) -> b -> Free f a
unfold φ b = Free $ \ f g -> either f (g . fmap ((\ (Free x) -> x f g) . unfold φ)) (φ b)

unfoldM :: (Traversable f, Monad m) => (b -> m (Either a (f b))) -> b -> m (Free f a)
unfoldM φ = fmap toChurch . NonChurch.unfoldM φ

unfoldW :: (Cotraversable f, Comonad ɯ) => (ɯ b -> Either a (f b)) -> ɯ b -> Free f a
unfoldW φ = toChurch . NonChurch.unfoldW φ
