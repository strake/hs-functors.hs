module Data.Cotraversable where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Proxy

-- | Laws:
--
-- * @'cosequence' = 'collect' 'id'@
--
-- * @'cosequence' . 'cosequence' = 'id'@
--
-- * @'collect' f = 'cosequence' . 'fmap' f@
--
-- * @'fmap' f = 'runIdentity' . 'collect' ('Identity' . f)@
--
-- * @'fmap' 'cosequence' . 'collect' f = 'getCompose' . 'collect' ('Compose' . f)@
class Functor f => Cotraversable f where
    {-# MINIMAL collect | cosequence | cotraverse #-}

    collect :: Functor g => (a -> f b) -> g a -> f (g b)
    collect f = cosequence . fmap f

    cosequence :: Functor g => g (f a) -> f (g a)
    cosequence = collect id

    cotraverse :: Functor g => (g a -> b) -> g (f a) -> f b
    cotraverse f = fmap f . cosequence

instance Cotraversable Identity where
    cosequence = Identity . fmap runIdentity

instance Cotraversable ((->) r) where
    cosequence x a = ($ a) <$> x

instance Cotraversable Proxy where
    cosequence _ = Proxy

instance Cotraversable f => Cotraversable (IdentityT f) where
    cosequence = IdentityT . collect runIdentityT

instance Cotraversable f => Cotraversable (ReaderT r f) where
    cosequence x = ReaderT $ \ a -> flip runReaderT a `collect` x

instance Cotraversable f => Cotraversable (Reverse f) where
    cosequence = Reverse . collect getReverse

instance Cotraversable f => Cotraversable (Backwards f) where
    cosequence = Backwards . collect forwards

instance (Cotraversable f, Cotraversable g) => Cotraversable (Compose f g) where
    cosequence = Compose . fmap cosequence . collect getCompose

instance (Cotraversable f, Cotraversable g) => Cotraversable (Product f g) where
    cosequence = liftA2 Pair (collect fstP) (collect sndP)
      where fstP (Pair a _) = a
            sndP (Pair _ b) = b
