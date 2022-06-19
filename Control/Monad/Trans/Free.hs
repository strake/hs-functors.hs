{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Trans.Free where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor.Biff
import Data.Functor.Compose
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Functor.Join
import Text.Read

newtype FreeT c f o a = FreeT { freeT :: o (Join (Biff c Identity (Compose f (FreeT c f o))) a) }
deriving instance (Bifunctor c, Functor f, Functor o) => Functor (FreeT c f o)
deriving instance (Bifoldable c, Foldable f, Foldable o) => Foldable (FreeT c f o)
deriving instance (Bitraversable c, Traversable f, Traversable o) => Traversable (FreeT c f o)
instance (Eq2 c, Eq1 f, Eq1 g, Eq a) => Eq (FreeT c f g a) where (==) = eq1
instance (Ord2 c, Ord1 f, Ord1 g, Ord a) => Ord (FreeT c f g a) where compare = compare1
instance (Read2 c, Read1 f, Read1 g, Read a) => Read (FreeT c f g a) where readPrec = readPrec1
instance (Show2 c, Show1 f, Show1 g, Show a) => Show (FreeT c f g a) where showsPrec = showsPrec1
instance (Eq2 c, Eq1 f, Eq1 g) => Eq1 (FreeT c f g) where
    liftEq (==) (FreeT x) (FreeT y) = (liftEq . liftEq) (==) x y
instance (Ord2 c, Ord1 f, Ord1 g) => Ord1 (FreeT c f g) where
    liftCompare cmp (FreeT x) (FreeT y) = (liftCompare . liftCompare) cmp x y
instance (Read2 c, Read1 f, Read1 g) => Read1 (FreeT c f g) where
    liftReadPrec rp rpl = FreeT <$> liftReadPrec (liftReadPrec rp rpl) (liftReadListPrec rp rpl)
instance (Show2 c, Show1 f, Show1 g) => Show1 (FreeT c f g) where
    liftShowsPrec sp sl n = liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl) n . freeT

type Free c f = FreeT c f Identity

instance (Bifunctor c, Functor f) => MFunctorPre (FreeT c f) where
    mmapPre f = FreeT . fmap (Join . Biff . bimap id (mmap (mmapPre f)) . unBiff . unJoin) . f . freeT

instance (Bifunctor c, Functor f) => MFunctorPost (FreeT c f) where
    mmapPost f = FreeT . f . fmap (Join . Biff . bimap id (mmap (mmapPost f)) . unBiff . unJoin) . freeT

mapT :: (Functor o, Functor g, Bifunctor c) => (∀ a . f a -> g a) -> FreeT c f o a -> FreeT c g o a
mapT f = FreeT . fmap (Join . Biff . bimap id (Compose . fmap (mapT f) . f . getCompose) . unBiff . unJoin) . freeT

instance (Functor f, Monad m) => Applicative (FreeT Either f m) where
    pure = FreeT . pure . Join . Biff . Left . Identity
    (<*>) = ap

instance (Functor f, Monad m) => Monad (FreeT Either f m) where
    FreeT x >>= f = FreeT $ unBiff . unJoin <$> x >>= \ case
         Left  (Identity a) -> freeT (f a)
         Right (Compose yf) -> (pure . Join . Biff) ((Right . Compose) ((>>= f) <$> yf))

instance (Functor f, MonadPlus m) => Alternative (FreeT Either f m) where
    empty = FreeT mzero
    FreeT x <|> FreeT y = FreeT (x `mplus` y)

instance (Functor f, MonadPlus m) => MonadPlus (FreeT Either f m)

instance MonadTrans (FreeT Either f) where lift = FreeT . fmap (Join . Biff . Left . Identity)

liftT :: (Functor f, Monad m) => f a -> FreeT Either f m a
liftT = FreeT . pure . Join . Biff . Right . Compose . fmap pure

instance (Functor f, Comonad ɯ) => Comonad (FreeT (,) f ɯ) where
    copure = runIdentity . fst . unBiff . unJoin . copure . freeT
    (<<=) f = FreeT . (=>> \ ɯ -> (Join . Biff) ((Identity . f) (FreeT ɯ), Compose $ (=>> f) <$> (getCompose . snd . unBiff . unJoin . copure) ɯ)) . freeT

instance ComonadTrans (FreeT (,) f) where colift = fmap (runIdentity . fst . unBiff . unJoin) . freeT

coliftT :: (Functor f, Comonad ɯ) => FreeT (,) f ɯ a -> f a
coliftT = fmap copure . getCompose . snd . unBiff . unJoin . copure . freeT
