module Control.Monad.Trans.Free where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Compose
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Functor.Sum
import Text.Read

newtype FreeT f m a = FreeT { freeT :: m (Sum Identity (Compose f (FreeT f m)) a) }
  deriving (Functor, Foldable, Traversable)

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure = FreeT . pure . InL . Identity
    (<*>) = ap

instance (Functor f, Monad m) => Monad (FreeT f m) where
    FreeT x >>= f = FreeT $ x >>= \ case
         InL (Identity a) -> freeT (f a)
         InR (Compose yf) -> pure ((InR . Compose) ((>>= f) <$> yf))

instance (Functor f, MonadPlus m) => Alternative (FreeT f m) where
    empty = FreeT mzero
    FreeT x <|> FreeT y = FreeT (x `mplus` y)

instance (Functor f, MonadPlus m) => MonadPlus (FreeT f m) where
    mzero = FreeT mzero
    FreeT x `mplus` FreeT y = FreeT (x `mplus` y)

instance (Eq1 f, Eq1 g) => Eq1 (FreeT f g) where
    liftEq (==) (FreeT x) (FreeT y) = (liftEq . liftEq) (==) x y

instance (Ord1 f, Ord1 g) => Ord1 (FreeT f g) where
    liftCompare cmp (FreeT x) (FreeT y) = (liftCompare . liftCompare) cmp x y

instance (Read1 f, Read1 g) => Read1 (FreeT f g) where
    liftReadPrec rp rl = readUnaryWith (liftReadPrec (liftReadPrec rp rl)
                                                     (liftReadListPrec rp rl)) "FreeT" FreeT

instance (Show1 f, Show1 g) => Show1 (FreeT f g) where
    liftShowsPrec sp sl n =
        showsUnaryWith (liftShowsPrec (liftShowsPrec sp sl)
                                      (liftShowList sp sl)) "FreeT" n . freeT

instance (Eq a, Eq1 f, Eq1 g) => Eq (FreeT f g a) where (==) = eq1
instance (Ord a, Ord1 f, Ord1 g) => Ord (FreeT f g a) where compare = compare1
instance (Read a, Read1 f, Read1 g) => Read (FreeT f g a) where readPrec = readPrec1
instance (Show a, Show1 f, Show1 g) => Show (FreeT f g a) where showsPrec = showsPrec1

instance MonadTrans (FreeT f) where lift = FreeT . fmap (InL . Identity)

type Free f = FreeT f Identity

raiseT :: (Functor f, Monad m) => f a -> FreeT f m a
raiseT = FreeT . pure . InR . Compose . fmap pure
