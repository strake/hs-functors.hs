{-# LANGUAGE RankNTypes #-}

module Control.Monad.Morph where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum

class MFunctor t where
    mmap :: (∀ a . m a -> n a) -> t m a -> t n a

class (MonadTrans t, MFunctor t) => MMonad t where
    mjoin :: Monad m => t (t m) a -> t m a
    mjoin = mbind id

    mbind :: Monad n => (∀ a . m a -> t n a) -> t m a -> t n a
    mbind f = mjoin . mmap f

instance Functor f => MFunctor (Compose f) where mmap f (Compose x) = Compose (f <$> x)
instance MFunctor (Product f) where mmap f (Pair x y) = Pair x (f y)
instance MFunctor (Sum f) where
    mmap _ (InL x) = InL x
    mmap f (InR y) = InR (f y)

instance MFunctor (ExceptT e) where mmap = mapExceptT
instance MFunctor IdentityT   where mmap = mapIdentityT
instance MFunctor MaybeT      where mmap = mapMaybeT
instance MFunctor (ReaderT r) where mmap = mapReaderT
instance MFunctor (StateT  s) where mmap = mapStateT
instance MFunctor (WriterT w) where mmap = mapWriterT

instance MMonad (ExceptT e) where mjoin (ExceptT   (ExceptT   x)) = ExceptT (join <$> x)
instance MMonad IdentityT   where mjoin (IdentityT (IdentityT x)) = IdentityT x
instance MMonad MaybeT      where mjoin (MaybeT    (MaybeT    x)) = MaybeT (join <$> x)
instance MMonad (ReaderT r) where mjoin (ReaderT f) = ReaderT (join (runReaderT . f))
instance Monoid w => MMonad (WriterT w) where
    mjoin (WriterT (WriterT x)) = WriterT ((\ ((a, u), v) -> (a, u <> v)) <$> x)
