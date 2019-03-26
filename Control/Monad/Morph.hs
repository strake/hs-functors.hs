module Control.Monad.Morph where

import Control.Monad
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy as L
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Writer.Lazy as L
import Control.Monad.Trans.Writer.Strict as S
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Data.Monoid ((<>))

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

instance MFunctor (ExceptT e)   where mmap = mapExceptT
instance MFunctor IdentityT     where mmap = mapIdentityT
instance MFunctor MaybeT        where mmap = mapMaybeT
instance MFunctor (ReaderT r)   where mmap = mapReaderT
instance MFunctor (L.StateT s)  where mmap = L.mapStateT
instance MFunctor (S.StateT s)  where mmap = S.mapStateT
instance MFunctor (L.WriterT w) where mmap = L.mapWriterT
instance MFunctor (S.WriterT w) where mmap = S.mapWriterT
instance MFunctor (AccumT c)    where mmap = mapAccumT

instance MMonad (ExceptT e) where mjoin (ExceptT   (ExceptT   x)) = ExceptT (join <$> x)
instance MMonad IdentityT   where mjoin (IdentityT (IdentityT x)) = IdentityT x
instance MMonad MaybeT      where mjoin (MaybeT    (MaybeT    x)) = MaybeT (join <$> x)
instance MMonad (ReaderT r) where mjoin (ReaderT f) = ReaderT (join (runReaderT . f))
instance Monoid w => MMonad (L.WriterT w) where
    mjoin (L.WriterT (L.WriterT x)) = L.WriterT ((\ ((a, u), v) -> (a, u <> v)) <$> x)
instance Monoid w => MMonad (S.WriterT w) where
    mjoin (S.WriterT (S.WriterT x)) = S.WriterT ((\ ((a, u), v) -> (a, u <> v)) <$> x)
