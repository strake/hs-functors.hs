module Control.Monad.Morph where

import Control.Monad
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS.Lazy as L
import Control.Monad.Trans.RWS.Strict as S
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

class MFunctor t => MSemimonad t where
    mjoin :: Functor m => t (t m) a -> t m a
    mjoin = mbind id

    mbind :: Functor n => (∀ a . m a -> t n a) -> t m a -> t n a
    mbind f = mjoin . mmap f

class MPointed t where
    mpure :: Functor m => m a -> t m a

instance Functor f => MFunctor (Compose f) where mmap f (Compose x) = Compose (f <$> x)
instance MFunctor (Product f) where mmap f (Pair x y) = Pair x (f y)
instance MFunctor (Sum f) where
    mmap _ (InL x) = InL x
    mmap f (InR y) = InR (f y)

instance MFunctor (ExceptT e)   where mmap = mapExceptT
instance MFunctor IdentityT     where mmap = mapIdentityT
instance MFunctor MaybeT        where mmap = mapMaybeT
instance MFunctor (L.RWST r w s)where mmap = L.mapRWST
instance MFunctor (S.RWST r w s)where mmap = S.mapRWST
instance MFunctor (ReaderT r)   where mmap = mapReaderT
instance MFunctor (L.StateT s)  where mmap = L.mapStateT
instance MFunctor (S.StateT s)  where mmap = S.mapStateT
instance MFunctor (L.WriterT w) where mmap = L.mapWriterT
instance MFunctor (S.WriterT w) where mmap = S.mapWriterT
instance MFunctor (AccumT c)    where mmap = mapAccumT

instance MSemimonad (Sum f) where
    mjoin (InL x) = InL x
    mjoin (InR y) = y

instance MSemimonad (ExceptT e) where mjoin (ExceptT   (ExceptT   x)) = ExceptT (join <$> x)
instance MSemimonad IdentityT   where mjoin (IdentityT (IdentityT x)) = IdentityT x
instance MSemimonad MaybeT      where mjoin (MaybeT    (MaybeT    x)) = MaybeT (join <$> x)
instance MSemimonad (ReaderT r) where mjoin (ReaderT f) = ReaderT (join (runReaderT . f))
instance Monoid w => MSemimonad (L.WriterT w) where
    mjoin (L.WriterT (L.WriterT x)) = L.WriterT ((\ ((a, u), v) -> (a, u <> v)) <$> x)
instance Monoid w => MSemimonad (S.WriterT w) where
    mjoin (S.WriterT (S.WriterT x)) = S.WriterT ((\ ((a, u), v) -> (a, u <> v)) <$> x)

instance MPointed (Sum f) where mpure = InR

instance MPointed (AccumT c) where mpure x = AccumT (\ c -> flip (,) c <$> x)
instance MPointed (ExceptT e) where mpure = ExceptT . fmap pure
instance MPointed IdentityT where mpure = IdentityT
instance MPointed MaybeT where mpure = MaybeT . fmap pure
instance Monoid w => MPointed (L.RWST r w s) where mpure x = L.RWST (\ _ s -> (\ a -> (a, s, mempty)) <$> x)
instance Monoid w => MPointed (S.RWST r w s) where mpure x = S.RWST (\ _ s -> (\ a -> (a, s, mempty)) <$> x)
instance MPointed (ReaderT r) where mpure = ReaderT . pure
instance MPointed (L.StateT w) where mpure x = L.StateT (\ s -> flip (,) s <$> x)
instance MPointed (S.StateT w) where mpure x = S.StateT (\ s -> flip (,) s <$> x)
instance Monoid w => MPointed (L.WriterT w) where mpure = L.WriterT . fmap (flip (,) mempty)
instance Monoid w => MPointed (S.WriterT w) where mpure = S.WriterT . fmap (flip (,) mempty)
