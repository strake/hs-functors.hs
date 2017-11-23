module Control.Comonad.Cofree where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Data.Cotraversable
import Data.Functor.Classes

data Cofree f a = Cofree a (f (Cofree f a))
  deriving (Functor, Foldable, Traversable)

instance Alternative f => Applicative (Cofree f) where
    pure = flip Cofree empty
    (<*>) = ap

instance Alternative f => Monad (Cofree f) where
    x >>= f = join (f <$> x) where join (Cofree (Cofree a s) t) = Cofree a (s <|> join <$> t)

instance Functor f => Comonad (Cofree f) where
    copure (Cofree a _) = a
    cut ɯ@(Cofree _ t) = Cofree ɯ (cut <$> t)

instance Cotraversable f => Cotraversable (Cofree f) where
    cosequence = liftA2 Cofree (fmap (\ (Cofree a _) -> a))
                               (fmap cosequence . collect (\ (Cofree _ t) -> t))

instance Eq1 f => Eq1 (Cofree f) where
    liftEq (==) (Cofree a s) (Cofree b t) = a == b && (liftEq . liftEq) (==) s t

instance Ord1 f => Ord1 (Cofree f) where
    liftCompare cmp (Cofree a s) (Cofree b t) = a `cmp` b <> (liftCompare . liftCompare) cmp s t

instance Read1 f => Read1 (Cofree f) where
    liftReadPrec rp rl =
        readBinaryWith rp (liftReadPrec (liftReadPrec rp rl)
                                        (liftReadListPrec rp rl)) "Cofree"  Cofree

instance Show1 f => Show1 (Cofree f) where
    liftShowsPrec sp sl n (Cofree a t) =
        showsBinaryWith sp (liftShowsPrec (liftShowsPrec sp sl)
                                          (liftShowList sp sl)) "Cofree" n a t
