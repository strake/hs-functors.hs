module Control.Monad.Trans.Compose where

import Control.Applicative
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Class
import Data.Functor.Classes
import qualified Data.Functor.Contravariant as Contravar
import Data.Semigroup (Semigroup (..))

newtype ComposeT s t m a = ComposeT { getComposeT :: s (t m) a }
  deriving stock (Foldable, Functor, Traversable)
  deriving (Applicative, Monad, Alternative, MonadPlus, Eq1, Ord1, Read1, Show1, Contravar.Functor) via s (t m)
  deriving (Eq, Ord, Bounded, Read, Show, Semigroup, Monoid) via s (t m) a

instance (MFunctor s, MonadTrans s, MonadTrans t) => MonadTrans (ComposeT s t) where
    lift = ComposeT . mmap lift . lift

instance (MFunctor s, MFunctor t) => MFunctor (ComposeT s t) where
    mmap f (ComposeT x) = ComposeT (mmap (mmap f) x)
