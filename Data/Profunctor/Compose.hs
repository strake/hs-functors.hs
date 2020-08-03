{-# LANGUAGE ExistentialQuantification #-}
module Data.Profunctor.Compose where

data Procompose p q a b = ∀ c . Procompose (p c b) (q a c)
