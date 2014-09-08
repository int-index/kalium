{-# LANGUAGE KindSignatures, DataKinds #-}
module Control.Monad.Counter where

import GHC.TypeLits
import Data.Proxy
import Control.Applicative
import Control.Monad

data Counter (n :: Nat) a = Counter Integer a | Done

instance KnownNat n => Functor (Counter n) where
    fmap f (Counter i a) = Counter i (f a)
    fmap _ _ = Done

instance KnownNat n => Applicative (Counter n) where
    pure  = Counter 0
    (<*>) = ap

instance KnownNat n => Monad (Counter n) where
    return = pure
    a >>= f = join (fmap f a) where
        join (Counter i (Counter j x))
            | m > natVal (Proxy :: Proxy n) = Done
            | otherwise = Counter m x
            where m = i+j
        join _ = Done
