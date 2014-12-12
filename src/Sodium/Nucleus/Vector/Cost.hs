{-# LANGUAGE RecordWildCards #-}
module Sodium.Nucleus.Vector.Cost where

import Data.List
import Data.Ord
import Data.Monoid
import Data.Function
import Control.Applicative
import Control.Monad.Writer

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap

data Cost = Cost
    { _countAccess :: Sum Integer
    , _countPrimary :: Sum Integer
    , _countLambda :: Sum Integer
    , _countBeta :: Sum Integer
    } deriving (Eq)

instance Monoid Cost where
    mempty = Cost mempty mempty mempty mempty
    mappend c1 c2 = Cost
        (on mappend _countAccess  c1 c2)
        (on mappend _countPrimary c1 c2)
        (on mappend _countLambda  c1 c2)
        (on mappend _countBeta    c1 c2)

estimate :: Expression -> Cost
estimate e = execWriter (recmapped w e) where
    w e = e <$ case e of
        Access   _ -> tell (mempty { _countAccess  = Sum 1 })
        Primary  _ -> tell (mempty { _countPrimary = Sum 1 })
        Lambda _ _ -> tell (mempty { _countLambda  = Sum 1 })
        Beta   _ _ -> tell (mempty { _countBeta    = Sum 1 })

estimateSimple :: Expression -> Integer
estimateSimple e
    = let Cost{..} = estimate e in getSum
    $ mconcat [_countAccess, _countPrimary, _countLambda, _countBeta]

lesser :: Expression -> [Expression] -> Expression
lesser e es = minimumBy (comparing estimateSimple) (e:es)
