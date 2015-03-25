{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
module Kalium.Nucleus.Vector.Cost where

import Prelude
import Data.Void (absurd)
import Data.Ord  (comparing)
import Data.List (minimumBy)
import Control.Applicative
import Control.Monad.Writer.Strict

import GHC.Exts (Int(I#), Int#, (+#))

import Kalium.Nucleus.Vector.Program
import Kalium.Nucleus.Vector.Recmap

data Cost = Cost
    { _countAccess :: Int#
    , _countPrimary :: Int#
    , _countLambda :: Int#
    , _countBeta :: Int#
    } deriving (Eq)

instance Monoid Cost where
    mempty = Cost 0# 0# 0# 0#
    mappend (Cost a p l b) (Cost a' p' l' b')
        = Cost (a +# a') (p +# p') (l +# l') (b +# b')

estimate :: Expression -> Cost
estimate e = execWriter (recmapped w e) where
    w e = e <$ case e of
        Access   _ -> tell (mempty { _countAccess  = 1# })
        Primary  _ -> tell (mempty { _countPrimary = 1# })
        Lambda _ _ -> tell (mempty { _countLambda  = 1# })
        Beta   _ _ -> tell (mempty { _countBeta    = 1# })
        Ext ext -> absurd ext

estimateSimple :: Expression -> Int
estimateSimple e =
    let !Cost{..} = estimate e
    in I# (_countAccess +# _countPrimary +# _countLambda +# _countBeta)

lesser :: Expression -> [Expression] -> Expression
lesser e es = minimumBy (comparing estimateSimple) (e:es)
