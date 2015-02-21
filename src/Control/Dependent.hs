{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Control.Dependent where

import Sodium.Prelude
import qualified Data.Set as S
import qualified Data.Graph as G
import Data.Tree (flatten)

class Ord (Name k) => Dependent k where
    type Name k :: *
    provides :: k -> Name k
    depends  :: k -> Set (Name k)

sift :: Dependent k => [k] -> [k]
sift ks
    | all satisfied ks = ks
    | otherwise = sift (filter satisfied ks)
  where
    available = S.fromList (map provides ks)
    satisfied k = depends k `S.isSubsetOf` available

group :: Dependent k => [k] -> [[k]]
group (sift -> ks) = groups
  where
    triple k = (k, provides k, S.toList (depends k))
    (graph, lookupVertex, _) = G.graphFromEdges (map triple ks)
    groups = map (map (view _1 . lookupVertex) . flatten) (G.scc graph)
