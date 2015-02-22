{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Control.Dependent where

import Sodium.Prelude hiding (group)
import Sodium.Util (asfar)
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
group = widen G.scc

split :: Dependent k => [k] -> [[k]]
split = widen G.components

widen :: Dependent k => (G.Graph -> G.Forest G.Vertex) -> [k] -> [[k]]
widen onGraph ks = groups
  where
    triple k = (k, provides k, S.toList (depends k))
    (graph, lookupVertex, _) = G.graphFromEdges (map triple ks)
    groups = map (map (view _1 . lookupVertex) . flatten) (onGraph graph)

-- inner  level: circular   dependencies
-- middle level: sequential dependencies
-- outer  level: null       dependencies
structure :: Dependent k => [k] -> [[[k]]]
structure = fmap group . split . sift

destructure :: ([k] -> a -> Maybe a) -> [[[k]]] -> a -> a
destructure f ks = (appEndo . foldMap Endo) (asfar f <$> ks)

restructure :: Dependent k => ([k] -> a -> Maybe a) -> [k] -> a -> a
restructure f = destructure f . structure
