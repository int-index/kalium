module Sodium.Util where

import Sodium.Prelude
import qualified Data.Map as M

type Pairs a b = [(a, b)]
type Endo' a = a -> a
type Kleisli' m a b = a -> m b
type EndoKleisli' m a = Kleisli' m a a

mAsList :: Ord k => Iso' (Map k a) (Pairs k a)
mAsList = iso M.toList M.fromList

composeMap :: (Ord k, Ord vk) => Map vk v -> Map k vk -> Map k v
composeMap m = M.foldrWithKey go mempty where
    go k vk = maybe id (M.insert k) (M.lookup vk m)

aliasUpdate :: Ord k => k -> k -> Endo' (Map k v)
aliasUpdate k1 k2 m = m & maybe id (M.insert k2) (M.lookup k1 m)

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)

closureM :: (Eq a, Monad m) => LensLike' m a a
closureM f = go where
    go x = f x >>= \y -> bool (go y) (return x) (x == y)

uniform :: Eq a => [a] -> Maybe a
uniform (x:xs) | all (==x) xs = Just x
uniform _ = Nothing

zipFilter :: [Bool] -> [a] -> Maybe [a]
zipFilter (keep:keeps) (a:as)
    | keep = (a:) `fmap` zipFilter keeps as
    | otherwise = zipFilter keeps as
zipFilter [] [] = Just []
zipFilter _ _ = Nothing
